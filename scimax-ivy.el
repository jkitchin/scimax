;;; scimax-ivy.el --- ivy functions for scimax  -*- lexical-binding: t -*-

;;; Commentary:
;;
(require 'counsel)

;; * scimax customizations of ivy

(setq ivy-read-action-function 'ivy-hydra-read-action)

(defun scimax-ivy-default-action-continue (&optional arg)
  "Apply default action and move to previous candidate with ARG else next candidate."
  (interactive "P")
  (ivy-call)
  (if arg
      (ivy-previous-line)
    (ivy-next-line)))

;; s-RET to quit (super)
(defun scimax-ivy-exit-no-action ()
  "Exit with no action."
  (interactive)
  (ivy-exit-with-action
   (lambda (x) nil)))

;; default and continue
(define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-call)
;; default and next
(define-key ivy-minibuffer-map (kbd "S-<return>") #'scimax-ivy-default-action-continue)
;; alternate action and continue
(define-key ivy-minibuffer-map (kbd "M-<return>") #'ivy-dispatching-call)
;; quit no action
(define-key ivy-minibuffer-map (kbd "s-<return>") 'scimax-ivy-exit-no-action)
;; kill whole line
(define-key ivy-minibuffer-map (kbd "C-k") 'kill-whole-line)


;; This is an idea I got from embark. It solves a problem I have had a lot,
;; where I want to transfer the input text to another command. Why not use
;; embark? I find it confusing.

(defun scimax-ivy-become ()
  "Change the command and reuse the current input.
    You will be prompted to enter a new key sequence which can be a
    shortcut or M-x. Then it will put the current input in the
    minibuffer for the command.

    Applications:
    1. start with swiper, enter some text, C-M-b H-s to transfer the current input to swiper-all
    2. C-xC -b to switch-buffer, C-M-b C-x C-f to transfer input to find-file."
  (interactive)
  (let* ((input ivy-text)
         (transfer-input (lambda ()
			   (setf (buffer-substring
				  (point) (line-beginning-position))
				 "")
			   (insert input))))
    (ivy-exit-with-action
     (lambda (x)
       (minibuffer-with-setup-hook
	   (:append transfer-input)
	 (call-interactively
	  (key-binding
	   (read-key-sequence (format "Switch from %s to key sequence with this input (%s): "
				      (propertize (symbol-name
						   (ivy-state-caller ivy-last))
						  'face '(:foreground "DodgerBlue3"))
				      (propertize input
						  'face '(:foreground "red4"))))
	   t)))))))


(define-key ivy-minibuffer-map (kbd "s-b") 'scimax-ivy-become)
(define-key ivy-minibuffer-map (kbd "s-o") 'ivy-occur)


;; Show key bindings
(defun scimax-show-key-bindings ()
  "Show keys in the map"
  (interactive)
  (describe-keymap ivy-minibuffer-map))

(define-key ivy-minibuffer-map (kbd "C-h") #'scimax-show-key-bindings)

(defun scimax-ivy-show-marked-candidates ()
  "Show marked candidates"
  (interactive)
  (with-help-window "*ivy-marked-candidates*"
    (cl-loop for cand in ivy-marked-candidates
	     do
	     (princ (s-trim cand))
	     (princ "\n"))))

(define-key ivy-minibuffer-map (kbd "C-s") #'scimax-ivy-show-marked-candidates)

;; Marking candidates
(defun scimax-ivy-toggle-mark ()
  "Toggle the mark"
  (interactive)
  (if (ivy--marked-p)
      (ivy-unmark)
    (ivy-mark))
  (ivy-previous-line))

(define-key ivy-minibuffer-map (kbd "M-TAB")
  #'scimax-ivy-toggle-mark)


;; * alternate actions via completion
;;
;; [2021-10-29 Fri] it looks like I basically reinvented some stuff in ivy-hydra
;; here. I will leave this here in case it is useful one day.
;; 
;; I wrote this because some commands have so many alternate actions
;; they are not all visible, and in those cases using completion is
;; preferrable to scanning a lot of options.

;; (defun scimax-ivy-alternate ()
;;   "Run an alternate selection on the current candidate with completion.
;; In an ivy completion, this command provides all the alternate
;; actions via a new completion, and then runs it on the selected
;; candidate."
;;   (interactive)
;;   (let* ((funcs (cl-loop for (key func desc) in (cdr (ivy-state-action ivy-last))
;; 			 collect (cons (format "%3s - %s" (string-pad key 3 ? )
;; 					       desc)
;; 				       func))) 
;; 	 (action `(lambda (x)
;; 		    (let ((func (cdr (assoc (ivy-read "alternate action: "
;; 						      ',funcs :initial-input "^")
;; 					    ',funcs))))
;; 		      ;; (message "Running %S on %S" func x)
;; 		      (funcall func x)))))
;;     (ivy-exit-with-action action)))


;; (define-key ivy-minibuffer-map (kbd "C-M-<return>") 'scimax-ivy-alternate)

;; (defun scimax-ivy-alternate-continue ()
;;   (interactive)
;;   (setq ivy-current-prefix-arg current-prefix-arg)
;;   (let ((actions (copy-sequence (ivy-state-action ivy-last)))
;;         (old-ivy-text ivy-text))
;;     (unwind-protect
;;         (when (ivy-read-action)
;;           (ivy-set-text old-ivy-text)
;;           (ivy-call))
;;       (ivy-set-action actions)))
;;   (ivy-shrink-after-dispatching)


;;   (let* ((funcs (cl-loop for (key func desc) in (cdr (ivy-state-action ivy-last))
;; 			 collect (cons (format "%3s - %s" (string-pad key 3 ? )
;; 					       desc)
;; 				       func))) 
;; 	 (action `(lambda (x)
;; 		    (let ((func (cdr (assoc (ivy-read "alternate action: "
;; 						      ',funcs :initial-input "^")
;; 					    ',funcs))))
;; 		      ;; (message "Running %S on %S" func x)
;; 		      (funcall func x)))))
;;     (ivy-exit-with-action action)))


;; (define-key ivy-minibuffer-map (kbd "M-s-<return>") 'scimax-ivy-alternate-continue)

;; * Generic ivy actions

;; I usually want to be able to insert
(ivy-set-actions
 t
 '(("i" (lambda (x)
	  (with-ivy-window
	    (let (cand)
	      (setq cand (cond
			  ;; x is a string, the only sensible thing is to insert it
			  ((stringp x)
			   x)
			  ;; x is a list where the first element is a string
			  ((and (listp x) (stringp (first x)))
			   (first x))
			  (t
			   (format "%S" x)))) 
	      (insert cand))))
    "insert candidate")
   ("?" (lambda (x)
	  (interactive)
	  (describe-keymap ivy-minibuffer-map))
    "Describe keys")))

;; ** Extra projectile actions
;; Here I can open bash or finder when switching projects

(defun scimax-ivy-projectile-bash (x)
  "Open bash at X chosen from `projectile-completing-read'."
  (let* ((full-path (f-join (projectile-project-root) x))
	 (dir (if (file-directory-p full-path)
		  full-path
		(file-name-directory full-path))))
    (bash dir)
    ;; I use this to just get out of whatever called this to avoid visiting a
    ;; file for example.
    (recursive-edit)
    (ivy-quit-and-run)))


(defun scimax-ivy-projectile-finder (x)
  "Open finder at X chosen from `projectile-completing-read'."
  (let* ((full-path (f-join (projectile-project-root) x))
	 (dir (if (file-directory-p full-path)
		  full-path
		(file-name-directory full-path))))
    (finder dir)
    ;; I use this to just get out of whatever called this to avoid visiting a
    ;; file for example.
    (recursive-edit)
    (ivy-quit-and-run)))


(defun scimax-ivy-insert-project-link (x)
  "Insert a relative path link to X chosen from `projectile-completing-read'."
  (let* ((full-path (f-join (projectile-project-root) x))
	 (current-path (file-name-directory (buffer-file-name)))
	 (rel-path (file-relative-name full-path current-path)))
    (insert (format "[[%s]]" rel-path)))
  ;; I use this to just get out of whatever called this to avoid visiting a
  ;; file for example.
  (recursive-edit)
  (ivy-quit-and-run))


(defun scimax-ivy-magit-status (x)
  "Run magit status from `projectile-completing-read'.
Right now, it runs `magit-status' in the directory associated
with the entry."
  (cond
   ;; A directory, we can just get the status
   ((file-directory-p x)
    (let ((default-directory x))
      (magit-status-setup-buffer)))
   ;; something else?
   (t
    ;; What should we do on a file? show that file change? just do magit status?
    (let* ((full-path (f-join (projectile-project-root) x))
	   (dir (if (file-directory-p full-path)
		    full-path
		  (file-name-directory full-path))))

      (let ((default-directory dir))
	(magit-status-setup-buffer)))
    (recursive-edit)
    (ivy-quit-and-run))))


;; See `counsel-projectile-switch-project-action-ag'
;; (defun scimax-ivy-projectile-ag (x)
;;   "Run projectile-ag in the selected project X."
;;   (let ((default-directory x))
;;     (call-interactively #'projectile-ag)))

;; See `counsel-projectile-switch-project-action-rg'
;; (defun scimax-ivy-projectile-ripgrep (x)
;;   "Run projectile-ag in the selected project X."
;;   (let ((default-directory x))
;;     (call-interactively #'projectile-ripgrep)))


(defun scimax-ivy-projectile-org-heading (x)
  "Open a heading in the project X"
  (let ((default-directory x))
    (call-interactively #'ivy-org-jump-to-project-headline)))

;; See [[nb:scimax::elpa/counsel-projectile-20201015.1109/counsel-projectile.el::c53333]]
;; for a long list of actions in counsel-projectile
(cl-loop for projectile-cmd in '(projectile-completing-read
				 counsel-projectile-switch-project)
	 do
	 (ivy-add-actions
	  projectile-cmd 
	  '(
	    ;; xs runs shell, and xe runs eshell. This is nice for an external shell.
	    ("xb" scimax-ivy-projectile-bash "Open bash here.")
	    ("xf" scimax-ivy-projectile-finder  "Open Finder here.")

	    ;; This may not be useful, there is already v for vcs
	    ("xg" scimax-ivy-magit-status  "Magit status")
	    ("h" scimax-ivy-projectile-org-heading "Open project heading")
	    ("l" scimax-ivy-insert-project-link "Insert project link")
	    ("4" (lambda (arg)
		   (find-file-other-window arg))
	     "open in other window")
	    ("5" (lambda (arg)
		   (find-file-other-frame arg))
	     "open in new frame")
	    ("6" (lambda (arg)
		   (find-file-other-tab arg))
	     "open in new tab")))) 


(ivy-add-actions 'counsel-projectile-switch-project
		 '(("l" (lambda (x)
			  (insert (format "[[%s]]" x)))
		    "Insert link to project")))


(defun scimax-projectile-switch-project-transformer (project)
  "Add title from readme.org in file if it exists."
  (let ((title (when (file-exists-p (f-join project "readme.org"))
		 (with-temp-buffer
		   (insert-file-contents (f-join project "readme.org"))
		   (when (re-search-forward "#\\+title:\\(.*\\)" nil t)
		     (propertize (match-string 1)
				 'face '(:foreground "DodgerBlue1")))))))
    (format "%60s%s" (s-pad-right 60 " " project) (or title ""))))


(ivy-configure 'counsel-projectile-switch-project :display-transformer-fn
	       #'scimax-projectile-switch-project-transformer)
(ivy-configure 'projectile-switch-project :display-transformer-fn
	       #'scimax-projectile-switch-project-transformer)


;; ** Find file actions
;; I like to do a lot of things from find-file.

;; I replace the x action with a x as a prefix here.
(let ((p (plist-get ivy--actions-list 'counsel-find-file )))
  (setq p (remove '("x" counsel-find-file-extern "open externally") p))
  (push '("xt" counsel-find-file-extern "open externally") p)
  (plist-put ivy--actions-list 'counsel-find-file p))


(ivy-add-actions
 'counsel-find-file
 '(("a" (lambda (x)
	  (unless (memq major-mode '(mu4e-compose-mode message-mode))
	    (compose-mail))
	  (mml-attach-file x))
    "Attach to email")
   ("c" (lambda (x) (kill-new (f-relative x))) "Copy relative path")
   ("4" (lambda (x) (find-file-other-window x)) "Open in new window")
   ("5" (lambda (x) (find-file-other-frame x)) "Open in new frame")
   ("C" (lambda (x) (kill-new x)) "Copy absolute path")
   ("d" (lambda (x) (dired x)) "Open in dired")
   ("D" (lambda (x) (delete-file x)) "Delete file")
   ("e" (lambda (x) (shell-command (format "open %s" x)))
    "Open in external program")
   ("f" (lambda (x)
	  "Open X in another frame."
	  (find-file-other-frame x))
    "Open in new frame")

   ("p" (lambda (path)
	  (with-ivy-window
	    (insert (f-relative path))))
    "Insert relative path")
   ("P" (lambda (path)
	  (with-ivy-window
	    (insert path)))
    "Insert absolute path")
   ("l" (lambda (path)
	  "Insert org-link with relative path"
	  (with-ivy-window
	    (insert (format "[[./%s]]" (f-relative path)))
	    (org-toggle-inline-images)
	    (org-toggle-inline-images)))
    "Insert org-link (rel. path)")
   ("L" (lambda (path)
	  "Insert org-link with absolute path"
	  (with-ivy-window
	    (insert (format "[[%s]]" path))
	    (org-toggle-inline-images)
	    (org-toggle-inline-images)))
    "Insert org-link (abs. path)")
   ("r" (lambda (path)
	  (rename-file path (read-string "New name: ")))
    "Rename")
   ("s" (lambda (path)
	  (find-file path)
	  (swiper))
    "Swiper in file")
   
   ("xb" (lambda (path)
	   (bash (file-name-directory path)))
    "Open in bash")
   
   ("xe" (lambda (path)
	   (let ((default-directory (file-name-directory
				     (expand-file-name path))))
	     (eshell)))
    "Open in eshell")
   
   ("xf" (lambda (path)
	   (finder (file-name-directory path)))
    "Open in finder/explorer")
   
   ("xh" (lambda (path)
	   (find-file path)
	   (ivy-org-jump-to-heading))
    "Jump to org heading in file")))


;; * ivy colors

(defun ivy-color-candidates ()
  "Get a list of candidates for `ivy-colors'."
  (save-selected-window
    (list-colors-display))
  (with-current-buffer (get-buffer "*Colors*")
    (prog1
	(cl-loop for line in (s-split "\n" (buffer-string))
		 collect
		 (append (list line)
			 (mapcar 's-trim
				 (mapcar 'substring (s-split "  " line t)))))
      (kill-buffer "*Colors*"))))


(defun ivy-colors ()
  "List colors in ivy."
  (interactive)
  (ivy-read "Color: " (ivy-color-candidates)
	    :action
	    '(1
	      ("i" (lambda (line)
		     (insert (second line)))
	       "Insert name")
	      ("c" (lambda (line)
		     (kill-new (second line)))
	       "Copy name")
	      ("h" (lambda (line)
		     (insert (car (last line))))
	       "Insert hex")
	      ("r" (lambda (line)
		     (insert (format "%s" (color-name-to-rgb (second line)))))
	       "Insert RGB")

	      ("m" (lambda (line) (message "%s" (cdr line)))))))

;; * ivy-top

(defcustom ivy-top-command
  "top -stats pid,command,user,cpu,mem,pstate,time -l 1"
  "Top command for `ivy-top'."
  :group 'scimax-ivy
  :type 'string)

(defun ivy-top ()
  (interactive)
  (let* ((output (shell-command-to-string ivy-top-command))
	 (lines (progn
		  (string-match "TIME" output)
		  (split-string (substring output (+ 1 (match-end 0))) "\n")))
	 (candidates (mapcar (lambda (line)
			       (list line (split-string line " " t)))
			     lines)))
    (ivy-read "process: " candidates)))


;; * ivy-ps


;; a data structure for a process
(cl-defstruct ivy-ps user pid)


(defun ivy-ps ()
  "WIP: ivy selector for ps.
TODO: sorting, actions."
  (interactive)
  (let* ((output (shell-command-to-string "ps aux | sort -k 3 -r"))
	 (lines (split-string output "\n"))
	 (candidates (mapcar
		      (lambda (line)
			(cons line
			      (let ((f (split-string line " " t)))
				(make-ivy-ps :user (elt f 0) :pid (elt f 1)))))
		      lines)))
    (ivy-read "process: " candidates
	      :action
	      '(1
		("k" (lambda (cand) (message "%s" (ivy-ps-pid cand))) "kill")))))

(provide 'scimax-ivy)

;;; scimax-ivy.el ends here
