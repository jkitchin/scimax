;;; scimax-projectile.el --- Scimax projectile customizations

;;; Commentary:
;;
;; I find these convenient to have around.
(require 'projectile)


;; * Extra projectile actions
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


;; These functions already exist in counsel. Leaving them here so i don't forget
;; later.
;;
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
	     "open file in other window")
	    ("5" (lambda (arg)
		   (find-file-other-frame arg))
	     "open file in new frame") 
	    ("6" (lambda (arg)
		   (find-file-other-tab arg))
	     "open in new tab")))) 


;; (ivy-add-actions 'counsel-projectile-switch-project
;; 		 '(("l" (lambda (x)
;; 			  (insert (format "[[%s]]" x)))
;; 		    "Insert link to project")))


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


;; * advices to remove ignore files, and prioritize default files
;;
;; I almost always want to open a default readme in a project, and I never want
;; to see files like .projectile. This section modifies the file list projectile
;; uses, removing anything in projectile-globally-ignored-files (maybe i should
;; just move them to the end?), and for any files that match the files in
;; `scimax-project-default-files' we add them to the top for selection.

(push ".projectile" projectile-globally-ignored-files)


(defcustom scimax-project-default-files
  '("readme.org"
    "README.org")
  "List of default files to prioritize in find-file."
  :group 'scimax-projectile
  :type '(list string))


(defun scimax-current-project-files (orig-func &rest args)
  "Advice function for `projectile-project-files'.
ORIG-FUNC is `projectile-project-files'. ARGS are provided.
Filters out files that match entries in
`projectile-globally-ignored-files', and moves files that match
entries in `scimax-project-default-files' to the top."
  (let ((files (apply orig-func args))
	(defaults))
    
    ;; Remove any files from projectile-globally-ignored-files 
    (setq files (cl-delete-if (lambda (f)
				(member (file-name-nondirectory f)
					projectile-globally-ignored-files))
			      files))
    ;; if there is a default file, put it first.
    (cl-loop for f in files do
	     (when (member (file-name-nondirectory f) scimax-project-default-files)
	       (push f defaults)))
    ;; return defaults first
    (-uniq (append defaults files))))


(advice-add #'projectile-project-files :around 'scimax-current-project-files)

;; * rename projects
;;
;; I sometimes want to rename projects, and this is a function to do that so
;; they do not get lost, and so old projects do not remain.

(defun scimax-projectile-mv-project (newpath)
  "Rename current project to NEWPRJ.
This updates the cache, and the project list for the new project."
  (interactive (list
		(read-directory-name "New Project: ")))

  (let ((oldprj (projectile-project-root))
	(newprj (f-join newpath (car (last (f-split (projectile-project-root))))))
	(projectile-switch-project-action (lambda (&optional args) nil))) 
    (projectile-invalidate-cache nil)
    (projectile-kill-buffers)

    
    (rename-file oldprj newprj)
    (projectile-remove-known-project oldprj)
    
    (projectile-add-known-project newprj)
    (projectile-switch-project-by-name newprj)
    (projectile-cache-project newprj (projectile-project-files newprj))))



(provide 'scimax-projectile)

;;; scimax-projectile.el ends here
