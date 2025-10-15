;;; scimax-jupyter.el --- scimax customization for emacs-jupyter -*- lexical-binding: t -*-

;;; Commentary:
;; This provides some fine-tuning of key bindings, and loads jupyter for scimax.

(require 'scimax-ob)
(require 'jupyter)


;; * scimax jupyter header defaults

(add-to-list 'org-babel-load-languages '(jupyter . t) t)

(setq org-babel-default-header-args:jupyter-python
      '((:results . "both")
	;; This seems to lead to buffer specific sessions!
	(:session . (lambda () (buffer-file-name)))
	(:kernel . "python3")
	(:pandoc . "t")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))


;; ** Julia
;; (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
;;                                                     (:session . "jupyter-julia")
;;                                                     (:kernel . "julia-1.6")
;; 						    (:exports . "both")
;; 						    (:eval . "never-export")))


;; https://datatofish.com/add-julia-to-jupyter/
;;  for setup
;; (add-to-list 'org-babel-load-languages '(julia . t) t)
;; (setq inferior-julia-program-name "/usr/local/bin/julia")

;; ** R setup

;; https://developers.refinitiv.com/en/article-catalog/article/setup-jupyter-notebook-r
;; (setq org-babel-default-header-args:jupyter-R
;;       '((:results . "value")
;; 	(:session . "jupyter-R")
;; 	(:kernel . "ir")
;; 	(:pandoc . "t")
;; 	(:exports . "both")
;; 	(:cache .   "no")
;; 	(:noweb . "no")
;; 	(:hlines . "no")
;; 	(:tangle . "no")
;; 	(:eval . "never-export")))

;; (require 'jupyter-R)

;; ** do the languages setup

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; * syntax highlighting
(add-to-list 'org-src-lang-modes '("jupyter-python" . python))
;; (add-to-list 'org-src-lang-modes '("jupyter-julia" . julia))
;; (add-to-list 'org-src-lang-modes '("jupyter-R" . R))

;; * make old ipython blocks work with jupyter python

(defalias 'org-babel-execute:ipython 'org-babel-execute:jupyter-python)
(setq org-babel-default-header-args:ipython org-babel-default-header-args:jupyter-python)
(add-to-list 'org-src-lang-modes '("ipython" . python))


;; * Kill the kernel when the buffer is closed
;; This is also harder than needed I think. This should kill the kernel in the current file.
(defun scimax-jupyter-get-session ()
  "Get the session name in the current buffer."
  (let ((lang (car (org-babel-get-src-block-info))))
    (let ((session (org-babel-read
		    (or
		     (cdr
		      (assoc :session
			     (cadr (org-babel-params-from-properties lang))))
		     (cdr (assoc :session
				 org-babel-default-header-args:jupyter-python))))))
      (cond
       ((functionp session)
	(funcall session))
       (t
	session)))))


(defun scimax-jupyter-org-kill-kernel ()
  "Kill the kernel."
  (interactive)
  (let ((session (scimax-jupyter-get-session)))
    (catch 'done
      (cl-loop for buffer in (buffer-list)
	       do
	       (when (string-match
		      (format "*jupyter-repl\\[.*\\]-%s*" session)
		      (buffer-name buffer))
		 (kill-buffer buffer)
		 (setq header-line-format nil)
		 (throw 'done buffer))))))


(defun scimax-jupyter-kill-kernel-hook (&rest args)
  (setq header-line-format
	(format "%s running. Click here to kill it."
		(scimax-jupyter-get-session)))
  (local-set-key [header-line down-mouse-1]
		 `(lambda ()
		    (interactive)
		    (scimax-jupyter-org-kill-kernel)))
  (add-hook 'kill-buffer-hook 'scimax-jupyter-org-kill-kernel nil t))


(defun scimax-jupyter-check-restart (&rest args)
  "If :restart is in the header, kill the kernel first."
  (let ((src (org-element-context)))
    (when (string-match ":restart" (or (org-element-property :parameters src) ""))
      (scimax-jupyter-org-kill-kernel))))


(defcustom scimax-jupyter-advices
  '((org-babel-execute:jupyter :before scimax-jupyter-kill-kernel-hook)
    (org-babel-execute:jupyter :before scimax-jupyter-check-restart)
    ;; (jupyter-org-sync-results :override scimax-jupyter-org-sync-results)
    ;; (jupyter-org--add-result :override scimax-jupyter-org--add-result)
    ;; (jupyter-org-export-block-or-pandoc :override scimax-jupyter-org-export-block-or-pandoc)
    )
  "Advices for scimax-jupyter.
This is a list of (emacs-jupyter-fn :position scimax-jupyter-fn)"
  :group 'scimax-jupyter)


(defun scimax-jupyter-advise ()
  "Turn scimax-jupyter advices on."
  (interactive)
  (cl-loop for (emacs-jupyter-fn position scimax-jupyter-fn) in scimax-jupyter-advices
	   do
	   (advice-add emacs-jupyter-fn position scimax-jupyter-fn)))


(defun scimax-jupyter-unadvise ()
  "Turn scimax-jupyter advices off."
  (interactive)
  (message "Un-advising emacs-jupyter")

  (cl-loop for (emacs-jupyter-fn position scimax-jupyter-fn) in scimax-jupyter-advices
	   do
	   (advice-remove emacs-jupyter-fn scimax-jupyter-fn)))


;; Turn on by default
(scimax-jupyter-advise)




;; I don't know where the \\ lines come from, this removes them.
(defun scimax-rm-backslashes ()
  "rm \\ from the end of lines."
  (let ((result-start (org-babel-where-is-src-block-result)))
    (save-excursion
      (when (and result-start
		 (goto-char result-start)
		 (looking-at org-babel-result-regexp))
	(while (re-search-forward "\\\\\\\\" (org-babel-result-end) t)
	  (replace-match "")
	  ;; I don't know why this is needed, but without, it was not finding everything.
	  (goto-char result-start))))))

(add-hook 'org-babel-after-execute-hook 'scimax-rm-backslashes)


;; * Working with exceptions
;;
;; The syntax highlighting for exceptions seems broken. `jupyter-org-client' has
;; a bunch of code that suggests this should be working, even referencing my
;; blog post about it. This is ok for now I guess. As I recall the syntax
;; highlighting is not persistent if you don't put the ansi codes in the buffer.
;;
;; this handled in generic functions that I find too difficult to trace for now.

(defun scimax-jupyter-jump-to-error ()
  "In a src block, jump to the line indicated as an error in the results.
In a SyntaxError, there is not a traceback with a line number, so
we handle it separately. It doesn't seem like it should be that
way, but it is."
  (interactive)
  (let* ((cp (point))
	 (location (org-babel-where-is-src-block-result))
	 (case-fold-search t))

    (when (and location
	       (goto-char location)
	       (looking-at org-babel-result-regexp))
      (cond
       ;; Check for SyntaxError
       ((string-match "SyntaxError:" (buffer-substring location (org-babel-result-end)))
	(re-search-forward (rx (zero-or-more " ") "^") nil (org-babel-result-end))
	(previous-line)
	(let ((pattern (string-trim-left
			(buffer-substring-no-properties
			 (line-beginning-position) (line-end-position)))))
	  (goto-char cp)
	  (goto-char (org-element-property :begin (org-element-context)))
	  (unless
	      (search-forward pattern (org-element-property :end (org-element-context)) t)
	    (message "No SyntaxError found like %s" pattern))))

       ;; search for something like --> 21
       (t
	(goto-char location)
	(re-search-forward "-*> \\([[:digit:]]*\\)" (org-babel-result-end))
	(save-match-data
	  (goto-char cp)
	  (goto-char (org-element-property :begin (org-element-context))))
	(forward-line (string-to-number (match-string-no-properties 1))))))))


;; * Handling ansi codes

(defun scimax-jupyter-ansi ()
  "Replaces ansi-codes in exceptions with colored text.
I thought emacs-jupyter did this automatically, but it may only
happen in the REPL. Without this, the tracebacks are very long
and basically unreadable.

We also add some font properties to click on goto-error.

This should only apply to jupyter-lang blocks."
  (when (string-match "^jupyter" (car (or (org-babel-get-src-block-info t) '(""))))
    (let* ((r (org-babel-where-is-src-block-result))
	   (result (when r
		     (save-excursion
		       (goto-char r)
		       (org-element-context)))))
      (when result
	(ansi-color-apply-on-region (org-element-property :begin result)
				    (org-element-property :end result))

	;; Let's fontify "# [goto error]" to it is clickable
	(save-excursion
	  (goto-char r)
	  (when (search-forward "# [goto error]" (org-element-property :end result) t)
	    (add-text-properties
	     (match-beginning 0) (match-end 0)
	     (list 'help-echo "Click to jump to error."
		   'mouse-face 'highlight
		   'local-map (let ((map (copy-keymap help-mode-map)))
				(define-key map [mouse-1] (lambda ()
							    (interactive)
							    (search-backward "#+BEGIN_SRC")
							    (scimax-jupyter-jump-to-error)))
				map))))))

      t)))


(add-to-list 'org-babel-after-execute-hook 'scimax-jupyter-ansi t)

;; * The scimax jupyter hydra
;; customization of what is in jupyter
;; These are more aligned with jupyter notebook I think

;; I had to use pretty hydra to get nicely aligned columns here.
(use-package pretty-hydra)

(pretty-hydra-define scimax-jupyter-org-hydra (:color blue :hint nil)
  ("Execute"
   (("<return>" org-ctrl-c-ctrl-c "current" :color red)
    ("C-<return>" org-ctrl-c-ctrl-c "current" :color red)
    ("S-<return>" jupyter-org-execute-and-next-block "current and next" :color red)
    ("M-<return>" (progn (org-ctrl-c-ctrl-c) (scimax-ob-insert-src-block t)) "current and new" )
    ("C-M-<return>" jupyter-org-execute-subtree "subtree")
    ("S-C-<return>" jupyter-org-restart-and-execute-to-point "restart to point")
    ("S-M-<return>" jupyter-org-restart-kernel-execute-buffer "restart buffer"))

   "Navigate"
   (("p" org-babel-previous-src-block "previous" :color red)
    ("P" jupyter-org-previous-busy-src-block "previous busy")
    ("n" org-babel-next-src-block  "next" :color red)
    ("N" jupyter-org-next-busy-src-block "next busy" :color red)
    ("g" jupyter-org-jump-to-visible-block "jump to visible src")
    ("G" jupyter-org-jump-to-block "jump to src block")
    ("e" scimax-jupyter-jump-to-error "Jump to error"))

   "Edit"
   (("<up>" jupyter-org-move-src-block "move up" :color red)
    ("<down>" (jupyter-org-move-src-block t) "move down" :color red)
    ("x" jupyter-org-kill-block-and-results "kill block")
    ("c" jupyter-org-copy-block-and-results "copy block")
    ("o" (jupyter-org-clone-block t) "clone")
    ("m" jupyter-org-merge-blocks "merge")
    ("s" jupyter-org-split-src-block "split")
    ("a" (jupyter-org-insert-src-block nil current-prefix-arg) "insert above")
    ("b" (jupyter-org-insert-src-block t current-prefix-arg) "insert below")
    ("l" org-babel-remove-result "clear result")
    ("L" jupyter-org-clear-all-results "clear all results")
    ("h" jupyter-org-edit-header "edit header"))

   "Misc"
   (("i" jupyter-org-inspect-src-block "inspect")
    ("<tab>" completion-at-point "Complete")

    ("O" scimax-ob/body "scimax-ob")
    ("q" nil "quit"))

   "Kernel"
   (("R" org-babel-jupyter-scratch-buffer "scratch")
    ("z" org-babel-switch-to-session "REPL")
    ("u" jupyter-org-interrupt-kernel "interrupt")
    ("r" (jupyter-org-with-src-block-client
	  (jupyter-repl-restart-kernel)) "restart")
    ("k" scimax-jupyter-org-kill-kernel "kill"))))


(jupyter-org-define-key (kbd "<f12>") #'scimax-jupyter-org-hydra/body)


(provide 'scimax-jupyter)

;;; scimax-jupyter.el ends here
