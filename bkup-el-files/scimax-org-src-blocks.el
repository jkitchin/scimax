;;; scimax-org-src-blocks.el --- Scimax customizations for src blocks.

;;; Commentary:
;;
;; This library fixes the fontification of <> in src-blocks so that you can use
;; them as operators.
;;
;;

;; * Babel settings
;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-link-shell-confirm-function nil)

;; register languages in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (shell . t)
   (matlab . t)
   (sqlite . t)
   (ruby . t)
   (perl . t)
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)
   (fortran . t)
   (C . t)))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)


;; ** Jupyter see [[./scimax-jupyter.el]]


;; ** Fortran
(defalias 'org-babel-execute:f90 'org-babel-execute:fortran)




;; * Colored src blocks

(setq  org-src-block-faces '(("emacs-lisp" (:background "LightCyan1" :extend t))
			     ("sh" (:background "gray90" :extend t))
			     ("python" (:background "DarkSeaGreen1" :extend t))
			     ("ipython" (:background "thistle1" :extend t))
			     ("jupyter-python" (:background "thistle1" :extend t))))


;; * Fixing <> fontlock in src blocks
;;
;; this was broken so that if you had < or > in a src block it would break
;; parens matching.

;; https://emacs.stackexchange.com/questions/50216/org-mode-code-block-parentheses-mismatch

(defun scimax-org-mode-<>-syntax-fix (start end)
  "Change syntax of characters <>, {} and [] within source code blocks.
It changes to symbol unless in a string, then it stays the same.

This fixes an issue in src-blocks where <>, {}, [] are considered
open/close brackets. That causes some confusion if you use <, >
in comparison operators, because they never close. This function
makes these characters regular symbols, except in strings, where
they are still open/close brackets.

I don't understand this completely, it seems like this function
is called twice, once in the src-block mode, and once in
org-mode. That is why there is the conditional branch here, that
seems to do the same thing. Without it though, you don't get the
behavior described above.
"
  (when (eq 'org-mode major-mode)
    (goto-char start)
    (while (re-search-forward "<\\|>" end 'mv)
      (cond
       ((member (car (org-babel-get-src-block-info 'light)) '("jupyter-python" "python" "emacs-lisp"))
	(unless (ppss-string-terminator (syntax-ppss (point)))
	  (put-text-property (point) (- (point) 1)
			     'syntax-table (string-to-syntax "_"))))))))

  (defun scimax-fix-<>-syntax ()
    "Fix syntax of <> in code blocks.
This function should be added to `org-mode-hook' to make it work."
    (setq syntax-propertize-function 'scimax-org-mode-<>-syntax-fix)
    (syntax-propertize (point-max)))

(add-hook 'org-mode-hook
	  #'scimax-fix-<>-syntax)


;; * Language mode keymaps on src blocks

;; The idea here is to get more src native editing in src blocks by combining
;; their keymaps with org-mode.
;; [2020-12-27 Sun] I am not sure this actually works right.
;; [2021-09-05 Sun] It kind of works

(defcustom scimax-src-block-python-edit-mode-map
  nil
  "Keymap used in editing Python blocks.
Some examples include `elpy-mode-map', or `anaconda-mode-map'.
This keymap is combined with some other keymaps in
`scimax-src-block-keymaps' to enable native edit commands in
them."
  :group :scimax)


(defcustom scimax-src-block-keymaps
  `(("ipython" . ,(let ((map (copy-keymap (make-composed-keymap
					   `(,scimax-src-block-python-edit-mode-map
					     ,(when (boundp 'python-mode-map) python-mode-map)
					     ,(when (boundp 'pyvenv-mode-map) pyvenv-mode-map) )
					   org-mode-map))))
		    ;; In org-mode I define RET so we f
		    (define-key map (kbd "<return>") 'newline)
		    (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
		    map))
    ("python" . ,(let ((map (copy-keymap (make-composed-keymap
					  `(,scimax-src-block-python-edit-mode-map
					    ,(when (boundp 'python-mode-map) python-mode-map)
					    ,(when (boundp 'pyvenv-mode-map) pyvenv-mode-map))
					  org-mode-map))))
		   ;; In org-mode I define RET so we f
		   (define-key map (kbd "<return>") 'newline)
		   (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
		   map))
    ("emacs-lisp" . ,(let ((map (copy-keymap (make-composed-keymap
					      `(,lispy-mode-map
						,emacs-lisp-mode-map
						,outline-minor-mode-map)
					      org-mode-map))))
		       (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
		       map))
    ("sh" . ,(let ((map (copy-keymap (make-composed-keymap
				      `(,(when (boundp 'shell-mode-map) shell-mode-map))
				      org-mode-map))))
	       (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
	       map)))
  "alist of custom keymaps for src blocks."
  :group :scimax)


(defun scimax-src-block-add-keymap (limit)
  "Add keymaps to src-blocks defined in `scimax-src-block-keymaps'.

This is run by font-lock in `scimax-src-keymap-mode'."
  (let ((case-fold-search t)
	lang)
    (while (re-search-forward org-babel-src-block-regexp limit t)
      (let ((lang (match-string 2))
	    (beg (match-beginning 0))
	    (end (match-end 0)))
	(if (assoc (org-no-properties lang) scimax-src-block-keymaps)
	    (progn
	      (add-text-properties
	       beg end `(local-map ,(cdr (assoc
					  (org-no-properties lang)
					  scimax-src-block-keymaps))))
	      (add-text-properties
	       beg end `(cursor-sensor-functions
			 ((lambda (win prev-pos sym)
			    ;; This simulates a mouse click and makes a menu change
			    (org-mouse-down-mouse nil)))))))))))


(defun scimax-src-block-spoof-mode (orig-func &rest args)
  "Advice function to spoof commands in org-mode src blocks.
It is for commands that depend on the major mode. One example is
`lispy--eval'. This simply let-binds the major-mode while running the command."
  (if (org-in-src-block-p)
      (let ((major-mode (intern (format "%s-mode" (first (org-babel-get-src-block-info))))))
	(apply orig-func args))
    (apply orig-func args)))


(defcustom scimax-src-block-spoof-commands '(lispy--eval)
  "List of commands to advise with `scimax-spoof-mode'.
`lispy--eval' is the only one I know of, but in case there are others, you can add them here."
  :group 'scimax)


(define-minor-mode scimax-src-block-src-keymap-mode
  "Minor mode to add mode keymaps to src-blocks."
  :init-value nil
  (if scimax-src-block-src-keymap-mode
      (progn
	(add-hook 'org-font-lock-hook #'scimax-src-block-add-keymap t)
	(add-to-list 'font-lock-extra-managed-props 'local-map)
	(add-to-list 'font-lock-extra-managed-props 'keymap)
	(add-to-list 'font-lock-extra-managed-props 'cursor-sensor-functions)
	(cl-loop for cmd in scimax-src-block-spoof-commands do
		 (advice-add cmd :around 'scimax-src-block-spoof-mode))

	(cursor-sensor-mode +1)
	(message "scimax-src-block-src-keymap-mode enabled"))
    ;; Leaving the mode
    (remove-hook 'org-font-lock-hook #'scimax-src-block-add-keymap)
    (cl-loop for cmd in scimax-src-block-spoof-commands do
	     (advice-remove cmd 'scimax-src-block-spoof-mode))
    (cursor-sensor-mode -1))
  (font-lock-ensure))

;; (add-hook 'org-mode-hook (lambda ()
;; 			   (scimax-src-block-src-keymap-mode +1)))



(provide 'scimax-org-src-blocks)

;;; scimax-org-src-blocks.el ends here
