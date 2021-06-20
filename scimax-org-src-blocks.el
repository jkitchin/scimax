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
  "Change syntax of characters ?< and ?> to symbol within source code blocks."
  (let ((case-fold-search t))
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
	;; this "fixes" <>, {} and [] that fixes some issues in src blocks, but
	;; makes some new issues, which is now you cannot use them as brackets.
	;; I guess a fancier solution is needed for when these chars are in a
	;; character or string, and should not be considered part of a bracket.
        (while (re-search-forward "[[<{]\\|[]>}]" end t)
          (when (save-excursion
                  (and
                   (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                   (string-equal (match-string 1) "begin")))
	    ;; removes open/close syntax
            (put-text-property (point) (1- (point))
                               'syntax-table (string-to-syntax "_"))))))))


(defun scimax-fix-<>-syntax ()
  "Fix syntax of <> in code blocks.
This function should be added to `org-mode-hook' to make it work."
  (setq syntax-propertize-function 'scimax-org-mode-<>-syntax-fix)
  (syntax-propertize (point-max)))

(add-hook 'org-mode-hook
	  #'scimax-fix-<>-syntax)

(provide 'scimax-org-src-blocks)

;;; scimax-org-src-blocks.el ends here
