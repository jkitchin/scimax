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
they are still open/close brackets."
  ;; I think this gets run in a special edit buffer for src-blocks. For now I
  ;; only run this in the src blocks, so that outside the src-blocks these still
  ;; act like b=open/close brackets.
  (when (org-src-edit-buffer-p)
    (let ((case-fold-search t))
      (goto-char start)
      ;; this "fixes" <>, {} and [] that fixes some issues in src blocks, but
      ;; makes some new issues, which is now you cannot use them as brackets.
      ;; this tries to be fancy and not change the syntax in strings.
      (while (re-search-forward "[[<{]\\|[]>}]" end t)
	(unless (ppss-string-terminator (syntax-ppss (point)))
	  (put-text-property (point) (1- (point))
                             'syntax-table (string-to-syntax "_")))))))


(defun scimax-fix-<>-syntax ()
  "Fix syntax of <> in code blocks.
This function should be added to `org-mode-hook' to make it work."
  (setq syntax-propertize-function 'scimax-org-mode-<>-syntax-fix)
  (syntax-propertize (point-max)))

(add-hook 'org-mode-hook
	  #'scimax-fix-<>-syntax)

(provide 'scimax-org-src-blocks)

;;; scimax-org-src-blocks.el ends here
