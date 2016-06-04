;;; scimax.el ---

;;; Commentary:
;; 

(load-theme 'leuven)

(setq inhibit-startup-screen t) ;; stop showing startup screen
(tool-bar-mode 0)           ; remove the icons
(menu-bar-mode 1)           ; keep the menus
(global-visual-line-mode 1) ;; how long lines are handled.  This
                            ;; appears to wrap long lines visually,
                            ;; but not add line-returns

(global-font-lock-mode t)   ;; turn on font-lock mode everywhere

;; I do not like autofill mode.
(auto-fill-mode -1)

(show-paren-mode 1)         ;; highlight parentheses
(setq show-paren-style 'mixed) ;; alternative is 'expression,
			       ;; 'parenthesis or 'mixed

(setq backup-inhibited t)  ;; disable backup file creation

(fset 'yes-or-no-p 'y-or-n-p) ; answer with y/n instead of yes/no

;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

;; * Programming
;; ** Emacs lisp
"^;; \\(\\*+.*\\)$"
(defconst lel-font-lock-keywords
  '(("^;; \\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^;; \\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^;; \\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^;; \\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^;; \\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    ;; (lel-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'emacs-lisp-mode lel-font-lock-keywords)

(defun lisp-outline-setup ()
  (setq-local outline-regexp ";; ?\\*+\\|\\`")
  (setq-local orgstruct-heading-prefix-regexp ";; ?\\*+\\|\\`")
  (orgstruct-mode)
  (org-global-cycle 3))

(add-hook 'emacs-lisp-mode-hook
	  #'lisp-outline-setup)

;; ** Python
(defconst lpy-font-lock-keywords
  '(("^# \\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^# \\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^# \\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^# \\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^# \\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    (lpy-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'python-mode lpy-font-lock-keywords)
(defun lpy-outline-comment-highlight (limit)
  (while (re-search-forward "^# \\(?:[^*]\\|$\\)" limit t)
    (let* ((pt (point))
	   (success (save-excursion
		      (and (re-search-backward "^# \\*" nil t)
			   (null (re-search-forward "^[^#]" pt t))))))
      (when success
	(set-match-data (list (line-beginning-position) (line-end-position)
			      (point) (line-end-position)))
	(end-of-line)
	t))))

(add-hook 'python-mode-hook
	  (lambda ()
	    (setq outline-regexp "# \\*+"
		  orgstruct-heading-prefix-regexp "# ?\\*+\\|\\`")
	    (orgstruct-mode)
	    (org-global-cycle 3)))


(provide 'scimax)

;;; scimax.el ends here
