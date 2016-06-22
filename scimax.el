;;; scimax.el ---

;;; Commentary:
;; 
;; * Basic settings
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

(setq custom-file (expand-file-name "user/custom.el" scimax-dir))
(setq abbrev-file-name (expand-file-name "user/abbrev_defs" scimax-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; * Version control
;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

;; * Diminish modes
(diminish 'orgstruct-mode)
(diminish 'ivy-mode)
(diminish 'lispy-mode)
(diminish 'abbrev-mode)
(diminish 'visual-line-mode)
(diminish 'beacon-mode)
(diminish 'aggressive-indent-mode)
(diminish 'emacs-keybinding-command-tooltip-mode)

;; * Programming
;; ** debugging
(add-hook 'edebug-mode-hook
	  (lambda ()
	    (define-key edebug-mode-map (kbd "h") 'edebug-goto-here)))

;; ** Emacs lisp
;; Setup pretty outlines in Emacs-lisp code
"^;; \\(\\*+.*\\)$"
(defconst lel-font-lock-keywords
  '(("^;; ?\\(\\* .*\\)$" 1 'org-level-1 prepend)
    ("^;; ?\\(\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^;; ?\\(\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^;; ?\\(\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^;; ?\\(\\*\\*\\*\\*\\* .*\\)$" 1 'org-level-5 prepend)
    ;; (lel-outline-comment-highlight 1 'default prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(font-lock-add-keywords 'emacs-lisp-mode lel-font-lock-keywords)

(defun lisp-outline-setup ()
  "Setup outline and orgstruct mode for emacs-lisp code.
This enables you to use tab to open and close outlines."
  (setq-local outline-regexp ";; ?\\*+\\|\\`")
  (setq-local orgstruct-heading-prefix-regexp ";; ?\\*+\\|\\`")
  (outline-minor-mode)
  (orgstruct-mode)
  (outline-show-branches))

(add-hook 'emacs-lisp-mode-hook
	  #'lisp-outline-setup)

;; ** Python

(elpy-enable)

(setq python-indent-offset 4)

;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)))

;; *** Outlines in python code
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

;; * Misc

(require 'image-mode)
(define-key image-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))

;; * Navigation

(defvar navy-forward 'forward-char
  "The next item in a forward sense.")

(defvar navy-backward 'backward-char
  "The previous item in a backward sense.")

(defvar navy-up 'previous-line
  "The previous item in an up sense.")

(defvar navy-down 'next-line
  "The next item in a down sense.")

(defvar navy-avy-1 'avy-goto-char-in-line
  "Preferred avy for item.")

(defvar navy-avy-2 'avy-goto-char-2
  "Preferred 2nd avy")

(defvar navy-avy-3 'avy-goto-line
  "Preferred 3rd avy")

(defvar navy-beginning 'beginning-of-visual-line
  "The beginning of an item.")

(defvar navy-end 'end-of-visual-line
  "The end of an item.")

(defvar navy-mode "char"
  "The active mode.")


(defhydra navy (:color red :hint nil)
  "
%(format \"%s-mode\" navy-mode)
                                     _i_: %`navy-up
_h_: %`navy-beginning    _j_:%`navy-backward    _k_: %`navy-forward  _;_: %`navy-end
                                     _k_: %`navy-down
                
                              _<_: point-min _>_: point-max     
                    _'_: %`navy-avy-1  _,_: %`navy-avy-2 _._: %`navy-avy-3
   
"
  ("j" (funcall navy-backward))
  ("l" (funcall navy-forward))
  ("i" (funcall navy-up))
  ("k" (funcall navy-down))

  ("q" nil "quit" :color blue)

  ("h" (call-interactively navy-beginning))
  (";" (call-interactively navy-end))
  
  ("'" (call-interactively navy-avy-1))
  ("," (call-interactively navy-avy-2))
  ("." (call-interactively navy-avy-3))

  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ;; these are different modes
  ;; char
  ("c" (lambda ()
	 (interactive)
	 (setq navy-mode "char"
	       navy-backward 'backward-char
	       navy-up 'previous-line
	       navy-forward 'forward-char 
	       navy-down 'next-line
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'avy-goto-char-2
	       navy-avy-3 'avy-goto-line))
   "char mode")

  ("w" (lambda ()
	 (interactive)
	 (setq navy-mode "word"
	       navy-backward 'backward-word
	       navy-up 'previous-line
	       navy-forward 'forward-word 
	       navy-down 'next-line
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'avy-goto-word-1 
	       navy-avy-3 'avy-goto-word-or-subword-1))
   "word mode")
  ("s" (lambda ()
	 (interactive)
	 (setq navy-mode "sentence"
	       navy-backward 'backward-sentence
	       navy-up 'previous-line
	       navy-down 'next-line
	       navy-forward 'forward-sentence 
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'avy-goto-char-2
	       navy-avy-3 'avy-goto-word-1))
   "sentence mode")
  
  ("p" (lambda ()
	 (interactive)
	 (setq navy-mode "paragraph"
	       navy-backward 'backward-paragraph
	       navy-forward 'forward-paragraph
	       navy-up 'previous-line
	       navy-down 'next-line
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'avy-goto-char-2
	       navy-avy-3 'avy-goto-line))
   "paragraph mode")

  ("g" (lambda ()
	 (interactive)
	 (setq navy-mode "page"
	       navy-backward 'backward-page
	       navy-forward 'forward-page
	       navy-up 'backward-page
	       navy-down 'forward-page
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'avy-goto-char-2
	       navy-avy-3 'avy-goto-line))
   "page mode")

  ("n" (lambda ()
	 (interactive)
	 (setq navy-mode "line"
	       navy-up 'avy-goto-line-above
	       navy-down 'avy-goto-line-below
	       navy-forward 'next-line
	       navy-backward 'previous-line
	       navy-avy-3 'avy-goto-line))
   "line mode")
  
  ("x" (lambda ()
	 (interactive)
	 (setq navy-mode "sexp"
	       navy-backward 'backward-sexp
	       navy-forward 'forward-sexp
	       navy-up 'previous-line
	       navy-down 'next-line
	       navy-avy-1 'avy-goto-char-in-line
	       navy-avy-2 'lispy-ace-paren
	       navy-avy-3 'lispy-ace-symbol)) 
   "sexp mode")

  ("r" counsel-git-grep "git grep")
  ("t" avy-goto-char-timer "char timer"))


(defun navy ()
  (interactive)
  (setq navy-backward 'backward-char
	navy-up 'previous-line
	navy-forward 'forward-char 
	navy-down 'next-line
	navy-avy-1 'avy-goto-char-in-line
	navy-avy-2 'avy-goto-char-2
	navy-avy-3 'avy-goto-line
	navy-beginning 'beginning-of-visual-line
	navy-end 'end-of-visual-line)
  (navy/body))

;;  I mapped Capslock to f12
(global-set-key (kbd "<f12>") 'navy)


;; https://pqrs.org/osx/karabiner/seil.html.en
;; (defhydra hydra-avy (:color blue :hint nil)
;;   "jump" 
;;   ("s" counsel-grep-or-swiper "swiper") 
;;   ("j" avy-goto-char-2 "char2")
;;   ("k" avy-goto-word-1 "word1")
;;   ("l" avy-goto-line "line")
;;   (";" avy-goto-char-in-line "char in line")
;;   ("t" avy-goto-char-timer "char timer")
;;   ("g" counsel-git-grep "git grep")
;;   ("h" ivy-org-jump-to-heading "org heading")
;;   ("a" ivy-org-jump-to-agenda-heading "agenda heading"))


;; * The end
(provide 'scimax)

;;; scimax.el ends here
