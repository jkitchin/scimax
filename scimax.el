;;; scimax.el ---

;;; Commentary:
;;
;; * Basic settings
(load-theme 'leuven)

;; Source code pro for the font if it is available

(let ((f "Source Code Pro"))
  (when (member f (font-family-list))
    (set-face-attribute 'default nil :font f)))

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
(when (f-exists? custom-file) (load custom-file))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" scimax-dir))

;; abbrevs
(setq abbrev-file-name (expand-file-name "user/abbrev_defs" scimax-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; * Version control
;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

(defun scimax-update ()
  "Update scimax from github."
  (interactive)
  (let ((default-directory scimax-dir))
    (when (not (string= "" (shell-command-to-string "git status --porcelain")))
      (shell-command "git add *")
      (shell-command "git commit -am \"commiting scimax.\""))
    (shell-command "git pull origin master")
    (shell-command "git submodule update")
    (load-file "init.el")))

;; * Diminish modes
;; (diminish 'orgstruct-mode)
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

;; (defun lisp-outline-setup ()
;;   "Setup outline and orgstruct mode for emacs-lisp code.
;; This enables you to use tab to open and close outlines."
;;   (setq-local outline-regexp ";; ?\\*+\\|\\`")
;;   (setq-local orgstruct-heading-prefix-regexp ";; ?\\*+\\|\\`")
;;   (outline-minor-mode)
;;   (orgstruct-mode)
;;   (outline-show-branches))

;; (add-hook 'emacs-lisp-mode-hook
;; 	  #'lisp-outline-setup)

;; (remove-hook 'emacs-lisp-mode-hook
;;  	  #'lisp-outline-setup)

;; ** Python
(setq python-indent-offset 4)

;; This eliminates an annoying message about the interpreter not using
;; readline. That doesn't seem to matter at all.
(setq warning-suppress-types '((python)
			       (emacs)))

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

;; (add-hook 'python-mode-hook
;; 	  (lambda ()
;; 	    (setq outline-regexp "# \\*+"
;; 		  orgstruct-heading-prefix-regexp "# ?\\*+\\|\\`")
;; 	    (orgstruct-mode)
;; 	    (org-global-cycle 3)))

;; * Misc

(require 'image-mode)
(define-key image-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))

;; * Navigation

(defvar navy-l 'forward-char
  "The next item in a forward sense.")

(defvar navy-j 'backward-char
  "The previous item in a backward sense.")

(defvar navy-i 'previous-line
  "The previous item in an up sense.")

(defvar navy-k 'next-line
  "The next item in a down sense.")

(defvar navy-semicolon 'avy-goto-char
  "Command bound to ;.")

(defvar navy-quote 'avy-goto-line
  "Command bound to '.")

(defvar navy-comma 'avy-goto-char-2
  "Command bound to ,")

(defvar navy-period 'avy-goto-word-0
  "Command bound to .")

(defvar navy-slash 'end-of-visual-line
  "The end of an item.")

(defvar navy-h 'beginning-of-visual-line
  "Command bound to h, usually a beginning of command.")

(defvar navy-mode "char"
  "The active mode.")


(defhydra navy (:color red :hint nil)
  "
%s(format \"%s-mode\" navy-mode)
%s(make-string (length (symbol-name navy-j)) ? )     _i_: %`navy-i
%`navy-j :_j_     _l_: %`navy-l     _;_: %`navy-semicolon  _'_: %`navy-quote
%s(make-string (length (symbol-name navy-j)) ? )     _k_: %`navy-k
  _,_: %`navy-comma _._: %`navy-period _/_: %`navy-slash
  point-min: _<_    _>_: point-max

"
  ("j" (funcall navy-j))
  ("l" (funcall navy-l))
  ("i" (funcall navy-i))
  ("k" (funcall navy-k))

  ("q" nil "quit" :color blue)

  ("h" (call-interactively navy-h))

  (";" (call-interactively navy-semicolon))
  ("'" (call-interactively navy-quote))

  ("," (call-interactively navy-comma))
  ("." (call-interactively navy-period))
  ("/" (call-interactively navy-slash))

  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ;; these are different modes
  ;; char

  ("c" (lambda ()
	 (interactive)
	 (setq navy-mode "char"
	       navy-j 'backward-char
	       navy-i 'previous-line
	       navy-l 'forward-char
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-char-in-line
	       navy-period 'avy-goto-word-1))
   "char mode")

  ("w" (lambda ()
	 (interactive)
	 (setq navy-mode "word"
	       navy-j 'backward-word
	       navy-i 'previous-line
	       navy-l 'forward-word
	       navy-k 'next-
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "word mode")

  ("s" (lambda ()
	 (interactive)
	 (setq navy-mode "sentence"
	       navy-j 'backward-sentence
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-l 'forward-sentence
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "sentence mode")

  ("p" (lambda ()
	 (interactive)
	 (setq navy-mode "paragraph"
	       navy-j 'backward-paragraph
	       navy-l 'forward-paragraph
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "paragraph mode")

  ("g" (lambda ()
	 (interactive)
	 (setq navy-mode "page"
	       navy-j 'backward-page
	       navy-l 'forward-page
	       navy-i 'backward-page
	       navy-k 'forward-page
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "page mode")

  ("n" (lambda ()
	 (interactive)
	 (setq navy-mode "line"
	       navy-i 'avy-goto-line-above
	       navy-k 'avy-goto-line-below
	       navy-l 'next-line
	       navy-j 'previous-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "line mode")

  ("x" (lambda ()
	 (interactive)
	 (setq navy-mode "sexp"
	       navy-j 'backward-sexp
	       navy-l 'forward-sexp
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'lispy-ace-symbol
	       navy-period 'lispy-ace-paren))
   "sexp mode")

  ("a" swiper-all "swiper-all")
  ("o" helm-org-agenda-files-headings "org headlines")
  ("r" counsel-git-grep "git grep")
  ("t" avy-goto-char-timer "char timer"))


(defun navy ()
  "Run the `navy/body' hydra."
  (interactive)
  (setq navy-mode "char"
	navy-j 'backward-char
	navy-i 'previous-line
	navy-l 'forward-char
	navy-k 'next-line
	navy-quote 'avy-goto-line
	navy-comma 'avy-goto-char-2
	navy-period 'avy-goto-char-in-line
	navy-h 'beginning-of-visual-line
	navy-semicolon 'avy-goto-char)
  (navy/body))


;; * dired enhancements
;; http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html
(require 'dired )

(defun scimax-dired-cycle-space-hyphen-underscore ()
  "In dired, rename current or marked files by cycling spaces->hyphens->underscores.
We only change the filename, not the rest of the path.
Adapted from http://ergoemacs.org/emacs/elisp_dired_rename_space_to_underscore.html."
  (interactive)
  (require 'f)
  (require 'dired-aux)
  (if (equal major-mode 'dired-mode)
      (let ((p (point))
	    (new-names '())
	    ;; evals to 2 if only one file is marked
	    (number-marked-files (length (dired-get-marked-files nil nil nil t))))
	(progn
	  (mapc (lambda (x)
		  (let* ((path-parts (f-split x))
			 (path-file-name (car (last path-parts))))
		    (cond
		     ;; There is a space, so we switch them all to -
		     ((string-match " " path-file-name)
		      (setcar (last path-parts)
			      (replace-regexp-in-string " " "-" path-file-name)))
		     ;; no spaces, but - gets converted to _
		     ((string-match "-" path-file-name)
		      (setcar (last path-parts)
			      (replace-regexp-in-string "-" "_" path-file-name)))
		     ;; no -, convert _ to spaces
		     ((string-match "_" path-file-name)
		      (setcar (last path-parts)
			      (replace-regexp-in-string "_" " " path-file-name))))

		    ;; now rename the file if match
		    (if (string-match "[\-|_| ]" path-file-name)
			(dired-rename-file x (apply 'f-join path-parts) nil))
		    ;; and save it so we can remark it at the end
		    (push (apply 'f-join path-parts) new-names)))
		(dired-get-marked-files))
	  (revert-buffer)
	  (when (not (eq 1 number-marked-files))
	    (loop for f in new-names do
		  (dired-goto-file f)
		  (dired-mark nil))))
	(goto-char p))
    (user-error "Not in dired")))

(define-key dired-mode-map (kbd "-") 'scimax-dired-cycle-space-hyphen-underscore)



;; * dubcaps
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook #'dubcaps-mode)

;; * copy/kill-dwim

(defun scimax-copy-dwim (arg)
  "Copy and do what I mean.
If a region is active copy it.
If at the beginning of a sentence, copy it.
If at the beginning of a paragraph copy it.
Default to copying the word at point"
  (interactive "P")
  (cond
   ((region-active-p)
    (kill-ring-save (region-beginning) (region-end)))
   ;; paragraph
   ((let ((cp (point)))
      (save-excursion
	(forward-paragraph)
	(backward-paragraph)
	;; if the preceding line is blank we end up on it. this moves us back to
	;; the beginning of the paragraph.
	(when (looking-at "^$") (forward-line))
	(= cp (point))))
    (kill-ring-save (point) (save-excursion (forward-paragraph) (point))))
   ;; sentence
   ((let ((cp (point)))
      (save-excursion
	(forward-sentence)
	(backward-sentence)
	(= cp (point))))
    (let* ((bounds (bounds-of-thing-at-point 'sentence))
	   (start (car bounds))
	   (end (cdr bounds)))
      (kill-ring-save start end)))
   ((bolp)
    (kill-ring-save (line-beginning-position) (line-end-position)))
   ;; default to word
   (t
    (let* ((bounds (bounds-of-thing-at-point 'word))
	   (start (car bounds))
	   (end (cdr bounds)))
      (kill-ring-save start end)))))

(defun scimax-kill-dwim (arg)
  "Kill and do what I mean.
If a region is active kill it.
If at the beginning of a sentence, kill it.
If at the beginning of a paragraph kill it.
Default to killing the word at point"
  (interactive "P")
  (cond
   ((region-active-p)
    (kill-region (region-beginning) (region-end)))
   ;; paragraph
   ((let ((cp (point)))
      (save-excursion
	(forward-paragraph)
	(backward-paragraph)
	;; if the preceding line is blank we end up on it. this moves us back to
	;; the beginning of the paragraph.
	(when (looking-at "^$") (forward-line))
	(= cp (point))))
    (kill-region (point) (save-excursion (forward-paragraph) (point))))
   ;; sentence
   ((let ((cp (point)))
      (save-excursion
	(forward-sentence)
	(backward-sentence)
	(= cp (point))))
    (let* ((bounds (bounds-of-thing-at-point 'sentence))
	   (start (car bounds))
	   (end (cdr bounds)))
      (kill-region start end)))
   ((bolp)
    (kill-line))
   ;; default to word
   (t
    (let* ((bounds (bounds-of-thing-at-point 'word))
	   (start (car bounds))
	   (end (cdr bounds)))
      (kill-region start end)))))


;; * garbage-collect when you switch out of Emacs
(add-hook 'focus-out-hook #'garbage-collect)

;; * The end
(provide 'scimax)

;;; scimax.el ends here
