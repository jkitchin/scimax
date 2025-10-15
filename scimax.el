;;; scimax.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; * Basic settings

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

;; (setq custom-file (expand-file-name "user/custom.el" scimax-dir))
;; (when (f-exists? custom-file) (load custom-file))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/saves-" scimax-dir))

;; abbrevs
;; (setq abbrev-file-name (expand-file-name "user/abbrev_defs" scimax-dir))
(setq save-abbrevs t)
(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; * Version control
;; Disable all version control. makes startup and opening files much faster
;; except git and svn which I actually use
(setq vc-handled-backends '(Git SVN))

(defun scimax-update ()
  "Update scimax from GitHUB."
  (interactive)
  (let ((default-directory scimax-dir))
    (when (not (string= "" (shell-command-to-string "git status --porcelain")))
      (shell-command "git add *")
      (shell-command "git commit -am \"committing scimax.\""))
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


;; * Misc

(require 'image-mode)
(define-key image-mode-map (kbd "q")
	    (lambda ()
	      (interactive)
	      (kill-buffer (current-buffer))))



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
	    (cl-loop for f in new-names do
		     (dired-goto-file f)
		     (dired-mark nil))))
	(goto-char p))
    (user-error "Not in dired")))

(define-key dired-mode-map (kbd "-") 'scimax-dired-cycle-space-hyphen-underscore)



;; * dubcaps
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals.
You can undo this if you do it right away."
  (interactive)
  (undo-boundary)
  (save-excursion
    (when (= ?w (char-syntax (char-before)))
      (when (if (called-interactively-p 'interactive)
		(skip-syntax-backward "w")
	      (= -3 (skip-syntax-backward "w")))
	(when (let (case-fold-search)
		(looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
	  (capitalize-word 1))))))

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
(if (and (>= 27 emacs-major-version)
	 (>= emacs-minor-version 1))
    (setq after-focus-change-function #'garbage-collect)
  ;; This should be ok for older emacs still.
  (add-hook 'focus-out-hook #'garbage-collect))

;; * The end
(provide 'scimax)

;;; scimax.el ends here
