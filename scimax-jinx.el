;;; scimax-jinx.el --- Spell-check setup using jinx for scimax -*- lexical-binding: t -*-

;;; Commentary:
;;
;; This module provides jinx-based spell checking for scimax, replacing the
;; older ispell/aspell-based scimax-spellcheck.el. Jinx uses a faster, more
;; modern spell checking engine.
;;
;; Features:
;; - Fast spell checking using jinx
;; - Ivy-based correction interface with multiple actions
;; - Jump to typos using avy
;; - Tooltip messages for misspelled words
;; - Navigation between spelling errors
;; - Integration with external typos CLI tool

;;; Code:

;; Check if enchant is installed (required by jinx)
(defun scimax-jinx-enchant-available-p ()
  "Return non-nil if enchant is available."
  (or (executable-find "enchant-2")
      (executable-find "enchant")))

;; Only load jinx if enchant is available
(if (scimax-jinx-enchant-available-p)
    (use-package jinx)
  (display-warning
   'scimax-jinx
   (concat "Enchant is not installed. Jinx requires enchant for spell checking.\n\n"
           "Installation instructions:\n"
           "  macOS:   brew install enchant\n"
           "  Ubuntu:  sudo apt-get install libenchant-2-dev\n"
           "  Fedora:  sudo dnf install enchant2-devel\n"
           "  Arch:    sudo pacman -S enchant\n"
           "  Windows: Download from https://github.com/AbiWord/enchant/releases\n\n"
           "After installing enchant, restart Emacs.")
   :warning))

;; Only define jinx-related functions if enchant is available
(when (scimax-jinx-enchant-available-p)

  (require 'ivy nil t)
  (require 'avy nil t)
  (declare-function jinx--correct-suggestions "jinx")
  (declare-function jinx--save-personal "jinx")
  (declare-function jinx--save-session "jinx")
  (declare-function jinx--recheck-overlays "jinx")
  (declare-function jinx-correct "jinx")
  (declare-function jinx-languages "jinx")
  (declare-function jinx--check-region "jinx")

  (defcustom scimax-jinx-languages "en_US"
    "Default language(s) for jinx spell checking.
  Can be a single language like \"en_US\" or multiple separated by spaces."
    :type 'string
    :group 'scimax-jinx)
  
  (defvar scimax-jinx-ivy--result nil
    "Temporary variable to store the result from ivy correction interface.")
  
  (defun scimax-incorrect-word-tooltip-jinx (overlay)
    "Show a message that reminds how to correct a misspelled word.
  OVERLAY is the jinx overlay at the misspelled word."
    (let ((word (buffer-substring (overlay-start overlay) (overlay-end overlay))))
      (message "%s misspelled. Type %s to fix it."
               word
               (substitute-command-keys "\\[jinx-correct]"))))
  
  (defun scimax-jinx-region-or-buffer (r1 r2)
    "Run jinx on region defined by R1 and R2 or entire buffer."
    (interactive "r")
    (if (region-active-p)
        (jinx--check-region r1 r2)
      (jinx--check-region (point-min) (point-max))))
  
  (defun scimax-ivy-jump-to-jinx-error ()
    "Use avy to jump to a jinx error and correct it."
    (interactive)
    (let ((overlays (cl-loop for ov in (overlays-in (point-min) (point-max))
                             when (eq (overlay-get ov 'category) 'jinx-overlay)
                             collect (overlay-start ov))))
      (if overlays
          (progn
            (save-excursion
              (avy-with avy-goto-jinx-error
                (avy-process overlays)
                (avy--style-fn avy-style)))
            (scimax-jinx-correct-at-point))
        (message "No spelling errors found"))))
  
  (defun scimax-jinx-ivy-correct (overlay &optional recenter)
    "Correct spelling error at OVERLAY using ivy interface.
  If RECENTER is non-nil, recenter after correction.
  
  This provides an ivy-based interface similar to flyspell-correct-ivy
  with actions for correcting, saving as abbrev, accepting, and skipping."
    (let* ((word (buffer-substring-no-properties
                  (overlay-start overlay)
                  (overlay-end overlay)))
           (suggestions (jinx--correct-suggestions word)))
  
      (setq scimax-jinx-ivy--result nil)
  
      (let* ((action-default
              (lambda (x)
                (message "Selected: %s" x)
                (setq scimax-jinx-ivy--result x)))
             (action-abbrev
              (lambda (x)
                ;; Correct the word
                (setq scimax-jinx-ivy--result x)
                ;; and add abbrev
                (define-abbrev global-abbrev-table word x)))
             (action-save-word
              (lambda (x)
                (setq scimax-jinx-ivy--result 'save)
                (jinx--save-personal t "s" word)
                (jinx--recheck-overlays)))
             (action-accept-session
              (lambda (x)
                (setq scimax-jinx-ivy--result 'session)
                (jinx--save-session t "S" word)))
             (action-skip-word
              (lambda (x)
                (setq scimax-jinx-ivy--result 'skip)))
             (action-stop
              (lambda (x)
                (setq scimax-jinx-ivy--result 'stop)))
             (action `(1
                       ("o" ,action-default "correct")
                       ("a" ,action-abbrev "correct and save abbrev")
                       ("s" ,action-save-word "Save to dictionary")
                       ("S" ,action-accept-session "Accept (session)")
                       ("k" ,action-skip-word "Skip")
                       ("p" ,action-stop "Stop"))))
  
        (ivy-read (format "Suggestions for \"%s\" [M-o for alternate actions]: " word)
                  suggestions
                  :action action
                  :require-match t
                  :caller 'scimax-jinx-ivy-correct)
  
        (when (and scimax-jinx-ivy--result
                   (stringp scimax-jinx-ivy--result))
          ;; Replace the misspelled word with the correction
          (save-excursion
            (goto-char (overlay-start overlay))
            (delete-region (overlay-start overlay) (overlay-end overlay))
            (insert scimax-jinx-ivy--result))
          (delete-overlay overlay))
  
        (when recenter
          (recenter))
  
        scimax-jinx-ivy--result)))
  
  (defun scimax-jinx-goto-next-error ()
    "Go to the next jinx error."
    (interactive)
    (let* ((pos (point))
           (overlays (cl-loop for ov in (overlays-in pos (point-max))
                              when (eq (overlay-get ov 'category) 'jinx-overlay)
                              collect ov)))
      (if overlays
          (progn
            (goto-char (overlay-start (car overlays)))
            (scimax-incorrect-word-tooltip-jinx (car overlays)))
        (message "No more misspelled words!"))))
  
  (defun scimax-jinx-goto-prev-error ()
    "Go to the previous jinx error."
    (interactive)
    (let* ((pos (point))
           (overlays (cl-loop for ov in (overlays-in (point-min) pos)
                              when (eq (overlay-get ov 'category) 'jinx-overlay)
                              collect ov)))
      (if overlays
          (progn
            (goto-char (overlay-start (car (last overlays))))
            (scimax-incorrect-word-tooltip-jinx (car (last overlays))))
        (message "No more misspelled words!"))))
  
  ;; Helper function to find jinx overlays
  (defun scimax-jinx-get-overlays-in-buffer ()
    "Get all jinx overlays in the current buffer."
    (cl-loop for ov in (overlays-in (point-min) (point-max))
             when (overlay-get ov 'category)
             when (eq (overlay-get ov 'category) 'jinx-overlay)
             collect ov))
  
  ;; Override jinx's default correction interface with our ivy-based one
  (defun scimax-jinx-correct-at-point ()
    "Find the nearest misspelled word and correct it using ivy interface.
  Searches forward from point first, then backward if nothing found ahead."
    (interactive)
    ;; First check if jinx-mode is active
    (unless (bound-and-true-p jinx-mode)
      (user-error "Jinx mode is not active. Enable it with M-x jinx-mode"))
  
    (let ((overlay nil))
      ;; Check if we're on an overlay
      (setq overlay (cl-find-if
                     (lambda (ov) (eq (overlay-get ov 'category) 'jinx-overlay))
                     (overlays-at (point))))
  
      (unless overlay
        ;; Not on an error, find the nearest one
        ;; First try backward from point
        (let ((prev-overlays (cl-loop for ov in (overlays-in (point-min) (point))
                                      when (eq (overlay-get ov 'category) 'jinx-overlay)
                                      collect ov)))
          (if prev-overlays
              (setq overlay (car (last prev-overlays)))
            ;; Nothing backward, try forward from point
            (let ((next-overlays (cl-loop for ov in (overlays-in (point) (point-max))
                                          when (eq (overlay-get ov 'category) 'jinx-overlay)
                                          collect ov)))
              (when next-overlays
                (setq overlay (car next-overlays)))))))
  
      (if overlay
          (save-excursion
            (goto-char (overlay-start overlay))
            (scimax-jinx-ivy-correct overlay))
        (message "No misspelled words found. Jinx may need time to check the buffer."))))
  
  ;; Key bindings for jinx
  (with-eval-after-load 'jinx
    ;; Global keybindings
    (global-set-key (kbd "M-$") 'jinx-correct)
    (global-set-key (kbd "C-M-$") 'jinx-languages)
    ;; Mode-specific keybindings
    (define-key jinx-mode-map (kbd "C-;") 'scimax-jinx-correct-at-point)
    (define-key jinx-mode-map (kbd "M-C-;") 'scimax-ivy-jump-to-jinx-error)
    (define-key jinx-mode-map (kbd "M-n") 'scimax-jinx-goto-next-error)
    (define-key jinx-mode-map (kbd "M-p") 'scimax-jinx-goto-prev-error))
  
  ;; Enable jinx-mode in org-mode by default
  (add-hook 'org-mode-hook 'jinx-mode))

;; * Compatibility with scimax-editmarks
;; scimax-editmarks uses scimax-flyspell-predicates which was defined in
;; scimax-spellcheck.el. We define it here for compatibility even though
;; jinx doesn't use the same predicate system as flyspell.

(defvar scimax-flyspell-predicates '()
  "List of functions to check in order for flyspell.
Each function returns t if it should continue, and nil to ignore.
This variable is maintained for compatibility with scimax-editmarks,
but is not used by jinx.")

;; * typos integration (same as scimax-spellcheck.el)

(defun typos ()
  "Run typos and make a clickable buffer to get to the typos.
See https://github.com/crate-ci/typos."
  (interactive)

  (unless (executable-find "typos")
    (error "typos was not found. Try: brew install typos-cli"))

  (let ((lines (split-string (string-trim (shell-command-to-string "typos --format json --exclude=*.png --exclude=*deprecated*")) "\n"))
        data)
    (with-current-buffer (get-buffer-create "*typos*")
      (erase-buffer)
      (cl-loop for line in lines do
               (let-alist (json-read-from-string line)
                 (insert (format "- %s [[elisp:(progn (find-file \"%s\") (goto-line %s)(forward-char %s))][%s]] %s -> %s\n"
                                 .type
                                 .path
                                 .line_num .byte_offset
                                 .path
                                 .typo .corrections))))
      (org-mode))
    (pop-to-buffer "*typos*")))

(provide 'scimax-jinx)

;;; scimax-jinx.el ends here
