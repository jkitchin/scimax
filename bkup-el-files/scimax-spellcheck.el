;;; scimax-spellcheck.el --- Spell-check setup for scimax

;;; Commentary:
;;
;; [2021-10-03 Sun] Updating the documentation. I am back to using aspell. It
;; isn't ideal, and previously I had used hunspell, but aspell seems easier to
;; get working on Windows.
;;
;; This setup uses `flyspell-correct-ivy'

(defcustom scimax-aspell-language-option "--lang=en_US"
  "Option to use in `ispell-extra-args' to specify the default language."
  :type 'string
  :group 'scimax-aspell)


(setq ispell-program-name "aspell"
      ispell-extra-args `("--encoding=utf-8" "--sug-mode=ultra" ,scimax-aspell-language-option))


(use-package flyspell-correct
  :after flyspell) 


(use-package flyspell-correct-ivy
  :after flyspell-correct
  :bind (:map flyspell-mode-map
	      ("C-;" . flyspell-correct-wrapper)
	      ("M-C-;" . scimax-ivy-jump-to-typo)
	      ("s-M-;" . scimax-spellcheck/body)))


(defun scimax-incorrect-word-tooltip (beg end sym)
  "Show a message that reminds me how to correct a misspelled word.
Used in `flyspell-incorrect-hook'."
  (message "%s misspelled. Type %s to fix it."
	   (buffer-substring beg end)
	   (substitute-command-keys
	    "\\[flyspell-correct-wrapper]"))
  ;; return nil so word is still highlighted.
  nil)


(defun scimax-flsypell-region-or-buffer (r1 r2)
  "Run flyspell on region defined by R! and R2 or buffer."
  (interactive "r")
  (if (region-active-p)
      (flyspell-region r1 r2)
    (flyspell-buffer)))


(defun scimax-ivy-jump-to-typo ()
  "Use AVY to jump to a typo, and correct it."
  (interactive)
  (save-excursion
    (avy-with avy-goto-typo
      (avy-process (cl-loop for ov in (ov-in 'flyspell-overlay) collect (overlay-start ov)))
      (avy--style-fn avy-style))
    (flyspell-correct-wrapper)))


(add-hook 'flyspell-incorrect-hook #'scimax-incorrect-word-tooltip)
(add-hook 'org-mode-hook 'flyspell-mode)


;; Add action to correct and save abbrev. I implement this as an override advice
;; because the actions are defined as closures in the function. This is a light
;; adaptation of what is in `flyspell-correct-ivy'.
(defun scimax-flyspell-correct-ivy (candidates word)
  "Run `ivy-read' for the given CANDIDATES.

List of CANDIDATES is given by flyspell for the WORD.

Return result according to `flyspell-correct-interface'
specification."
  (setq flyspell-correct-ivy--result nil)
  (let* ((action-default
          (lambda (x)
            (setq flyspell-correct-ivy--result x)))
         (action-save-word
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'save (if (member x candidates) word x)))))
         (action-accept-session
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'session (if (member x candidates) word x)))))
         (action-accept-buffer
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'buffer (if (member x candidates) word x)))))
         (action-skip-word
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'skip (if (member x candidates) word x)))))
         (action-stop
          (lambda (x)
            (setq flyspell-correct-ivy--result
                  (cons 'stop (if (member x candidates) word x)))))
	 ;; JRK added this
	 (action-abbrev
	  (lambda (x)
	    ;; Correct
	    (setq flyspell-correct-ivy--result x)
	    ;; and add abbrev
	    (define-abbrev global-abbrev-table word x)))
         (action `(1
                   ("o" ,action-default "correct")
		   ("a" ,action-abbrev "correct and save abbrev")
                   ("s" ,action-save-word "Save")
                   ("S" ,action-accept-session "Accept (session)")
                   ("b" ,action-accept-buffer "Accept (buffer)")
                   ("k" ,action-skip-word "Skip")
                   ("p" ,action-stop "Stop"))))
    (ivy-read (format "Suggestions for \"%s\" in dictionary \"%s\": "
                      word (or ispell-local-dictionary
                               ispell-dictionary
                               "Default"))
              candidates
              :action action
              :keymap flyspell-correct-ivy-map
              :caller 'flyspell-correct-ivy)
    flyspell-correct-ivy--result))


(advice-add 'flyspell-correct-ivy :override #'scimax-flyspell-correct-ivy)


;; Adapted from flyspell-goto-next-error
(defun scimax-flyspell-goto-prev-error ()
  "Go to the previous previously detected error.
In general FLYSPELL-GOTO-PREV-ERROR must be used after
FLYSPELL-BUFFER."
  (interactive)
  (let ((pos (point))
	(min (point-min)))
    (if (and (eq (current-buffer) flyspell-old-buffer-error)
	     (eq pos flyspell-old-pos-error))
	(progn
	  (if (= flyspell-old-pos-error min)
	      ;; goto beginning of buffer
	      (progn
		(message "Restarting from end of buffer")
		(goto-char (point-max)))
	    (backward-word 1))
	  (setq pos (point))))
    ;; seek the next error
    (while (and (> pos min)
		(let ((ovs (overlays-at pos))
		      (r '()))
		  (while (and (not r) (consp ovs))
		    (if (flyspell-overlay-p (car ovs))
			(setq r t)
		      (setq ovs (cdr ovs))))
		  (not r)))
      (setq pos (1- pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (if (= pos min)
	(message "No more miss-spelled word!"))))

;; * flyspell predicate function in scimax
;;
;; This variable `flyspell-generic-check-word-predicate' is used to determine if a word is mispelled. Org-mode sets this variable to `org-mode-flyspell-verify'. I have a need to augment this.

(defvar scimax-flyspell-predicates '(org-mode-flyspell-verify)
  "List of functions to check in order for flyspell.
Each function returns t if it should continue, and nil to ignore.")

(defun scimax-flyspell-verify ()
  "flyspell-verification function.
Only continue if all functions return t. If any returns nil, then
it should be marked."
  (cl-every (lambda (x) (funcall x)) scimax-flyspell-predicates))

(setq flyspell-generic-check-word-predicate #'scimax-flyspell-verify)


;; * typos

(defun typos ()
  "Run typos and make a clickable buffer to get to the typos.
See https://github.com/crate-ci/typos."
  (interactive)

  (unless (executable-find "typos")
    (error "typos was not found. Try: brew install typos-cli."))
  
  (let ((lines (split-string (string-trim (shell-command-to-string "typos --format json --exclude=*.png --exclude=*deprecated*")) "\n"))
	data)
    (with-current-buffer (get-buffer-create "*typos*")
      (erase-buffer)
      ;; (insert (shell-command-to-string "typos --format json"))
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

(provide 'scimax-spellcheck)

;;; scimax-spellcheck.el ends here
