;;; scimax-spellcheck.el --- Spell-check setup for scimax

;;; Commentary:
;;
;; I currently use hunspell, because it seems more up to date than aspell. It is
;; a little irritating to install though. For the Windows scimax, it should be
;; bundled in the installed files. On a mac, you can brew install hunspell, but
;; you have to download your own dictionaries and put them in
;; ~/Library/Spelling. These are not currently part of scimax.
;;

;; Adapted from https://manuel-uberti.github.io/emacs/2016/06/06/spellchecksetup/

;; [2019-08-10 Sat] I took out some mac/windows specific things. The plan is to
;; get this to work generally as much as possible, and not to hack a special
;; solution for windows.
(use-package ispell)

(use-package flyspell)

(defcustom scimax-aspell-language-option "--lang=en_US"
  "Option to use in `ispell-extra-args' to specify the default language."
  :type 'string
  :group 'scimax-aspell)

(use-package flyspell-correct-ivy
  :ensure t
  :init
  (setq ispell-program-name "aspell"
	ispell-extra-args `("--encoding=utf-8" "--sug-mode=ultra" ,scimax-aspell-language-option)
	flyspell-correct-interface 'flyspell-correct-ivy)

  (add-hook 'flyspell-incorrect-hook
	    (lambda (beg end sym)
	      "Show a message that reminds me how to correct a misspelled word."
	      (message "%s misspelled. Type %s to fix it."
		       (buffer-substring beg end)
		       (substitute-command-keys
			"\\[flyspell-correct-previous-word-generic]"))
	      ;; return nil so word is still highlighted.
	      nil))

  (add-hook 'org-mode-hook
	    (lambda ()
	      ;; do we need to start this? It seems like the first time your run
	      ;; flyspell-mode you get a nil error, and the second time it
	      ;; works.
	      (ispell-init-process)
	      (message "initializing spell-checkers!")
	      (flyspell-mode +1)
	      (flycheck-mode +1))
	    t)

  :after flyspell
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)))


;;* flyspell save abbrevs

;; I adapted this idea to define abbreviations while spell-checking
;; This uses the ivy selection I prefer.
;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
;; I adapted this function in flyspell-correct.el


(defcustom scimax-save-spellcheck-abbrevs t
  "If t save spellchecks as global-abbrevs.")

;; Note this redefines an alias in flyspell-correct that points to
;; `flyspell-correct-previous'.
(defun flyspell-correct-previous-word-generic (position)
  "Correct the first misspelled word that occurs before point.
But don't look beyond what's visible on the screen.

Uses `flyspell-correct-at-point' if installed or
`flyspell-correct-word-generic' function for correction."
  (interactive "d")
  (let ((top (window-start))
        (bot (window-end))
        (incorrect-word-pos)
        (position-at-incorrect-word))
    (save-excursion
      (save-restriction
        ;; make sure that word under point is checked first
        (forward-word)

        ;; narrow the region
        (narrow-to-region top bot)
        (overlay-recenter (point))

        (let ((overlay-list (overlays-in (point-min) (+ position 1)))
              (overlay 'dummy-value))

          (while overlay
            (setq overlay (car-safe overlay-list))
            (setq overlay-list (cdr-safe overlay-list))
            (when (and overlay
                       (flyspell-overlay-p overlay))
              (setq position-at-incorrect-word (and (<= (overlay-start overlay) position)
                                                    (>= (overlay-end overlay) position)))
              (setq incorrect-word-pos (overlay-start overlay))
              (setq overlay nil)))

          (when incorrect-word-pos
            (save-excursion
              (goto-char incorrect-word-pos)
	      (let (bef aft)
		(setq bef (word-at-point))
		;; See issue https://github.com/jkitchin/scimax/issues/336
		(if (fboundp 'flyspell-correct-at-point)
		    (flyspell-correct-at-point)
		  (flyspell-correct-word-generic))
		(goto-char incorrect-word-pos)
		(setq aft (word-at-point))
		(when (and scimax-save-spellcheck-abbrevs
			   (not (string= bef aft)))
		  (define-global-abbrev bef aft))))))))
    (when position-at-incorrect-word
      (forward-word))))


(defun flyspell-goto-prev-error ()
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
		(message "Restarting from beginning of buffer")
		(goto-char (point-min)))
	    (forward-word 1))
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
      (setq pos (1+ pos)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)
    (if (= pos min)
	(message "No more miss-spelled word!"))))



(provide 'scimax-spellcheck)

;;; scimax-spellcheck.el ends here
