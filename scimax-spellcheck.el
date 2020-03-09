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


(provide 'scimax-spellcheck)

;;; scimax-spellcheck.el ends here
