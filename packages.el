;;; packages.el --- Install and configure scimax packages
;;; Commentary:
;;
;; This is a starter kit for scimax. This package provides a
;; customized setup for emacs that we use daily for scientific
;; programming and publication.
;;
;; see https://github.com/jwiegley/use-package for details on use-package


;;; Code:

(setq use-package-always-ensure t)

;; * org-mode
;; load this first before anything else to avoid mixed installations
(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window
	org-support-shift-select t)

  ;; I like to press enter to follow a link. mouse clicks also work.
  (setq org-return-follows-link t)
  :bind
  (("C-c l" . org-store-link)
   ("C-c L" . org-insert-link-global)
   ("C-c o" . org-open-at-point-global)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("s-<SPC>" . org-mark-ring-goto)
   ("H-." . org-time-stamp-inactive)))

;; [2019-01-07 Mon] This is another package I don't use, and that sometimes is a
;; problem on windows installations
;; (use-package org-bullets)

;; [2019-01-07 Mon] I don't use this now, and it frequently causes an issue on
;; installing scimax
;; (use-package org-edna
;;   :init (org-edna-load))


;; * Other packages
(use-package diminish)

(use-package aggressive-indent
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package auto-complete
  :diminish auto-complete-mode
  :config (ac-config-default))

(use-package avy)


;; May 24, 2017: this seems to be causing emacs 25.2 to be crashing on my linux box.
(unless (eq system-type 'gnu/linux)
  (use-package tex
    :ensure auctex))

;; [2019-01-07 Mon] This also sometimes causes problems installing scimax,
;; especially on Windows.
;; Make cursor more visible when you move a long distance
;; (use-package beacon
;;   :config
;;   (beacon-mode 1))


(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "user/bookmarks" scimax-dir)
	bookmark-save-flag 1))


(use-package bookmark+
  ;; I am not currently using this, and it loads a bunch of files on startup.
  :disabled t)

(use-package button-lock)

;; Potential for commandline scripts using emacs
(use-package commander
  :disabled t)

(use-package drag-stuff)

(use-package swiper
  :bind
  ("C-s" . counsel-grep-or-swiper)
  ("H-s" . swiper-all)
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package counsel
  :init
  (require 'ivy)
  (setq projectile-completion-system 'ivy)
  (setq ivy-use-virtual-buffers t)
  (define-prefix-command 'counsel-prefix-map)
  (global-set-key (kbd "H-c") 'counsel-prefix-map)

  ;; default pattern ignores order.
  (setf (cdr (assoc t ivy-re-builders-alist))
	'ivy--regex-ignore-order)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-info-lookup-symbol)
   ("H-c r" . ivy-resume)
   ("H-c l" . counsel-load-library)
   ("H-c f" . counsel-git)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (progn
    (counsel-mode)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    (define-key ivy-minibuffer-map (kbd "M-<SPC>") 'ivy-dispatching-done)

    ;; C-RET call and go to next
    (define-key ivy-minibuffer-map (kbd "C-<return>")
      (lambda ()
	"Apply action and move to next/previous candidate."
	(interactive)
	(ivy-call)
	(ivy-next-line)))

    ;; M-RET calls action on all candidates to end.
    (define-key ivy-minibuffer-map (kbd "M-<return>")
      (lambda ()
	"Apply default action to all candidates."
	(interactive)
	(ivy-beginning-of-buffer)
	(loop for i from 0 to (- ivy--length 1)
	      do
	      (ivy-call)
	      (ivy-next-line)
	      (ivy--exhibit))
	(exit-minibuffer)))

    ;; s-RET to quit
    (define-key ivy-minibuffer-map (kbd "s-<return>")
      (lambda ()
	"Exit with no action."
	(interactive)
	(ivy-exit-with-action
	 (lambda (x) nil))))

    ;; Show keys
    (define-key ivy-minibuffer-map (kbd "?")
      (lambda ()
	(interactive)
	(describe-keymap ivy-minibuffer-map)))

    (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
    (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-backward-delete-char)))

;; Provides functions for working on lists
(use-package dash)
(use-package dash-functional)

(use-package dashboard)

(use-package elfeed)

;; Python editing mode
(use-package elpy
  :config
  (elpy-enable))

(use-package esup)

;; Provides functions for working with files
(use-package f)

;; https://github.com/amperser/proselint
;; pip install proselint
(use-package flycheck
  ;; Jun 28 - I like this idea, but sometimes this is too slow.
  :config
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'org-mode-hook #'flycheck-mode)
  (define-key flycheck-mode-map (kbd "s-;") 'flycheck-previous-error))


;; https://manuel-uberti.github.io/emacs/2016/06/06/spellchecksetup/
(use-package flyspell-correct-ivy
  :ensure t
  :init
  (if (file-directory-p (expand-file-name "emacs-win" scimax-dir))
      (progn
	;; spell-checking on windows
	(setq ispell-program-name
	      (expand-file-name
	       "emacs-win/bin/hunspell"
	       scimax-dir))

	(setq ispell-dictionary "english")

	(setq ispell-local-dictionary-alist
	      `(("english"
		 "[[:alpha:]]"
		 "[^[:alpha:]]"
		 "[']"
		 t
		 ("-d" "en_US" "-p" ,(expand-file-name
				      "emacs-win/share/hunspell/en_US"
				      scimax-dir))
		 nil
		 utf-8))))
    (setenv "DICPATH" (expand-file-name "~/Library/Spelling"))
    (setq ispell-program-name (executable-find "hunspell")
	  ispell-dictionary "en_US"
	  ispell-local-dictionary "en_US"
	  ispell-local-dictionary-alist
	  `(("english"
	     "[[:alpha:]]"
	     "[^[:alpha:]]"
	     "[']"
	     t
	     ("-d" "en_US" "-p" ,(expand-file-name "~/Library/Spelling/"))
	     nil
	     utf-8)
	    ("en_US"
	     "[[:alpha:]]"
	     "[^[:alpha:]]"
	     "[']"
	     t
	     ("-d" "en_US" "-p" ,(expand-file-name "~/Library/Spelling/"))
	     nil
	     utf-8))
	  flyspell-correct-interface 'flyspell-correct-ivy))
  (add-hook 'flyspell-incorrect-hook
	    (lambda (beg end sym)
	      (message "%s misspelled. Type %s to fix it."
		       (buffer-substring beg end)
		       (substitute-command-keys
			"\\[flyspell-correct-previous-word-generic]"))
	      ;; return nil so word is still highlighted.
	      nil))
  (add-hook 'text-mode-hook
	    (lambda ()
	      (flyspell-mode)
	      (flycheck-mode)))

  (add-hook 'org-mode-hook
	    (lambda ()
	      (flyspell-mode +1)
	      (flycheck-mode +1)))

  :after flyspell
  :config
  (progn
    (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)))


(use-package flx)

(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))

;; google-this
(use-package google-this
  :config
  (google-this-mode 1))

(use-package helm
  :init (setq helm-command-prefix-key "C-c h")
  :bind
  ("<f7>" . helm-recentf)
  ;; ("M-x" . helm-M-x)
  ;; ("M-y" . helm-show-kill-ring)
  ;; ("C-x b" . helm-mini)
  ;; ("C-x C-f" . helm-find-files)
  ;; ("C-h C-f" . helm-apropos)
  :config
  (add-hook 'helm-find-files-before-init-hook
	    (lambda ()
	      (helm-add-action-to-source
	       "Insert path"
	       (lambda (target)
		 (insert (file-relative-name target)))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Insert absolute path"
	       (lambda (target)
		 (insert (expand-file-name target)))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Attach file to email"
	       (lambda (candidate)
		 (mml-attach-file candidate))
	       helm-source-find-files)

	      (helm-add-action-to-source
	       "Make directory"
	       (lambda (target)
		 (make-directory target))
	       helm-source-find-files))))


(use-package helm-bibtex)

(use-package helm-projectile)

(use-package help-fns+
  :load-path scimax-dir)

;; Functions for working with hash tables
(use-package ht)

(use-package htmlize)

(use-package hy-mode)

(use-package hydra
  :init
  (setq hydra-is-helpful t)

  :config
  (require 'hydra-ox))

(use-package ivy-hydra)

(use-package jedi)

(use-package jedi-direx)

;; Superior lisp editing
(use-package lispy
  :config
  (dolist (hook '(emacs-lisp-mode-hook
		  hy-mode-hook))
    (add-hook hook
	      (lambda ()
		(lispy-mode)
		(eldoc-mode)))))

(use-package magit
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("<f5>" . magit-status)
  ("C-c v t" . magit-status))

(use-package magithub
  :after magit)

;; Templating system
;; https://github.com/Wilfred/mustache.el
(use-package mustache)

;; this is a git submodule
(if (executable-find "jupyter")
    (use-package ob-ipython
      :ensure nil
      :load-path (lambda () (expand-file-name "ob-ipython-upstream" scimax-dir))
      :init (add-to-list 'load-path (expand-file-name "ob-ipython-upstream" scimax-dir))
      (require 'ob-ipython))
  (message "jupyter was not found on your path. ob-ipython was not loaded."))

(use-package scimax-org-babel-ipython-upstream
  :ensure nil
  :load-path scimax-dir)

(use-package ov)

;; [2019-01-23 Wed] commented out. I don't use this at all, and it causes an error on Windows when starting up.
;; (use-package org-edit-latex)

(use-package pdf-tools)

(use-package org-mime
  :ensure nil
  :load-path (lambda () (expand-file-name "org-mime" scimax-dir))
  :init (setq org-mime-up-subtree-heading 'org-back-to-heading
	      org-mime-export-options '(:section-numbers nil
							 :with-author nil
							 :with-toc nil
							 :with-latex dvipng)))

;; this is a git submodule
(use-package org-ref
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
  :init
  (add-to-list 'load-path
	       (expand-file-name "org-ref" scimax-dir))
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5
	org-ref-bibtex-hydra-key-binding (kbd "H-b"))
  ;; (define-key bibtex-mode-map org-ref-bibtex-hydra-key-binding 'org-ref-bibtex-hydra/body)
  ;; (global-set-key (kbd "H-b") 'org-ref-bibtex-hydra/body)
  )

(use-package org-ref-arxiv
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref" scimax-dir)))

(use-package org-ref-scopus
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref" scimax-dir)))

(use-package org-ref-wos
  :ensure nil
  :load-path (lambda () (expand-file-name "org-ref" scimax-dir)))


;; https://github.com/bbatsov/projectile
(use-package projectile
  :init (setq projectile-cache-file
	      (expand-file-name "user/projectile.cache" scimax-dir)
	      projectile-known-projects-file
	      (expand-file-name "user/projectile-bookmarks.eld" scimax-dir))
  :bind
  ("C-c pp" . projectile-switch-project)
  ("C-c pb" . projectile-switch-to-buffer)
  ("C-c pf" . projectile-find-file)
  ("C-c pg" . projectile-grep)
  ("C-c pk" . projectile-kill-buffers)
  ;; nothing good in the modeline to keep.
  :diminish ""
  :config
  (define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
  (projectile-global-mode))

(use-package pydoc)

(use-package rainbow-mode)

(use-package recentf
  :config
  (setq recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
          ".*png$" "\\*message\\*" "auto-save-list\\*"))
  (setq recentf-max-saved-items 60))


;; Functions for working with strings
(use-package s)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))


;; keep recent commands available in M-x
(use-package smex)

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

;; Note ws-butler-global-mode causes some issue with org-ref ref links. If you
;; are right after one you cannot add a space without getting a new line.
(use-package ws-butler)

(use-package yasnippet)

(unless (version-list-<= (version-to-list emacs-version) '(25 3 1))
  (use-package ivy-yasnippet
    :bind ("H-," . ivy-yasnippet)))

;; * Scimax packages
(use-package scimax
  :ensure nil
  :load-path scimax-dir
  :init (require 'scimax)
  :bind
  ("C-x C-b" . ibuffer))

(use-package scimax-mode
  :ensure nil
  :load-path scimax-dir
  :init (require 'scimax-mode)
  :config (scimax-mode))

(use-package scimax-org
  :ensure nil
  :load-path scimax-dir
  :bind
  ("s--" . org-subscript-region-or-point)
  ("s-=" . org-superscript-region-or-point)
  ("s-i" . org-italics-region-or-point)
  ("s-b" . org-bold-region-or-point)
  ("s-v" . org-verbatim-region-or-point)
  ("s-c" . org-code-region-or-point)
  ("s-u" . org-underline-region-or-point)
  ("s-+" . org-strikethrough-region-or-point)
  ("s-4" . org-latex-math-region-or-point)
  ("s-e" . ivy-insert-org-entity)
  :init
  (require 'scimax-org))

(use-package ox-clip
  :ensure nil
  :load-path (lambda () (expand-file-name "ox-clip" scimax-dir))
  :bind ("H-k" . ox-clip-formatted-copy))

(use-package scimax-email
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-notebook
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-utils
  :ensure nil
  :load-path scimax-dir
  :bind ( "<f9>" . hotspots))

(use-package bibtex-hotkeys
  :ensure nil
  :load-path scimax-dir)

(use-package ox-manuscript
  :ensure nil
  :load-path (lambda () (expand-file-name "ox-manuscript" scimax-dir)))

(use-package org-show
  :ensure nil
  :load-path (lambda () (expand-file-name "org-show" scimax-dir)))

;; (use-package techela
;;   :ensure nil
;;   :load-path (lambda () (expand-file-name "techela" scimax-dir)))

(use-package words
  :ensure nil
  :load-path scimax-dir
  :bind ("H-w" . words-hydra/body))

(use-package ore
  :ensure nil
  :load-path scimax-dir
  :bind ("H-o" . ore))

;; (use-package org-editmarks
;;   :ensure nil
;;   :load-path scimax-dir)

(use-package scimax-ivy
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-lob
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-yas
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-autoformat-abbrev
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-hydra
  :ensure nil
  :load-path scimax-dir
  :bind ("<f12>" . scimax/body))

(use-package scimax-journal
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-apps
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-ob
  :ensure nil
  :load-path scimax-dir)

(use-package kitchingroup
  :ensure nil
  :load-path scimax-dir)

;; (use-package ov-highlight
;;   :ensure nil
;;   :load-path (lambda () (expand-file-name "ov-highlight" scimax-dir))
;;   :bind ("H-h" . ov-highlight/body)
;;   :init
;;   (add-to-list 'load-path
;; 	       (expand-file-name "ov-highlight" scimax-dir))
;;   (require 'ov-highlight))

(org-babel-load-file (expand-file-name "scimax-editmarks.org" scimax-dir))

;; * User packages

;; We load one file: user.el

(when (and
       scimax-load-user-dir
       (file-exists-p (expand-file-name "user.el" scimax-user-dir)))
  (load (expand-file-name "user.el" scimax-user-dir)))

(add-to-list 'Info-directory-list scimax-dir)

;; * The end
(provide 'packages)

;;; packages.el ends here
