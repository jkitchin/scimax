;;; packages.el --- Install and configure scimax packages -*- lexical-binding: t -*-
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
(use-package org
  :straight t
  :ensure t
  :pin gnu
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
   ("M-<SPC>" . org-mark-ring-goto)
   ("H-." . org-time-stamp-inactive)))


;; * Other packages
(use-package diminish)

(use-package aggressive-indent
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; [2023-12-11 Mon] I don't think we use this for anything. commenting out for now.
;; see issue #487
;; (use-package auto-complete
;;   :diminish auto-complete-mode
;;   :config (ac-config-default))

(use-package avy)

;; May 24, 2017: this seems to be causing emacs 25.2 to be crashing on my linux box.
(unless (eq system-type 'gnu/linux)
  (use-package tex
    :ensure auctex))


(use-package bookmark
  :init
  (setq bookmark-save-flag 1))


(use-package button-lock)

;; Potential for commandline scripts using emacs
(use-package commander
  :disabled t)

(use-package drag-stuff)

(use-package swiper
  :bind 
  ("H-s" . swiper-all)
  :diminish ivy-mode
  :config
  (ivy-mode)
  (define-key global-map [remap isearch-forward]
    (if (executable-find "grep") 
	'counsel-grep-or-swiper
      'swiper)))

(use-package multiple-cursors
  :config
  ;; mc/cmds-to-run-once is defined in `lispy'.
  (add-to-list 'mc/cmds-to-run-once 'swiper-mc))

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
   ("C-x C-b" . counsel-ibuffer)
   ("C-x d" . counsel-dired)
   ("C-x C-f" . counsel-find-file)
   ("<f7>" . counsel-recentf)
   ("C-x f" . counsel-recentf)
   ("C-x l" . counsel-locate)
   ("C-x p" . counsel-projectile)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-info-lookup-symbol)
   ("C-c r" . ivy-resume)
   ("H-c r" . ivy-resume)
   ("s-r" . ivy-resume)
   ("H-r" . ivy-resume)
   ("H-c l" . counsel-load-library)
   ("H-c f" . counsel-find-library)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (counsel-mode))

(use-package ivy-avy)

(use-package counsel-projectile)

;; Provides functions for working on lists
(use-package dash)

(use-package dashboard)

(use-package elfeed)

;; Python editing mode
;; (use-package elpy			;
;;   :config
;;   (elpy-enable))

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

(use-package flx)

(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))

;; google-this
(use-package google-this
  :config
  (google-this-mode 1))

;; (use-package help-fns+
;;   :load-path scimax-dir)

;; Functions for working with hash tables
(use-package ht)

(use-package htmlize)

;; (use-package hy-mode)

(use-package hydra
  :init
  (setq hydra-is-helpful t)

  :config
  (require 'hydra-ox))

(use-package ivy-hydra)

;; (use-package jedi)

;; (use-package jedi-direx)

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

(use-package move-text
  :init (move-text-default-bindings))

;; Templating system
;; https://github.com/Wilfred/mustache.el
(use-package mustache)

(when (executable-find "jupyter")
  (use-package jupyter)
  (use-package scimax-jupyter :load-path scimax-dir))

(use-package ov)

(use-package pdf-tools)

(use-package parsebib)
(use-package helm)
(use-package helm-bibtex)
(use-package ivy-bibtex)
(use-package citeproc)

(use-package org-ref
  :init
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body))


(use-package org-ref-ivy
  :load-path (lambda () (file-name-directory (locate-library "org-ref")))
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	      org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	      org-ref-insert-label-function 'org-ref-insert-label-link
	      org-ref-insert-ref-function 'org-ref-insert-ref-link
	      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))

(use-package ox-pandoc)

;; https://github.com/bbatsov/projectile
(use-package projectile 
  :bind
  ("C-c pp" . counsel-projectile-switch-project)
  ("C-c pb" . counsel-projectile-switch-to-buffer)
  ("C-c pf" . counsel-projectile-find-file)
  ("C-c pd" . counsel-projectile-find-dir)
  ("C-c pg" . counsel-projectile-grep)
  ("C-c ph" . ivy-org-jump-to-project-headline)
  ("C-c pG" . counsel-projectile-git-grep)
  ("C-c pa" . counsel-projectile-ag)
  ("C-c pr" . counsel-projectile-rg)
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

(use-package ivy-yasnippet
  :bind ("H-," . ivy-yasnippet))

;; * Scimax packages
(use-package scimax
  :ensure nil
  :load-path scimax-dir
  :init (require 'scimax))

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
  ("s-\"" . org-double-quote-region-or-point)
  ("s-'" . org-single-quote-region-or-point)
  :init
  (require 'scimax-org))

(use-package ox-clip
  :ensure nil
  :bind ("H-k" . ox-clip-formatted-copy))

;; (use-package scimax-contacts
;;   :ensure nil
;;   :load-path scimax-dir)

(use-package scimax-email
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-projectile
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-spellcheck
  :ensure nil
  :load-path scimax-dir)

(org-babel-load-file (expand-file-name "scimax-notebook.org" scimax-dir))

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

(use-package words
  :ensure nil
  :load-path scimax-dir
  :bind ("H-w" . words-hydra/body))

(use-package ore
  :ensure nil
  :load-path scimax-dir
  :bind ("H-o" . ore))

(use-package scimax-ivy
  :ensure nil
  :load-path scimax-dir)

;; (use-package scimax-lob
;;   :ensure nil
;;   :load-path scimax-dir)

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

(let ((enable-local-variables nil))
  (org-babel-load-file (expand-file-name "scimax-editmarks.org" scimax-dir)))


(add-to-list 'Info-directory-list scimax-dir)

;; * The end
(provide 'packages)

;;; packages.el ends here
