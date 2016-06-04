;;; packages.el --- Install and configure scimax packages

;;; Commentary:
;; see https://github.com/jwiegley/use-package
;; These are packages that should get installed from an external repo.

(setq use-package-always-ensure t)

(add-to-list 'package-archives
	     '("org"         . "http://orgmode.org/elpa/"))

(add-to-list 'package-archives
	     '("gnu"         . "http://elpa.gnu.org/packages/"))

(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :init
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window)

  ;; I like to press enter to follow a link. mouse clicks also work.
  (setq org-return-follows-link t)
  
  :bind
  (("C-c l" . org-store-link)
   ("C-c L" . org-insert-link-global)
   ("C-c o" . org-open-at-point-global)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))

(use-package aggressive-indent
  :config (aggressive-indent-global-mode 1))

;; (use-package auctex)

(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "user/bookmarks" scimax-dir)
      bookmark-save-flag 1))

(use-package counsel
  :init (setq projectile-completion-system 'ivy)
  :bind
  ("C-c r" . ivy-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)

  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("C-h i" . counsel-info-lookup-symbol)
  ("C-c g" . counsel-git))

;; Provides functions for working on lists
(use-package dash)

(use-package elfeed)

(use-package elpy)

;; Provides functions for working with files
(use-package f)

;; (use-package helm
;;   :init (setq helm-command-prefix-key "C-c h")
;;   :bind ("<f7>" . helm-recentf)
;;   ("M-x" . helm-M-x)
;;   ("M-y" . helm-show-kill-ring)
;;   ("C-x b" . helm-mini)
;;   ("C-x C-f" . helm-find-files)
;;   ("C-h C-f" . helm-apropos)
;;   :config
;;   (add-hook 'helm-find-files-before-init-hook
;; 	    (lambda ()

;; 	      (helm-add-action-to-source
;; 	       "Insert path"
;; 	       (lambda (target)
;; 		 (insert (file-relative-name target)))
;; 	       helm-source-find-files)

;; 	      (helm-add-action-to-source
;; 	       "Insert absolute path"
;; 	       (lambda (target)
;; 		 (insert (expand-file-name target)))
;; 	       helm-source-find-files)

;; 	      (helm-add-action-to-source
;; 	       "Attach file to email"
;; 	       (lambda (candidate)
;; 		 (mml-attach-file candidate)) 
;; 	       helm-source-find-files)

;; 	      (helm-add-action-to-source
;; 	       "Make directory"
;; 	       (lambda (target)
;; 		 (make-directory target))
;; 	       helm-source-find-files))))


(use-package helm-bibtex)

(use-package helm-projectile)

;; Functions for working with hash tables
(use-package ht)

(use-package hydra)

(use-package jedi)

(use-package jedi-direx)

;; Superior lisp editing
(use-package lispy
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (lispy-mode)
	      (eldoc-mode)))
  (add-hook 'python-mode-hook
	    (lambda ()
	      (lispy-mode)
	      (eldoc-mode))))

(use-package magit
  :bind ("<f5>" . magit-status))

(use-package ob-ipython)

(use-package org-ref
  :config
  (require 'doi-utils)
  (require 'org-ref-isbn)
  (require 'org-ref-pubmed)
  (require 'org-ref-arxiv)
  (require 'org-ref-bibtex)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils))

(use-package projectile)

(use-package pydoc)

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package rainbow-mode)

(use-package recentf
  :init (setq recentf-max-saved-items 200
	      recentf-max-menu-items 15)
  :config (recentf-mode +1))

;; Functions for working with strings
(use-package s)

(use-package swiper
  :bind
  ("C-s" . swiper)
  :config
  (ivy-mode))

(use-package undo-tree
  :config (global-undo-tree-mode))

(provide 'packages)

;;; packages.el ends here
