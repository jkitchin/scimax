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


(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "user/bookmarks" scimax-dir)
	bookmark-save-flag 1))


(use-package button-lock)

;; Potential for commandline scripts using emacs
(use-package commander
  :disabled t)

(use-package drag-stuff)

(use-package swiper
  :bind
  ([remap isearch-forward] . counsel-grep-or-swiper)
  ("H-s" . swiper-all)
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package multiple-cursors
  :config
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
   ("H-c l" . counsel-load-library)
   ("H-c f" . counsel-git)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (progn
    (counsel-mode)

    ;; these are mostly for navigating directories with find file. I find it
    ;; convenient to use the right arrow to "go into" a directory, and the left
    ;; arrow or delete to go up a directory. I have commented out the left arrow
    ;; though so it can be used to go backwards into the input. This is not
    ;; critical though, the right-arrow does the same thing as S-ret below.

    ;; (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
    (define-key ivy-minibuffer-map (kbd "<right>") 'ivy-alt-done)

    ;; single candidate
    ;; default action on the current candidate
    (define-key ivy-minibuffer-map (kbd "M-<return>") #'ivy-dispatching-done)
    (define-key ivy-minibuffer-map (kbd "S-<return>") #'ivy-alt-done)

    ;; multiple candidates
    ;; C-RET call and go to next

    (defun scimax-ivy-default-action-continue ()
      "Apply default action and move to next/previous candidate."
      (interactive)
      (ivy-call)
      (ivy-next-line))

    (define-key ivy-minibuffer-map (kbd "C-<return>") #'scimax-ivy-default-action-continue)

    ;; choose action on current candidate and continue
    (define-key ivy-minibuffer-map (kbd "C-M-<return>") #'ivy-dispatching-call)

    ;; s-RET to quit (super)
    (defun scimax-ivy-exit-no-action ()
      "Exit with no action."
      (interactive)
      (ivy-exit-with-action
       (lambda (x) nil)))

    (define-key ivy-minibuffer-map (kbd "s-<return>") 'scimax-ivy-exit-no-action)

    (defun scimax-ivy-become ()
      "Change the command and reuse the current input.
    You will be prompted to enter a new key sequence which can be a
    shortcut or M-x. Then it will put the current input in the
    minibuffer for the command.

    Applications:
    1. start with swiper, C-M-b H-s to transfer the current input to swiper-all
    2. C-xC -b to switch-buffer, C-M-b C-x C-f to transfer input to find-file."
      (interactive)
      (let* ((input ivy-text)
             (transfer-input (lambda () (insert input))))
	(ivy-exit-with-action
	 (lambda (x)
	   (minibuffer-with-setup-hook
	       (:append transfer-input)
	     (call-interactively (key-binding (read-key-sequence "Switch to key sequence: ") t)))))))

    (define-key ivy-minibuffer-map (kbd "C-M-b") 'scimax-ivy-become)

    ;; Show keys
    (defun scimax-show-marked-candidates ()
      "Show keys in the map"
      (interactive)
      (describe-keymap ivy-minibuffer-map))

    (define-key ivy-minibuffer-map (kbd "C-h") #'scimax-show-marked-candidates)

    (defun scimax-ivy-show-marked-candidates ()
      "Show marked candidates"
      (interactive)
      (with-help-window "*ivy-marked-candidates*"
	(cl-loop for cand in ivy-marked-candidates
		 do
		 (princ (s-trim cand))
		 (princ "\n"))))

    (define-key ivy-minibuffer-map (kbd "C-s") #'scimax-ivy-show-marked-candidates)

    (defun scimax-ivy-comma-insert ()
      "Insert current candidate with comma separation. and clear minibuffer."
      (interactive)
      (let ((x (ivy-state-current ivy-last))
	    cand)
	(setq cand (cond
		    ;; x is a string, the only sensible thing is to insert it
		    ((stringp x)
		     x)
		    ;; x is a list where the first element is a string
		    ((and (listp x) (stringp (first x)))
		     (first x))
		    (t
		     (format "%S" x))))

	(with-ivy-window
	  (cond
	   ;; at beginning of line, looking back at space, or comma insert
	   ((or (bolp) (looking-back  " " 1) (looking-back  "," 1))
	    (insert cand))
	   ((word-at-point)
	    ;; jump to first space? first word?
	    (re-search-forward " " nil t)
	    (insert ", " cand))
	   (t
	    (insert ", " cand)))))
      (delete-minibuffer-contents)
      (setq ivy-text ""))

    (define-key ivy-minibuffer-map  ","  #'scimax-ivy-comma-insert)

    ;; Marking candidates
    (defun scimax-ivy-toggle-mark ()
      "Toggle the mark"
      (interactive)
      (if (ivy--marked-p)
	  (ivy-unmark)
	(ivy-mark))
      (ivy-previous-line))

    (define-key ivy-minibuffer-map (kbd "M-TAB")
      #'scimax-ivy-toggle-mark)))

(use-package counsel-projectile)

;; Provides functions for working on lists
(use-package dash)

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

(use-package flx)

(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))

;; google-this
(use-package google-this
  :config
  (google-this-mode 1))

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
  ("C-c pp" . counsel-projectile-switch-project)
  ("C-c pn" . counsel-projectile-switch-project-by-name)
  ("C-c pb" . counsel-projectile-switch-to-buffer)
  ("C-c pf" . counsel-projectile-find-file)
  ("C-c pd" . counsel-projectile-find-dir)
  ("C-c pg" . counsel-projectile-grep)
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
  :load-path (lambda () (expand-file-name "ox-clip" scimax-dir))
  :bind ("H-k" . ox-clip-formatted-copy))

(use-package scimax-contacts
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-email
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
  :load-path (lambda () (expand-file-name "kitchingroup" scimax-dir)))

(let ((enable-local-variables nil))
  (org-babel-load-file (expand-file-name "scimax-editmarks.org" scimax-dir)))

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
