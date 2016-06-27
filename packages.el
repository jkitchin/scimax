;;; packages.el --- Install and configure scimax packages

;;; Commentary:
;; see https://github.com/jwiegley/use-package
;; These are packages that should get installed from an external repo.

(setq use-package-always-ensure t)

(add-to-list 'package-archives
	     '("org"         . "http://orgmode.org/elpa/"))

(add-to-list 'package-archives
	     '("gnu"         . "https://elpa.gnu.org/packages/"))

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

(use-package el-get
  :init
  (setq el-get-bundle-init-directory (file-name-as-directory
				      (expand-file-name "el-get" scimax-dir))))

;; * Other packages

(use-package aggressive-indent
  :config (aggressive-indent-global-mode 1))

(use-package auto-complete
  :diminish auto-complete-mode
  :config (ac-config-default))

(use-package avy)

;; installation is failing
;; (use-package auctex)

;; Make cursor more visible when you move a long distance
(use-package beacon
  :config
  (beacon-mode 1))


(use-package bookmark
  :init
  (setq bookmark-default-file (expand-file-name "user/bookmarks" scimax-dir)
	bookmark-save-flag 1))


(use-package bookmark+
  ;; I am not currently using this, and it loads a bunch of files on startup.
  :disabled t)

;; criticmarks
;; This is my fork of cm-mode
(el-get-bundle jkitchin/criticmarkup-emacs)


(use-package commander)


(use-package swiper
  :bind
  ("C-s" . counsel-grep-or-swiper)
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

  (setf (cdr (assoc t ivy-re-builders-alist))
	'ivy--regex-ignore-order)
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . ivy-switch-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h i" . counsel-info-lookup-symbol)
   ("H-c r" . ivy-resume)
   ("H-c l" . counsel-load-library)
   ("H-c g" . counsel-git-grep)
   ("H-c a" . counsel-ag)
   ("H-c p" . counsel-pt))
  :diminish ""
  :config
  (progn
    (counsel-mode)
    
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
    
    (define-key ivy-minibuffer-map (kbd "?")
      (lambda ()
	(interactive)
	(describe-keymap ivy-minibuffer-map)))
    
    (define-key ivy-minibuffer-map (kbd "<left>") 'ivy-backward-delete-char)
    
    (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-backward-delete-char)
    
    (ivy-set-actions
     t
     '(("i" (lambda (x) (with-ivy-window
			  (insert x))) "insert candidate")
       (" " (lambda (x) (ivy-resume)) "resume")
       ("?" (lambda (x)
	      (interactive)
	      (describe-keymap ivy-minibuffer-map)) "Describe keys")))
    
    (ivy-add-actions
     'counsel-find-file
     '(("a" (lambda (x)
	      (unless (memq major-mode '(mu4e-compose-mode message-mode))
		(compose-mail)) 
	      (mml-attach-file x)) "Attach to email")
       ("c" (lambda (x) (kill-new (f-relative x))) "Copy relative path")
       ("4" (lambda (x) (find-file-other-window x)) "Open in new window")
       ("5" (lambda (x) (find-file-other-frame x)) "Open in new frame")
       ("C" (lambda (x) (kill-new x)) "Copy absolute path")
       ("d" (lambda (x) (dired x)) "Open in dired")
       ("D" (lambda (x) (delete-file x)) "Delete file")
       ("e" (lambda (x) (shell-command (format "open %s" x)))
	"Open in external program")
       ("f" (lambda (x)
	      "Open X in another frame."
	      (find-file-other-frame x))
	"Open in new frame")
       ("p" (lambda (path)
	      (with-ivy-window
		(insert (f-relative path))))
	"Insert relative path")
       ("P" (lambda (path)
	      (with-ivy-window
		(insert path)))
	"Insert absolute path")
       ("l" (lambda (path)
	      "Insert org-link with relative path"
	      (with-ivy-window
		(insert (format "[[file:%s]]" (f-relative path)))))
	"Insert org-link (rel. path)")
       ("L" (lambda (path)
	      "Insert org-link with absolute path"
	      (with-ivy-window
		(insert (format "[[file:%s]]" path))))
	"Insert org-link (abs. path)")
       ("r" (lambda (path)
	      (rename-file path (read-string "New name: ")))
	"Rename")))))

;; Provides functions for working on lists
(use-package dash)

(use-package elfeed)

;; Python editing mode
(use-package elpy)

;; Provides functions for working with files
(use-package f)

;; https://github.com/amperser/proselint
;; pip install proselint
(use-package flycheck
  :disabled t
  :config
  (flycheck-define-checker
      proselint
    "A linter for prose."
    :command ("proselint" source-inplace)
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": "
	      (id (one-or-more (not (any " "))))
	      (message (one-or-more not-newline)
		       (zero-or-more "\n" (any " ") (one-or-more not-newline)))
	      line-end))
    :modes (text-mode org-mode))
  (add-to-list 'flycheck-checkers 'proselint)
  (unless (executable-find "proselint")
    (shell-command "pip install proselint"))
  
  (add-hook 'text-mode-hook #'flycheck-mode)
  (add-hook 'org-mode-hook #'flycheck-mode)
  (define-key flycheck-mode-map (kbd "s-;") 'flycheck-previous-error))


;; https://manuel-uberti.github.io/emacs/2016/06/06/spellchecksetup/
(use-package flyspell-correct-ivy
  :ensure t
  :init (setq ispell-program-name (executable-find "hunspell")
	      ispell-dictionary "en_US"
	      flyspell-correct-interface 'flyspell-correct-ivy) 
  :after flyspell 
  :config 
  (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic) 
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
	      (flyspell-mode)
	      (flycheck-mode))))


(use-package flx)

(use-package git-messenger
  :bind ("C-x v o" . git-messenger:popup-message))

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

(use-package help-fns+)

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
  :after swiper
  :config
  (dolist (hook '(emacs-lisp-mode-hook
		  hy-mode-hook
		  python-mode-hook))
    (add-hook hook
	      (lambda ()
		(lispy-mode)
		(eldoc-mode)))))

(use-package magit
  :init (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("<f5>" . magit-status)
  ("C-c v t" . magit-status))

;; https://github.com/Wilfred/mustache.el
(use-package mustache)


(use-package ob-ipython
  :disabled t
  :config
  (defun ob-ipython--kernel-repl-cmd (name)
    (list "jupyter" "console" "--existing" (format "emacs-%s.json" name)))
  ;; Make sure pygments can handle ipython for exporting.
  (unless (= 0 (shell-command "python -c \"import pygments.lexers; pygments.lexers.get_lexer_by_name('ipython')\""))
    (shell-command "pip install git+git://github.com/sanguineturtle/pygments-ipython-console")))

(el-get-bundle jkitchin/ob-ipython
  ;; Make sure pygments can handle ipython for exporting.
  (unless (= 0 (shell-command "python -c \"import pygments.lexers; pygments.lexers.get_lexer_by_name('ipython')\""))
    (shell-command "pip install git+git://github.com/sanguineturtle/pygments-ipython-console"))
  (require 'ob-ipython))


(use-package org-ref
  :ensure nil
  :load-path "/Users/jkitchin/vc/jkitchin-github/org-ref/"
  :config
  (require 'doi-utils)
  (require 'org-ref-isbn)
  (require 'org-ref-pubmed)
  (require 'org-ref-arxiv)
  (require 'org-ref-bibtex)
  (require 'org-ref-pdf)
  (require 'org-ref-url-utils)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  (global-set-key (kbd "H-b") 'org-ref-bibtex-hydra/body))

;; Bleeding edge org-ref from github.
;; (el-get-bundle jkitchin/org-ref
;;   (require 'doi-utils)
;;   (require 'org-ref-isbn)
;;   (require 'org-ref-pubmed)
;;   (require 'org-ref-arxiv)
;;   (require 'org-ref-bibtex)
;;   (require 'org-ref-pdf)
;;   (require 'org-ref-url-utils)
;;   (setq bibtex-autokey-year-length 4
;; 	bibtex-autokey-name-year-separator "-"
;; 	bibtex-autokey-year-title-separator "-"
;; 	bibtex-autokey-titleword-separator "-"
;; 	bibtex-autokey-titlewords 2
;; 	bibtex-autokey-titlewords-stretch 1
;; 	bibtex-autokey-titleword-length 5)
;;   (global-set-key (kbd "H-b") 'org-ref-bibtex-hydra/body))

;; This is the MELPA version
;; (use-package org-ref
;;   :disabled t
;;   :init
;;   :bind ("H-b" . org-ref-bibtex-hydra/body))

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
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-global-mode))

(use-package pydoc)

(use-package rainbow-mode)

(use-package recentf
  :config
  (setq recentf-exclude
        '("COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
          ".*png$" "\\*message\\*"))
  (setq recentf-max-saved-items 60))


;; Functions for working with strings
(use-package s)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'light)
  (sml/setup))

(use-package smart-mode-line-powerline-theme
  :disabled t)

;; keep recent commands available in M-x
(use-package smex)

(use-package undo-tree
  :diminish undo-tree-mode 
  :config (global-undo-tree-mode))


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
  :init
  (require 'scimax-org))

(use-package ox-rtf
  :ensure nil
  :load-path scimax-dir
  :bind ("H-k" . ox-rtf-formatted-copy))

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

(let ((path (expand-file-name "ox-manuscript" scimax-dir)))
  (use-package ox-manuscript
    :ensure nil
    :load-path path))

(let ((path (expand-file-name "org-show" scimax-dir)))
  (use-package org-show
    :ensure nil
    :load-path path))

(use-package words
  :ensure nil
  :load-path scimax-dir
  :bind ("H-w" . words-hydra/body))

(use-package ore
  :ensure nil
  :load-path scimax-dir
  :bind ("H-o" . ore))

(use-package cm-mods
  :ensure nil
  :load-path scimax-dir)


;; * User packages

;; We load one file: user.el

(when (and
       scimax-load-user-dir
       (file-exists-p (expand-file-name "user.el" user-dir)))
  (load (expand-file-name "user.el" user-dir)))

;; * The end
(provide 'packages)

;;; packages.el ends here
