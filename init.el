;;; init.el --- Where all the magic begins
;;

;;; Commentary:
;;
;; This is a starter kit for scimax. This package provides a
;; customized setup for emacs that we use daily for scientific
;; programming and publication.
;;

;;; Code:
;; this makes garbage collection less frequent, which speeds up init by about 2 seconds.
(setq gc-cons-threshold 80000000)

(when (version< emacs-version "24.4")
  (warn "You probably need at least Emacs 24.4. You should upgrade. You may need to install leuven-theme manually."))

;; remember this directory
(defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where the scimax is installed.")

(defvar user-dir (expand-file-name "user" scimax-dir)
  "User directory for personal code.")

(setq user-emacs-directory user-dir)

(setq package-user-dir (expand-file-name "elpa"  scimax-dir))

;; we load the user/preload.el file if it exists. This lets users define
;; variables that might affect packages when they are loaded, e.g. key-bindings,
;; etc... setup autoupdate, .

(let ((preload (expand-file-name "user/preload.el" scimax-dir)))
  (when (file-exists-p preload)
    (load preload)))

(defvar scimax-load-user-dir t
  "Controls if the user directory is loaded.")

(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(add-to-list
 'package-archives
 '("org"         . "http://orgmode.org/elpa/")
 t)

(add-to-list 'load-path scimax-dir)
(add-to-list 'load-path user-dir)

(let ((default-directory scimax-dir))
  (shell-command "git submodule update --init"))

(require 'bootstrap)
(require 'packages)

(provide 'init)

;;; init.el ends here
