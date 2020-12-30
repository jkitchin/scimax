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

(when (version< emacs-version "25.0")
  (warn "You probably need at least Emacs 25. You should upgrade. You may need to install leuven-theme manually."))

;; remember this directory
(defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where the scimax is installed.")

(defvar scimax-user-dir (expand-file-name "user" scimax-dir)
  "User directory for personal code.")

(setq user-emacs-directory scimax-user-dir)

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

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

  (add-to-list
   'package-archives
   (cons "org" (concat proto "://orgmode.org/elpa/")))

  (when no-ssl
    (setq package-check-signature nil)
    (setq tls-program
  	  ;; Defaults:
  	  '("gnutls-cli --insecure -p %p %h"
  	    "gnutls-cli --insecure -p %p %h --protocols ssl3"
  	    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
  	  ;; '("gnutls-cli -p %p %h"
  	  ;;   "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof")
  	  ))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))



;; (if (file-directory-p (expand-file-name "emacs-win" scimax-dir))
;;     ;; Windows archives.
;;     (progn
;;       (add-to-list
;;        'package-archives
;;        '("org"         . "https://orgmode.org/elpa/"))

;;       ;; According to https://melpa.org/#/getting-started there may be issues with https on Windows
;;       (add-to-list
;;        'package-archives
;;        `("melpa" . "http://melpa.org/packages/")
;;        t))


;;   ;; For non-windows
;;   ;; replace the old http path with https.
;;   (setf (cdr (assoc "gnu" package-archives)) "https://elpa.gnu.org/packages")

;;   (add-to-list
;;    'package-archives
;;    '("org"         . "https://orgmode.org/elpa/"))

;;   ;; According to https://melpa.org/#/getting-started there may be issues with https on Windows
;;   (add-to-list
;;    'package-archives
;;    `("melpa" . "https://melpa.org/packages/")
;;    t))

(add-to-list 'load-path scimax-dir)
(add-to-list 'load-path scimax-user-dir)

(let ((default-directory scimax-dir))
  (shell-command "git submodule update --init"))

(set-language-environment "UTF-8")

(require 'bootstrap)
(require 'packages)

;; it appears this help library is not loaded fully in the emacs-win
;; directory. See issue #119. This appears to fix that.
(when (file-directory-p (expand-file-name "emacs-win" scimax-dir))
  (load-library "help"))

(setq gc-cons-threshold 800000)

(provide 'init)

;;; init.el ends here
