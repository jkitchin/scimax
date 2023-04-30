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

(add-to-list 'load-path scimax-dir)

;; TODO: is this the right place for this? why not have this in user-space?
;; (setq package-user-dir (expand-file-name "elpa"  scimax-dir))

;; we load the preload.el file if it exists. This lets users define
;; variables that might affect packages when they are loaded, e.g. key-bindings

(let ((preload (expand-file-name (locate-user-emacs-file "preload.el"))))
  (when (file-exists-p preload)
    (load preload)))

(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  
  (when no-ssl
    (setq package-check-signature nil)
    (setq tls-program
  	  ;; Defaults:
  	  '("gnutls-cli --insecure -p %p %h"
  	    "gnutls-cli --insecure -p %p %h --protocols ssl3"
  	    "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))


(set-language-environment "UTF-8")

(require 'bootstrap)
(require 'packages)

(setq gc-cons-threshold 800000)

(provide 'init)

;;; init.el ends here
