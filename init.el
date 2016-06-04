;;; init.el --- Where all the magic begins
;;

;;; Commentary:
;;
;; This is a starter kit for scimax. This package provides a
;; customized setup for emacs that we use daily for scientific
;; programming and publication.
;;

;;; Code:

(when (version< emacs-version "24.4")
  (warn "You probably need Emacs 24.4. You should upgrade. You may need to install leuven-theme manually."))

;; remember this directory
(defconst scimax-dir (file-name-directory (or load-file-name (buffer-file-name)))
    "Directory where the scimax is installed.")

(defvar user-dir (expand-file-name "user" scimax-dir)
  "User directory for personal code.")

(setq package-user-dir (expand-file-name "elpa"  scimax-dir))

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")))

(add-to-list 'load-path scimax-dir)
(add-to-list 'load-path user-dir)

(require 'bootstrap)
(require 'packages)
(require 'scimax)
(require 'scimax-org)

(provide 'init)

;;; init.el ends here
