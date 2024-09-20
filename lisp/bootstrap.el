;;; bootstrap.el --- install use-package


;;; Commentary:
;;

;;; Code:
(package-initialize)

(when (memq system-type '(windows-nt ms-dos))
  ;; On windows this solves some issues checking package signatures in the gnu elpa.
  (setq package-check-signature nil))

(require 'gnutls)
(when (and (string= "26" (substring emacs-version 0 2))
	   (null gnutls-algorithm-priority)
           (not (memq system-type '(windows-nt ms-dos))))
  ;; This appears to be a bug in emacs 26 that prevents the gnu archive from being downloaded.
  ;; This solution is from https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(unless (file-exists-p (expand-file-name "archives/gnu/archive-contents" package-user-dir))
  (package-refresh-contents))

(unless package--initialized (package-initialize))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (when (and  (boundp 'scimax-package-refresh) scimax-package-refresh)
    (package-refresh-contents)))


(require 'diminish) ;; if you use :diminish

(require 'bind-key) ;; if you use any :bind variant

;; bootstrap straight
;; https://github.com/radian-software/straight.el#bootstrapping-straightel
;; adding to support using a git repo to install packages

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'bootstrap)

;;; bootstrap.el ends here
