;;; bootstrap.el --- install use-package


;;; Commentary:
;;

;;; Code:
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

(provide 'bootstrap)

;;; bootstrap.el ends here
