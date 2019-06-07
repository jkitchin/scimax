(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list
   'package-archives
   (cons "org" (concat proto "://orgmode.org/elpa/")))

  (when no-ssl
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

(print (format "
==================================================================
system-type: %s
package-archive: %s
==================================================================
"
	       system-type
	       package-archives))


;; See what spell-checker there is, and if it is working.
(load-file "init.el")
(setenv "LANG" "C")
(message "%s: exit status = %s" ispell-program-name (shell-command ispell-program-name))
(message "%s" (shell-command-to-string ispell-program-name))
(message "\n\n-D output: %s" (shell-command-to-string (format "%s -D" ispell-program-name)))
(message "\n\n-d output: %s" (shell-command-to-string (format "%s -d" ispell-program-name)))
(message "LANG: %s" (getenv "LANG") )
