;;; scimax-utils.el --- Utility functions scimax cannot live without

;;; Commentary:
;; 

;;; Code:

;; * Hotspots
(defcustom scimax-user-hotspot-commands '()
  "A-list of hotspots to jump to in `hotspots'.
These are shortcut to commands.
\(\"label\" . command)")

(defcustom scimax-user-hotspot-locations '()
  "A-list of hotspot locations to jump to in  `hotspots'.
\(\"label\" . \"Path to file\").

These are like bookmarks.")


;;;###autoload
(defun hotspots (arg)
  "Helm interface to hotspot locations.
This includes user defined
commands (`scimax-user-hotspot-commands'),
locations (`scimax-user-hotspot-locations'), org agenda files,
recent files and bookmarks. You can set a bookmark also."
  (interactive "P")
  (helm :sources `(((name . "Commands")
		    (candidates . ,scimax-user-hotspot-commands)
		    (action . (("Open" . (lambda (x) (funcall x))))))
		   ((name . "My Locations")
		    (candidates . ,scimax-user-hotspot-locations)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   ((name . "My org files")
		    (candidates . ,org-agenda-files)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   helm-source-recentf
		   helm-source-bookmarks
		   helm-source-bookmark-set)))


;;;###autoload
(defun scimax-help ()
  "Open the ‘scimax’ manual."
  (interactive)
  (find-file (expand-file-name
              "scimax.org"
	      scimax-dir)))

;; * utilities
;;;###autoload
(defun kill-all-buffers ()
  "Kill all buffers.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))


;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers but this one.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer) (buffer-list)))
  (delete-other-windows))


;;;###autoload
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

;; * Version control
;; Some new bindings to add to vc-prefix-map
(define-key 'vc-prefix-map "t" 'magit-status)

;;;###autoload
(defun vc-git-push ()
  "Run git push."
  (interactive)
  (shell-command "git push"))

;;;###autoload
(defun vc-git-pull (&optional arg)
  "Run git pull"
  (interactive)
  (shell-command "git pull"))

(define-key 'vc-prefix-map "p" 'vc-git-push)
(define-key 'vc-prefix-map "P" 'vc-pull)

;; * Windows
;;;###autoload
(defun explorer ()
  "Open Finder or Windows Explorer in the current directory."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder %s"
			   (if (buffer-file-name)
			       (file-name-directory (buffer-file-name))
			     "~/"))))
   ((string= system-type "windows-nt")
    (shell-command (format "explorer %s"
			   (replace-regexp-in-string
			    "/" "\\\\"
			    (if (buffer-file-name)
				(file-name-directory (buffer-file-name))
			      (expand-file-name  "~/"))))))))

(defalias 'finder 'explorer "Alias for `explorer'.")


(defun bash ()
  "Open a bash window."
  (interactive)
  (cond
   ((string= system-type "darwin")
    (shell-command
     (format "open -b com.apple.terminal \"%s\""
	     (if (buffer-file-name)
		 (file-name-directory (buffer-file-name))
	       (expand-file-name default-directory)))))
   ((string= system-type "windows-nt")
    (shell-command "start \"\" \"%SYSTEMDRIVE%\\Program Files\\Git\\bin\\bash.exe\" --login"))))


;; * The end
(provide 'scimax-utils)

;;; scimax-utils.el ends here
