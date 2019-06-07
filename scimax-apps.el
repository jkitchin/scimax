;;; scimax-apps.el --- Library to open programs

;;; Commentary:
;;

;;;###autoload
(defun explorer (&optional path)
  "Open Finder or Windows Explorer in the current directory."
  (interactive (list (if (buffer-file-name)
			 (file-name-directory (buffer-file-name))
		       (expand-file-name  "~/"))))
  (cond
   ((string= system-type "gnu/linux")
    (shell-command "nautilus"))
   ((string= system-type "darwin")
    (shell-command (format "open -b com.apple.finder%s"
			   (if path (format " \"%s\""
					    (file-name-directory
					     (expand-file-name path))) ""))))
   ((string= system-type "windows-nt")
    (shell-command (format "explorer %s"
			   (replace-regexp-in-string
			    "/" "\\\\"
			    path))))))

(defalias 'finder 'explorer "Alias for `explorer'.")


(defun bash (&optional path)
  "Open a bash window.
PATH is optional, and defaults to the current directory."
  (interactive (list (if (buffer-file-name)
			 (file-name-directory (buffer-file-name))
		       (expand-file-name default-directory))))
  (cond
   ((string= system-type "gnu/linux")
    (shell-command "gnome-terminal"))
   ((string= system-type "darwin")
    (shell-command
     (format "open -b com.apple.terminal%s"
	     (if path (format " \"%s\"" path) ""))))
   ((string= system-type "windows-nt")
    (shell-command "start \"\" \"%SYSTEMDRIVE%\\Program Files\\Git\\bin\\bash.exe\" --login &"))))


(defun excel ()
  "Open Microsoft Excel."
  (interactive)
  (cond
   ((string= system-type "gnu/linux")
    (error "Excel is not on Linux."))
   ((string= system-type "darwin")
    (shell-command
     (shell-command "open -b com.microsoft.Excel")))
   ((string= system-type "windows-nt")
    (shell-command "start excel"))))


(defun word ()
  "Open Microsoft Word."
  (interactive)
  (cond
   ((string= system-type "gnu/linux")
    (error "Word is not on Linux."))
   ((string= system-type "darwin")
    (shell-command
     (shell-command "open -b com.microsoft.Word")))
   ((string= system-type "windows-nt")
    (shell-command "start winword"))))


(defun powerpoint ()
  "Open Microsoft Powerpoint."
  (interactive)
  (cond
   ((string= system-type "gnu/linux")
    (error "Powerpoint is not on Linux."))
   ((string= system-type "darwin")
    (shell-command
     (shell-command "open -b com.microsoft.Powerpoint")))
   ((string= system-type "windows-nt")
    (shell-command "start powerpnt"))))


(defun tweetdeck ()
  (interactive)
  (when (region-active-p)
    (kill-ring-save nil nil t))
  (browse-url "https://tweetdeck.twitter.com"))


(defun google ()
  "Open default browser to google.com."
  (interactive)
  (browse-url "http://google.com"))


(provide 'scimax-apps)

;;; scimax-apps.el ends here
