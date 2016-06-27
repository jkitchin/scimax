;;; ox-clip.el --- Formatted copy commands across platforms

;;; Commentary: This module copies selected regions in org-mode as formatted
;;; text on the clipboard that can be pasted into other applications.

;; For Windows you need this script:
;; https://github.com/jkitchin/scimax/blob/master/bin/html-clip-w32.py and to
;; set `ox-clip-w32-cmd' to the path to that script.

;; Mac OSX needs textutils and pbcopy, which should be installed.

;; Linux needs a relatively modern xclip.

;;; Code:

(defvar ox-clip-w32-cmd
  (expand-file-name "bin/html-clip-w32.py" scimax-dir)
  "Absolute path to html-clip-w32.py.")

(defun formatted-copy-win32 ()
  "Export region to html and copy to Windows clipboard."
  (interactive)
  (unless (file-exists-p ox-clip-w32-cmd)
    (error "You need to set `ox-clip-w32-cmd' to the absolute path to html-clip-w32.py"))
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         (format  "python %s" ox-clip-w32-cmd)))
      (kill-buffer buf))))


(defun formatted-copy-osx ()
  "Export region to HTML, convert to RTF and copy to Mac clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
	   (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
	(shell-command-on-region
	 (point-min)
	 (point-max)
	 "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))


(defun formatted-copy-linux ()
  "Export region to HTML and copy to Linux clipboard."
  (interactive)
  ;; from https://github.com/abo-abo/oremacs/blob/6c86696c0a1f66bf690e1a934683f85f04c6f34d/auto.el#L386
  (org-export-to-file 'html "/tmp/org.html" nil nil t t)
  (apply
   'start-process "xclip" "*xclip*"
   (split-string
    "xclip -verbose -i /tmp/org.html -t text/html -selection clipboard" " ")))


(defun formatted-copy ()
  "Export the selected region to HTML and copy it to the clipboard.
This just figures out your platform and runs the platform
dependent commands above."
  (interactive)
  (cond
   ((eq system-type 'windows-nt)
    (formatted-copy-win32))
   ((eq system-type 'darwin)
    formatted-copy-osx)
   ((eq system-type 'gnu/linux)
    (formatted-copy-linux))))






(provide 'ox-clip)

;;; ox-clip.el ends here
