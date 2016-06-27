;; * Copy formatted text to clipboards

(defvar ox-clip-w32-cmd
  (expand-file-name "bin/html-clip-w32.py" scimax-dir)
  "Absolute path to html-clip-w32.py.")

(defun formatted-copy-win32 ()
  "Export region to html and copy to Windows clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "python html-clip-w32.py")) 
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

;; (defun formatted-copy (r1 r2)
;;   "Copy region to clipboard.
;; When in org-mode and on a Mac, copy an RTF version for pasting too.
;; When in org-mode and on Linux, copy an HTML version for pasting."
;;   (interactive "r")
;;   ;; The regular copy command
;;   (kill-ring-save r1 r2)

;;   ;; now a fancy copy for org-mode
;;   (when
;;       (and (eq major-mode 'org-mode)
;; 	   (not (org-at-comment-p)) ) 
;;     (cond
;;      ((eq system-type 'darwin)
;;       (save-window-excursion
;; 	(let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
;; 	       (html (with-current-buffer buf (buffer-string))))
;; 	  (with-current-buffer buf
;; 	    (shell-command-on-region
;; 	     (point-min)
;; 	     (point-max)
;; 	     "textutil -stdin -format html -convert rtf -stdout | pbcopy")) 
;; 	  (kill-buffer buf))))
;;      ((eq system-type 'gnu/linux)
;;       ))))


