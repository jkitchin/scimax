;;; ox-rtf.el --- An org-mode exporter to RTF

;;; Commentary:
;; 

(defun rtf-bold (bold contents info)
  (format "{\\b %s}" contents))

(defun rtf-italic (el contents info)
  (format "{\\i %s}" contents))

(defun rtf-underline (el contents info)
  (format "{\\ul %s}" contents))

(defun rtf-sub (el contents info)
  (format "{\\sub %s}" contents))

(defun rtf-super (el contents info)
  (format "{\\super %s}" contents))

(defun rtf-verbatim (el contents info)
  (format "{\\f2 %s}" (org-element-property :value el)))

(defun rtf-fixed-width (el contents info)
  (format "{\\f2 %s\\line}" (org-element-property :value el)))

(defun rtf-strike (el contents info)
  (format "{\\strike %s}" contents))

(defun rtf-paragraph (el contents info)
  (format "{\\pard %s\\par\\sa1}" contents))

(defun rtf-headline (hl contents info)
  (format
   "{\\b %s}\\par %s"
   (org-element-property :raw-value hl) contents))

(defun rtf-src (src contents info)
  (let ((lang (org-element-property :language src))
	(code (org-element-property :value src)))
    (with-temp-file "rtf-exporter" (insert code))
    (prog1
	(format
	 "%s
{\\fonttbl
{\\f0\\froman Times;}
{\\f1\\fswiss Arial;}
{\\f2\\fmodern Courier New;}}\\f0"
	 (shell-command-to-string     
	  (format "highlight -O rtf --src-lang=python rtf-exporter" lang)))
      (delete-file "rtf-exporter"))))

(defun rtf-link (link contents info)
  (cond
   ((-contains? '("http" "https") (org-element-property :type link))
    (format "{\\field{\\*\\fldinst{HYPERLINK \"%s\"}}{\\fldrslt{\\ul %s}}}" (org-element-property :raw-link link)
	    (if (org-element-property :contents-begin link)
		(buffer-substring (org-element-property :contents-begin link)
				  (org-element-property :contents-end link))
	      (org-element-property :raw-link link))))
   ((and (string= "file" (org-element-property :type link))
	 (-contains? '("png") (file-name-extension (org-element-property :path link))))
    (format "{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE  \\\\d
 \"%s\"
 \\\\* MERGEFORMATINET }}{\\fldrslt {  }}}"
	    (expand-file-name (org-element-property :path link))))
   (t (org-element-property :raw-link link))))

(defun rtf-table (tbl contents info)
  "Hacky interim solution. Making tables looks hard."
  (mapconcat (lambda (row) (format "{\\f1 %s}\\line" row))
	     (split-string 
	      (buffer-substring
	       (org-element-property :contents-begin tbl)
	       (org-element-property :contents-end tbl))
	      "\n")
	     "\n"))

(org-export-define-derived-backend 'RTF 'ascii
  :translate-alist '((bold . rtf-bold)
		     (italic . rtf-italic)
		     (underline . rtf-underline)
		     (superscript . rtf-super)
		     (subscript . rtf-sub)
		     (verbatim . rtf-verbatim)
		     (strike-through . rtf-strike)
		     (paragraph . rtf-paragraph)
		     (headline . rtf-headline)
		     (src-block . rtf-src)
		     (table . rtf-table) 
		     (fixed-width . rtf-fixed-width)
		     (link . rtf-link)))

;;;###autoload
(defun ox-rtf-formatted-copy (r1 r2)
  "Convert the selected region to RTF and put it on the clipboard."
  (interactive "r")
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'RTF "*Org RTF Export*" nil nil t t))
	   (rtf (with-current-buffer buf (buffer-string))))
      (with-temp-buffer
	(insert (format "{\\rtf1\\ansi\\deff0
{\\fonttbl
{\\f0\\froman Times;}
{\\f1\\fswiss Arial;}
{\\f2\\fmodern Courier New;}}
%s}" rtf))
	(kill-new (buffer-string))
	(shell-command-on-region (point-min) (point-max) "pbcopy")) 
      (kill-buffer buf))))

;;;###autoload
(defun ox-rtf-export-to-rtf (&optional async subtreep visible-only body-only ext-plist)
  "Export to RTF."
  (interactive)
  (ox-rtf-formatted-copy (point-min) (point-max))
  (let ((fname (file-name-sans-extension (buffer-file-name)))) 
    (with-temp-file fname
      (yank))
    (org-open-file fname)))


(provide 'ox-rtf)

;;; ox-rtf.el ends here
