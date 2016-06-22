;;; ox-rtf.el --- An org-mode exporter to RTF

;;; Commentary:
;; See http://orgmode.org/manual/Advanced-configuration.html
;;
;; for RTF see http://www.biblioscape.com/rtf15_spec.htm
;; and https://www.safaribooksonline.com/library/view/rtf-pocket-guide/9781449302047/ch01.html
;;
;; It isn't my intention to full support RTF export to arbitrily complex
;; documents, although I would like to get to where citations as footnotes are
;; reasonable, and cross-references work.

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
  (format "{\\pard %s\\par\\sa1}" (or contents "")))

(defun rtf-headline (hl contents info)
  (format
   "{\\b %s}\\par %s"
   (or (org-element-property :raw-value hl) "")  contents))

;; TODO: how to handle this fonttbl?
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
	 (with-temp-buffer
	   (insert code)
	   (shell-command-on-region
	    (point-min)
	    (point-max)
	    (format "pygmentize -f rtf -l %s" lang))
	   (with-current-buffer "*Shell Command Output*"
	     (buffer-string)))))))

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
   ;; org-ref citations. 
   ((-contains? org-ref-cite-types (org-element-property :type link))
    (let* ((path (org-element-property :path link)))
      (mapconcat (lambda (key)
		   (format "{\\super\\chftn}{\\footnote\\pard\\plain\\chftn %s.}"
			   (org-ref-get-bibtex-entry-citation key)))
		 (split-string path ",")
		 "{\\super ,}")))
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

(defun rtf-latex-fragment (ltx contents info)
  (let ((f (concat temporary-file-directory
		   "latex-fragment-"
		   (md5 (org-element-property :value ltx))
		   ".png")))
    (org-create-formula-image
     (org-element-property :value ltx)
     f
     '(:foreground default :background default
		   :scale 1.0 :html-foreground "Black"
		   :html-background "Transparent" :html-scale 1.0
		   :matchers
		   ("begin" "$1" "$" "$$" "\\(" "\\["))
     (current-buffer)
     org-latex-create-formula-image-program)
    (format "{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE  \\\\d
 \"%s\"
 \\\\* MERGEFORMATINET }}{\\fldrslt {  }}}"
	    f)))

;; This is not very sophisticated.
;; TODO, neighboring footnotes, formatting.
(defun rtf-footnote-reference (footnote-reference contents info)
  (let ((def (org-export-get-footnote-definition footnote-reference info)))
    (format "{\\super\\chftn}{\\footnote\\pard\\plain\\chftn %s}"
	    (org-export-data def info))))

(defun rtf-footnote-definition (footnote-definition contents info)
  "")

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
		     (link . rtf-link)
		     (latex-fragment . rtf-latex-fragment)
		     (footnote-reference . rtf-footnote-reference)
		     (footnote-definition . rtf-footnote-definition)))

;;;###autoload
(defun ox-rtf-formatted-copy (r1 r2)
  "Convert the selected region to RTF and put it on the clipboard."
  (interactive "r")
  ;; temporarily overwrite this template from ox-ascii
  (cl-flet ((org-ascii-inner-template (contents info) contents)) 
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
	(kill-buffer buf)))))

;; TODO: this isn't up to the other exporters standards
;;;###autoload
(defun ox-rtf-export-to-rtf-and-open ()
  "Export to RTF and open the result."
  (interactive)
  (ox-rtf-formatted-copy (point-min) (point-max))
  (let ((fname (file-name-sans-extension (buffer-file-name)))) 
    (with-temp-file fname
      (yank))
    (org-open-file fname)))


(provide 'ox-rtf)

;;; ox-rtf.el ends here
