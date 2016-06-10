;; Package for exporting a CMU cheme qualifier report
;; this code was heavily inspired by the ox-koma-letter.el code

;;; Define Back-End
(require 'ox)

(org-export-define-derived-backend 'cmu-qualifier 'latex
  :options-alist
  '((:abstract "ABSTRACT" nil ""))
  :translate-alist '((template . cmu-qualifier-template))
  :menu-entry
  '(?Q "Export with CMU qualifier report"
       ((?L "As LaTeX buffer" cmu-qualifier-export-as-latex)
	(?l "As LaTeX file" cmu-qualifier-export-to-latex)
	(?p "As PDF file" cmu-qualifier-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (cmu-qualifier-export-to-pdf t s v b)
		(org-open-file (cmu-qualifier-export-to-pdf nil s v b))))))))


(defun cmu-qualifier-template (contents info)
  "return complete document string for this export"
  (concat
   ;; Time-stamp.
   (and (plist-get info :time-stamp-file)
        (format-time-string "%% Created %Y-%m-%d %a %H:%M\n"))
   ;; Document class and packages.
   (let* ((class (plist-get info :latex-class))
	  (class-options (plist-get info :latex-class-options))
	  (header (nth 1 (assoc class org-latex-classes)))
	  (document-class-string
	   (and (stringp header)
		(if (not class-options) header
		  (replace-regexp-in-string
		   "^[ \t]*\\\\documentclass\\(\\(\\[[^]]*\\]\\)?\\)"
		   class-options header t nil 1)))))
     (if (not document-class-string)
	 (user-error "Unknown LaTeX class `%s'" class)
       (org-latex-guess-babel-language
	(org-latex-guess-inputenc
	 (org-element-normalize-string
	  (org-splice-latex-header
	   document-class-string
	   org-latex-default-packages-alist ; Defined in org.el.
	   org-latex-packages-alist nil     ; Defined in org.el.
	   (concat (org-element-normalize-string (plist-get info :latex-header))
		   (plist-get info :latex-header-extra)))))
	info)))

   ;; Now the core content
   (let ((signature-page (plist-get info :signature-page))
	 (acknowledgements (plist-get info :acknowledgements))
	 (abstract (plist-get info :abstract))
	 (author (plist-get info :author))
	 (title (plist-get info :title)))
     (concat 
   "
\\begin{document}
"
(format "\\title{%s}\n" (org-export-data title info))
(format "\\author{%s}\n" (org-export-data author info))
"
\\date{\\today}
"
(format "\\begin{abstract}\n%s\n\\end{abstract}\n" (org-export-data abstract info))
"
\\maketitle
\\thispagestyle{empty}
\\clearpage
\\setcounter{page}{1}
"
contents
"\n\\end{document}"
))))


;;;###autoload
(defun cmu-qualifier-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU MS report letter.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write content.

EXT-PLIST, when provided, is a proeprty list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CMU qualifier Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (cmu-qualifier-special-contents)
    (org-export-to-buffer 'cmu-qualifier "*Org CMU qualifier Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun cmu-qualifier-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU qualifier report (tex).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write contents.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

When optional argument PUB-DIR is set, use it as the publishing
directory.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep))
	(cmu-qualifier-special-contents))
    (org-export-to-file 'cmu-qualifier outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun cmu-qualifier-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU qualifier report (pdf).

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{letter}\" and \"\\end{letter}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((file (org-export-output-file-name ".tex" subtreep))
	(cmu-qualifier-special-contents))
    (org-export-to-file 'cmu-qualifier file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun cmu-qualifier-export-to-pdf-and-open 
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)

  (org-open-file (cmu-qualifier-export-to-pdf async subtreep visible-only body-only ext-plist)))

(provide 'ox-cmu-qualifier)
;;; ox-cmu-qualifier ends here
