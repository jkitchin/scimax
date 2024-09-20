;; Package for exporting an MS report
;; this code was heavily inspired by the ox-koma-letter.el code
;; the formatting is based on a report written by Prateek Mehta

;;; Define Back-End
(require 'ox)

(org-export-define-derived-backend 'cmu-ms-report 'latex
  :options-alist
  '((:signature-page "SIGNATURE_PAGE" nil nil)
    (:acknowledgements "ACKNOWLEDGEMENTS" nil nil)
    (:abstract "ABSTRACT" nil ""))
  :translate-alist '((template . cmu-ms-report-template))
  :menu-entry
  '(?m "Export with CMU MS report"
       ((?L "As LaTeX buffer" cmu-ms-report-export-as-latex)
	(?l "As LaTeX file" cmu-ms-report-export-to-latex)
	(?p "As PDF file" cmu-ms-report-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (cmu-ms-report-export-to-pdf t s v b)
		(org-open-file (cmu-ms-report-export-to-pdf nil s v b))))))))


(defun cmu-ms-report-template (contents info)
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
     ;; org-mode escapes these in the abstract. This is hackery to
     ;; undo it. It is probably not fail-proof
     (setq abstract (org-export-data abstract info))
     (setq abstract (replace-regexp-in-string "\\\\\\$" "$" abstract))
     (setq abstract (replace-regexp-in-string "\\\\{" "{" abstract))
     (setq abstract (replace-regexp-in-string "\\\\}" "}" abstract))
     (setq abstract (replace-regexp-in-string "\\\\_" "_" abstract))
     (setq abstract (replace-regexp-in-string "\\$\\\\backslash\\$" "\\\\" abstract))
     (concat
   "
\\begin{document}
\\thispagestyle{empty}
\\begin{titlepage}

    \\begin{center}
        \\vspace*{1cm}
        \\LARGE
        "
   ;; note the use of org-export-data, it did not work to just put title in here.
   (format "\\textbf{%s}" (org-export-data title info)) "

        \\vspace{2.5cm}
        \\large
        " (format "\\textbf{%s}" (org-export-data author info)) "

        \\vfill

        Submitted in partial fulfillment of the requirements for the degree of\\\\
        Master of Science

        \\vspace{0.8cm}

        \\includegraphics[scale=0.1]{cmu-seal}

        Department of Chemical Engineering\\\\
        Carnegie Mellon University\\\\
        Pittsburgh, PA, USA\\\\
        \\today

    \\end{center}
\\end{titlepage}


\\thispagestyle{empty}

\\raggedbottom

% scan your signature page and name it signature.pdf
"
	(when signature-page
	  (format "\\includepdf[pages=1]{%s}" (org-export-data signature-page info)))

(when acknowledgements
  (concat "\\section*{Acknowledgements}\n"
  (format "%s\n" (org-export-data acknowledgements info))))
"

\\newpage

\\tableofcontents
\\newpage

\\section*{Abstract}
"
abstract
"

\\newpage
\n\n"
contents
"\n\\end{document}"))))


;;;###autoload
(defun cmu-ms-report-export-as-latex
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

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org CMU MS Report Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (cmu-ms-report-special-contents)
    (org-export-to-buffer 'cmu-ms-report "*Org CMU MS Report Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun cmu-ms-report-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU MS report (tex).

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
	(cmu-ms-report-special-contents))
    (org-export-to-file 'cmu-ms-report outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun cmu-ms-report-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU MS report (pdf).

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
	(cmu-ms-report-special-contents))
    (org-export-to-file 'cmu-ms-report file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun cmu-ms-report-export-to-pdf-and-open
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)

  (org-open-file (cmu-ms-report-export-to-pdf async subtreep visible-only body-only ext-plist)))

(provide 'ox-cmu-ms-report)
;;; ox-cmu-ms-report ends here
