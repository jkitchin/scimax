;;; Define Back-End
(require 'ox)

(add-to-list 'org-latex-classes
	     '("cmu-memo"                          ;class-name
	       "\\documentclass{letter}
                [NO-DEFAULT-PACKAGES]
                [PACKAGES]
                [EXTRA]"        ; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-export-define-derived-backend 'cmu-memo 'latex
  :options-alist
  '((:department "DEPARTMENT" nil "Department of Chemical Engineering")
    ;; the name is a committee or your name
    (:fromname "FROMNAME" nil nil)
    ;; I am leaving out proftitle, location, telephone, email, and zipcode.
    (:to "TO" nil nil)
    (:from "FROM" nil nil)
    (:subject "SUBJECT" nil nil)
    (:cc "CC" nil "")
    (:signature-lines "SIGNATURE-LINES" nil t))
  :translate-alist '((template . cmu-memo-template))
  :menu-entry
  '(?M "Export with CMU Memo"
       ((?L "As LaTeX buffer" cmu-memo-export-as-latex)
	(?l "As LaTeX file" cmu-memo-export-to-latex)
	(?p "As PDF file" cmu-memo-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (cmu-memo-export-to-pdf t s v b)
		(org-open-file (cmu-memo-export-to-pdf nil s v b))))))))

(defun cmu-memo-template (contents info)
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
   (let ((to (plist-get info :to))
	 (from (plist-get info :from))
	 (subject (plist-get info :subject))
	 (fromname (plist-get info :fromname))
	 (cc (plist-get info :cc)))
     (concat "
\\usepackage[color-logo]{cmumemo}
\\usepackage{charter}
\\newcommand{\\section}[1]{{{\\bfseries #1}: }}


\\begin{document}
\\begin{letter}{}
\\NAME{" (org-export-data fromname info) "}
\\TO{" (org-export-data to info) "}
\\FROM{" (org-export-data from info) "}
\\SUBJECT{" (org-export-data subject info) "}
"
(when cc (concat "\\CC{" (org-export-data cc info) "}"))
"
\\opening{}
"
contents

(unless (string= "nil" (plist-get info :signature-lines))
  "
\\signaturelines
")

"
\\end{letter}
\\end{document}
"))))

;;;###autoload
(defun cmu-memo-export-as-latex
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
  (let (cmu-memo-special-contents)
    (org-export-to-buffer 'cmu-memo "*Org CMU memo Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun cmu-memo-export-to-latex
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
	(cmu-memo-special-contents))
    (org-export-to-file 'cmu-memo outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun cmu-memo-export-to-pdf
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
	(cmu-memo-special-contents))
    (org-export-to-file 'cmu-memo file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun cmu-memo-export-to-pdf-and-open
    (&optional async subtreep visible-only body-only ext-plist)
  "Export a memo to PDF and open it.

TEXINPUTS is augmented with the path to the cmumemo style file."
  (interactive)
  ;; I think on windows ; is used for separators. On Mac/Linux : is used.
  
  (let* ((separator (pcase system-type
		      ('windows-nt ";")
		      (_ ":")))
	 (texinputs (getenv "TEXINPUTS"))
	 (*TEXINPUTS* (format "TEXINPUTS=%s%s%s"
			      (if texinputs
				  (concat texinputs separator)
				"")
			      (expand-file-name "tex/latex/cmu/"
						(file-name-directory
						 (locate-library "ox-cmu-memo")))
			      separator))
	 (process-environment (cons *TEXINPUTS* process-environment)))
    (org-open-file (cmu-memo-export-to-pdf async subtreep visible-only body-only ext-plist))))

(provide 'ox-cmu-memo)
;;; ox-cmu-memo ends here
