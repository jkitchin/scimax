;;; ox-cmu-dissertation.el -- export for a CMU PhD dissertation

;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;

;;; Define Back-End
(require 'ox)

(add-to-list 'org-latex-classes
	     '("cmu-dissertation"                          ;class-name
	       "\\documentclass[12pt,letterpaper,oneside]{article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"        ; header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(org-export-define-derived-backend 'cmu-dissertation 'latex
  :options-alist
  '(
    ;; your undergraduate degrees
    (:priordegree "PRIORDEGREE" nil nil)
    (:degree "DEGREE" nil "Doctor of Philosophy")
    (:department "DEPARTMENT" nil "Department of Chemical Engineering")
    (:acknowledgements "ACKNOWLEDGEMENTS" nil nil)
    (:abstract "ABSTRACT" nil ""))
  :translate-alist '((template . cmu-dissertation-template))
  :menu-entry
  '(?d "Export with CMU dissertation"
       ((?L "As LaTeX buffer" cmu-dissertation-export-as-latex)
	(?l "As LaTeX file" cmu-dissertation-export-to-latex)
	(?p "As PDF file" cmu-dissertation-export-to-pdf)
	(?o "As PDF file and open"
	    (lambda (a s v b)
	      (if a (cmu-dissertation-export-to-pdf t s v b)
		(org-open-file (cmu-dissertation-export-to-pdf nil s v b))))))))

(defun cmu-dissertation-template (contents info)
  "return complete document string for export"
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

   (concat "
\\renewcommand{\\thefootnote}{\\alph{footnote}}

\\DeclareCaptionType{mycapequ}[][List of equations]
\\captionsetup[mycapequ]{labelformat=empty}
\\widowpenalty=10000
\\clubpenalty=10000
% \\raggedright
\\tolerance=1
\\emergencystretch=\\maxdimen
\\hyphenpenalty=10000
\\hbadness=10000
\\counterwithin{figure}{section}
\\counterwithin{equation}{section}
\\counterwithin{table}{section}
\\setlength{\\skip\\footins}{1cm} %space between body text and footnotes


\\begin{document}
\\pagenumbering{roman}
\\newpage\\thispagestyle{empty}

\\begin{center}
\\setstretch{1.667}
\\vspace*{15mm}
\\bf{" (org-export-data (plist-get info :title) info) "}

\\par\\vspace{15mm}

\\begin{center}
\\emph{Submitted in partial fulfillment of the requirements for\\\\ 
the degree of\\\\
"
(org-export-data (plist-get info :degree) info) "\\\\}
"
(org-export-data (plist-get info :department) info) "\\\\

\\par\\vspace{40mm}
" (org-export-data (plist-get info :author) info) "

\\singlespacing
" (org-export-data (plist-get info :priordegree) info) "
\\end{center}

\\vspace{40mm}
\\singlespacing
Carnegie Mellon University\\\\ 
Pittsburgh, Pennsylvania 

\\vspace{5mm}"
(org-export-data (plist-get info :date) info) "
\\end{center}

\\doublespacing
\\newpage
"
(when (plist-get info :acknowledgements)
  (concat
  "
\\begin{center}\\emph{Acknowledgments}
\\end{center}
" (org-export-data (plist-get info :acknowledgements) info)))
"
\\addcontentsline{toc}{section}{Abstract}

\\newpage
\\section*{Abstract}
" (format "%s" (plist-get info :abstract) info) "

\\newpage
\\singlespacing
\\tableofcontents{}
\\newpage
\\addcontentsline{toc}{section}{List of Tables}
\\listoftables{}
\\newpage
\\addcontentsline{toc}{section}{List of Figures}
\\listoffigures{}
\\newpage
\\mbox{}
\\newpage
\\clearpage
\\pagenumbering{arabic}
\\doublespacing
" contents "
\\end{document}
")))

;;;###autoload
(defun cmu-dissertation-export-as-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU dissertation letter.

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

Export is done in a buffer named \"*Org CMU dissertation Export*\".  It
will be displayed if `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (let (cmu-dissertation-special-contents)
    (org-export-to-buffer 'cmu-dissertation "*Org CMU Dissertation Export*"
      async subtreep visible-only body-only ext-plist
      (lambda () (LaTeX-mode)))))

;;;###autoload
(defun cmu-dissertation-export-to-latex
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU dissertation (tex).

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
	(cmu-dissertation-special-contents))
    (org-export-to-file 'cmu-dissertation outfile
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun cmu-dissertation-export-to-pdf
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a CMU Dissertation (pdf).

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
	(cmu-dissertation-special-contents))
    (org-export-to-file 'cmu-dissertation file
      async subtreep visible-only body-only ext-plist
      (lambda (file) (org-latex-compile file)))))

;;;###autoload
(defun cmu-dissertation-export-to-pdf-and-open 
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)

  (org-open-file (cmu-dissertation-export-to-pdf async subtreep visible-only body-only ext-plist)))

(provide 'ox-cmu-dissertation)
;;; ox-cmu-dissertation ends here
