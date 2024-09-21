;;; ox-manuscript.el -- utilities to export scientific manuscripts,

;; Copyright(C) 2014-2021 John Kitchin

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
;; Provides the export menu and setup for the scientific manuscripts
;; we write.


;; important functions
;; ox-manuscript-export-and-build
;; ox-manuscript-export-and-build-and-open
;; ox-manuscript-build-submission-manuscript
;; ox-manuscript-build-submission-manuscript-and-open
;; ox-manuscript-export-and-build-and-email

;;; Code:

(require 'ox)
(require 'ox-publish)

;; * Custom variables

(defgroup ox-manuscript nil
  "Customization group for ox-manuscript.")

(defcustom ox-manuscript-latex-command
  "lualatex"
  "Command to run latex."
  :group 'ox-manuscript)

(defcustom ox-manuscript-bibtex-command
  "bibtex8"
  "Command to run bibtex."
  :group 'ox-manuscript)

(defcustom ox-manuscript-page-numbering
  ""
  "Command to influence page-numbering.
Set it to \"\\pagenumbering{gobble}\n\" if you want no page numbers."
  :group 'ox-manuscript
  :type 'string)

(defcustom ox-manuscript-interactive-build
  nil
  "Determines if pdfs are built with interaction from the user.
nil means just build without user interaction.  Anything else will
show the user a window of the results of each build step, and ask
if you should continue to the next step."
  :group 'ox-manuscript)

(defcustom ox-manuscript-user-template-dir
  (file-name-as-directory
   (expand-file-name
    (locate-user-emacs-file "ox-manuscript-templates")))
  "Directory for user-defined ox-manuscript templates."
  :group 'ox-manuscript)

(unless (file-directory-p ox-manuscript-user-template-dir)
  (make-directory ox-manuscript-user-template-dir t))

;; * Journal templates
;; Bare-bones template
(add-to-list 'org-latex-classes
	     '("article-nodefaults"
	       "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<ACS journals>>
(add-to-list 'org-latex-classes
	     '("achemso"
	       "\\documentclass{achemso}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<APS journals>>
(add-to-list 'org-latex-classes '("revtex4-2"
				  "\\documentclass{revtex4-2}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<Springer journals>>
(add-to-list 'org-latex-classes '("svjour"
				  "%%%%%%%%%%%%%%%%%%%%%%%% Springer-Verlag %%%%%%%%%%%%%%%%%%%%%%%%%%
%
\\begin{filecontents}{leer.eps}
%!PS-Adobe-2.0 EPSF-2.0
%%CreationDate: Mon Jul 13 16:51:17 1992
%%DocumentFonts: (atend)
%%Pages: 0 1
%%BoundingBox: 72 31 601 342
%%EndComments

gsave
72 31 moveto
72 342 lineto
601 342 lineto
601 31 lineto
72 31 lineto
showpage
grestore
%%Trailer
%%DocumentFonts: Helvetica
\\end{filecontents}
\\documentclass{svjour}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes '("svjour3"
				  "\\documentclass{svjour3}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** <<Elsevier journals>>
(add-to-list 'org-latex-classes '("elsarticle"
				  "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** Nature
;; http://www.nature.com/srep/authors/submissions.html
;; the example shows unnumbered sections which might require a full exporter to get.
(add-to-list 'org-latex-classes '("nature"
				  "\\documentclass[fleqn,10pt]{wlscirep}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; ** Taylor and Francis
(add-to-list 'org-latex-classes '("gMOS2e"
				  "\\documentclass[]{gMOS2e}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
				  ("\\section{%s}" . "\\section*{%s}")
				  ("\\subsection{%s}" . "\\subsection*{%s}")
				  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
				  ("\\paragraph{%s}" . "\\paragraph*{%s}")
				  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


;; ** RSC
;; See http://www.rsc.org/Publishing/Journals/guidelines/AuthorGuidelines/AuthoringTools/Templates/tex.asp
;; I think their structure is too complex for ox-manuscript, and a real exporter would be required.

;; ** Science Magazine
;; Support for LaTeX in Science borders on ridiculous - LaTeX to HTML to Word - via DOS...
;; http://www.sciencemag.org/site/feature/contribinfo/prep/TeX_help/index.xhtml
;; No support in ox-manuscript for this.

;; ** Wiley
;; I have not been able to find a LaTeX package for Wiley

;; ** customized article. better margins
(add-to-list 'org-latex-classes
	     '("cmu-article"                          ;class-name
	       "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; ** NSF proposal - with Times New Roman
(add-to-list 'org-latex-classes
	     '("NSF"			;class-name
	       "\\documentclass[12pt]{article}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))




;; * Functions
;;;###autoload
(defun ox-manuscript-toggle-interactive-build ()
  "Toggle state of `ox-manuscript-interactive-build'.
When interactive you will see the output of every step, and be
prompted to continue it."
  (interactive)
  (if ox-manuscript-interactive-build
      (setq ox-manuscript-interactive-build nil)
    (setq ox-manuscript-interactive-build t)))


;;;###autoload
(defun ox-manuscript-cleanup (&optional depth)
  "Delete a bunch of temporary files based on extension.
DEPTH is an optional symbol to also remove the tex source and pdf
file.  DEPTH='deep will also remove the tex source and pdf file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (org-base (file-name-sans-extension org-file))
         (extensions '(".aux" ".pyg" ".bbl" ".blg" ".toc"
		       ".ind" ".ilg"
		       ".log" ".out" ".spl" "_flymake.out"
		       "Notes.bib" ".dvi"))
         (temp-files (mapcar (lambda (extension)
			       (concat org-base extension))
			     extensions)))
    (mapcar (lambda (temp-file)
              (if (file-exists-p temp-file)
		  (delete-file temp-file)))
	    temp-files)
    (when (file-exists-p "texput.log") (delete-file "texput.log"))
    (when depth
      (cond ((eq depth 'deep)
	     (when (file-exists-p (concat org-base ".tex"))
	       (delete-file (concat org-base ".tex")))
	     (when (file-exists-p (concat org-base ".pdf"))
	       (delete-file (concat org-base ".pdf"))))))))


;;;###autoload
(defun ox-manuscript-remove-image-extensions ()
  "Remove .png/pdf/eps extensions from \includegraphics directives.
This happens in the  exported latex file.

Run this from an org-buffer after you have exported it to a LaTeX
file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (tex-contents (with-temp-buffer (insert-file-contents tex-file) (buffer-string))))
    (with-temp-file tex-file (insert (replace-regexp-in-string
                                      (concat "\\(\\includegraphics"
                                              "\\(\[?[^\].*\]?\\)?\\)" ;; match optional [stuff]
                                              "{\\([^}].*\\)\.\\(png\\|pdf\\|eps\\|jpg\\|jpeg\\)}")
                                      "\\1{\\3}"  tex-contents)))))


;;;###autoload
(defun ox-manuscript-bibliography-to-bbl ()
  "Replace \\bibliography{} in tex file with contents of the bbl file.
We check for a bbl file, and if there is not one, we run
pdflatex, then bibtex to get one."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (bbl-file (replace-regexp-in-string "org$" "bbl" org-file))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file))
	 (bib-file (file-name-sans-extension tex-file)))

    ;; if no .bbl run commands to get one.
    (unless (file-exists-p bbl-file)
      (ox-manuscript-latex tex-file)
      (ox-manuscript-bibtex tex-file))

    (find-file tex-file)
    (goto-char (point-min))
    (re-search-forward "bibliography{" (point-max))
    (beginning-of-line)
    (kill-line)
    (insert-file-contents bbl-file)
    (delete-file bbl-file)
    (save-buffer)
    (kill-buffer)))


;;;###autoload
(defun ox-manuscript-nobibliography ()
  "Create the separate bibliography file and build it.
This occurs if you use a nobibliography link to specify that
references should go into a separate file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (bbl-file (replace-regexp-in-string "org$" "bbl" org-file))
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (references-tex-file (replace-regexp-in-string
			       ".org$"
			       "-references.tex" org-file))
	 (references-pdf (replace-regexp-in-string "tex$" "pdf" references-tex-file))
	 (bib-file (file-name-sans-extension tex-file))
	 p1)

    ;; if no .bbl run commands to get one.
    (unless (file-exists-p bbl-file)
      (ox-manuscript-latex tex-file)
      (ox-manuscript-bibtex tex-file))

    (when (file-exists-p references-tex-file)
      (delete-file references-tex-file))

    (when (file-exists-p references-pdf)
      (delete-file references-pdf))

    ;; The idea here is to take the original tex file and use the same layout,
    ;; packages etc... in case any of the are important for building the
    ;; references. Basically we delete everything between \begin{document} and
    ;; \end{document} and then insert the bbl contents.
    (find-file references-tex-file)

    (insert-file-contents tex-file)
    (goto-char (point-min))
    (re-search-forward "begin{document}" (point-max))
    (forward-line)
    (insert ox-manuscript-page-numbering)
    (beginning-of-line)
    (setq p1 (point))
    (re-search-forward "end{document}")
    (beginning-of-line)
    (forward-line -1)
    (delete-region p1 (point))
    (insert (format "\\input{%s}\n" bbl-file))

    (save-buffer)
    (kill-buffer)

    ;; now build it
    (ox-manuscript-latex references-tex-file)
    (ox-manuscript-latex references-tex-file)

    ;; now clean up
    (cl-loop for ext in '(".aux" ".out" ".pyg" ".log")
	     with fname = (concat (file-name-sans-extension references-tex-file)
				  ext)
	     do
	     (when (file-exists-p fname)
	       (delete-file fname)))

    references-pdf))


;;;###autoload
(defun ox-manuscript-run-bibtex-p ()
  "Return whether we need to run bibtex or not.
Based on there being a cite link in the buffer.  We assume there
is a bibliography and style defined if a cite is found.  no check
is made for that."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "cite:" nil t)))

;; * Build functions

;;;###autoload
(defun ox-manuscript-latex (tex-file)
  "Run `ox-manuscript-latex-command' on TEX-FILE.
This function checks for the presence of minted, and uses
-shell-escape if needed.  You can run this interactively, and you
will be prompted for a tex file name."
  (interactive "fTex file: ")
  (message "running %s on %s" ox-manuscript-latex-command tex-file)

  (let ((minted-p (with-temp-buffer
		    (insert-file-contents tex-file)
		    (beginning-of-buffer)
		    (re-search-forward "{minted}" nil t)))
	(search-upper-case nil)
	(cb (current-buffer))
	(results))

    ;; run pdflatex
    (if minted-p
	(setq results (shell-command-to-string
		       (concat
			ox-manuscript-latex-command
			" -shell-escape -interaction nonstopmode "
			tex-file)))
      ;; else
      (setq results
	    (shell-command-to-string
	     (concat ox-manuscript-latex-command
		     " -interaction nonstopmode "
		     tex-file))))

    (with-current-buffer (get-buffer-create "*latex*")
      (insert results))

    ;; return truth value for success
    (when (or (string-match "Fatal error occurred" results)
	      (string-match "Undefined control sequence" results))
      (switch-to-buffer "*latex*")
      (occur "warning\\|undefined\\|error\\|missing")
      (if (y-or-n-p "Continue?")
	  nil
	(let ((debug-on-error nil))
	  (error "LaTeX compilation error detected."))))))

;;;###autoload
(defun ox-manuscript-bibtex (tex-file)
  "Run `ox-manuscript-bibtex-command' on the TEX-FILE."
  (interactive "fTex file: ")
  (message "running bibtex on %s" tex-file)

  (let* ((basename (file-name-sans-extension tex-file))
	 (output (shell-command-to-string (concat ox-manuscript-bibtex-command " " basename))))
    (with-current-buffer (get-buffer-create "*bibtex*")
      (insert output))))


;;;###autoload
(defun ox-manuscript-makeindex (tex-file)
  "Run makeindex program on TEX-FILE."
  (interactive "fTex file: ")
  (let* ((basename (file-name-sans-extension tex-file))
	 (output (shell-command-to-string (concat "makeindex " basename ".idx"))))
    (with-current-buffer (get-buffer-create "*makeindex*")
      (insert output))))


;;;###autoload
(defun ox-manuscript-makeglossary (tex-file)
  "Run makeglossary program on TEX-FILE."
  (interactive "fTex file: ")
  (let* ((basename (file-name-base tex-file))
	 (cmd (concat "makeglossaries " basename))
	 (output (shell-command-to-string cmd)))
    (message "Ran %s" cmd)
    (with-current-buffer (get-buffer-create "*makeglossary*")
      (insert output))))


;;;###autoload
(defun ox-manuscript-latex-pdf-process (quoted-tex-file)
  "Build QUOTED-TEX-FILE to pdf.
The argument is called quoted-tex-file because this seems to be
what `org-mode' passes to this function.  The function strips the
quotes out.  Depending on the value of
`ox-manuscript-interactive-build', you will get buffers of the
intermediate output steps."
  (interactive "fTex file: ")
  ;; it seems the filename passed to this function from org-mode has
  ;; "" in it. we remove them here.
  (let* ((tex-file (replace-regexp-in-string "\"" "" quoted-tex-file))
	 (basename (file-name-sans-extension tex-file))
	 (pdf-file (concat basename ".pdf"))
	 (status)
	 (cb (current-buffer))
	 (run-makeindex-p)
	 (run-makeglossary-p)
	 (run-bibtex-p)
	 (nobibliography-p))

    ;; start out clean
    (ox-manuscript-cleanup)

    (when (file-exists-p pdf-file)
      (delete-file pdf-file))

    ;; Figure out which things need to get run.
    (with-temp-buffer
      (insert-file-contents tex-file)
      (goto-char (point-min))
      (setq run-makeindex-p (re-search-forward "\\\\makeindex" nil t))
      (goto-char (point-min))
      (setq run-makeglossary-p (re-search-forward "\\\\makeglossaries" nil t))
      (goto-char (point-min))
      (setq run-bibtex-p (re-search-forward "\\\\bibliography{" nil t))
      (goto-char (point-min))
      (setq nobibliography-p (re-search-forward "\\\\nobibliography{" nil t)))

    (setq status (catch 'status
		   ;; run first latex
		   (ox-manuscript-latex tex-file)
		   (when ox-manuscript-interactive-build
		     (switch-to-buffer "*latex*")
		     (end-of-buffer)
		     (occur "warning\\|undefined\\|error\\|missing")
		     (if (y-or-n-p "Continue to bibtex? ")
			 ;; continuing. delete buffers
			 (progn
			   (mapcar (lambda (x)
				     (when (get-buffer x) (kill-buffer x)))
				   '("*latex*" "*bibtex*" "*makeindex*"
				     "*makeglossary*" "*Occur*"))
			   (switch-to-buffer cb))
		       ;; not continuing
		       (throw 'status nil)))

		   ;; run bibtex if needed
		   (when run-bibtex-p
		     (ox-manuscript-bibtex tex-file)
		     (when ox-manuscript-interactive-build
		       (switch-to-buffer "*bibtex*")
		       (end-of-buffer)
		       (occur "warning\\|undefined\\|error\\|missing")
		       (if (y-or-n-p "Continue? ")
			   ;; continuing. delete buffers
			   (progn
			     (mapcar (lambda (x)
				       (when (get-buffer x) (kill-buffer x)))
				     '("*latex*" "*bibtex*"
				       "*makeindex*" "*Occur*"))
			     (switch-to-buffer cb))
			 ;; not continuing
			 (throw 'status nil))))

		   ;; Handle the nobibliography case.
		   (when nobibliography-p
		     (ox-manuscript-nobibliography))

		   ;; glossary
		   (when run-makeglossary-p
		     (ox-manuscript-makeglossary tex-file)
		     (when ox-manuscript-interactive-build
		       (switch-to-buffer "*makeglossary*")
		       (end-of-buffer)
		       (occur "warning\\|undefined\\|error\\|missing")
		       (if (y-or-n-p "Continue to latex 2? ")
			   ;; continuing. delete buffers
			   (progn
			     (mapcar (lambda (x)
				       (when (get-buffer x) (kill-buffer x)))
				     '("*latex*" "*bibtex*" "*makeindex*"
				       "*makeglossary*" "*Occur*"))
			     (switch-to-buffer cb))
			 ;; not continuing
			 (throw 'status nil))))

		   ;; index
		   (when run-makeindex-p
		     (ox-manuscript-makeindex tex-file)
		     (when ox-manuscript-interactive-build
		       (switch-to-buffer "*makeindex*")
		       (end-of-buffer)
		       (occur "warning\\|undefined\\|error\\|missing")
		       (if (y-or-n-p "Continue to latex 2? ")
			   ;; continuing. delete buffers
			   (progn
			     (mapcar (lambda (x)
				       (when (get-buffer x) (kill-buffer x)))
				     '("*latex*" "*bibtex*" "*makeindex*"
				       "*makeglossary*" "*Occur*"))
			     (switch-to-buffer cb))
			 ;; not continuing
			 (throw 'status nil))))

		   (ox-manuscript-latex tex-file)
		   (when ox-manuscript-interactive-build
		     (switch-to-buffer "*latex*")
		     (end-of-buffer)
		     (occur "warning\\|undefined\\|error\\|missing")
		     (if (y-or-n-p "Continue to latex3? ")
			 ;; continuing. delete buffers
			 (progn
			   (mapcar (lambda (x)
				     (when (get-buffer x) (kill-buffer x)))
				   '("*latex*" "*bibtex*"
				     "*makeindex*" "*Occur*"))
			   (switch-to-buffer cb))
		       ;; not continuing
		       (throw 'status nil)))

		   (ox-manuscript-latex tex-file)
		   (mapcar (lambda (x) (when (get-buffer x) (kill-buffer x)))
			   '("*latex*" "*bibtex*" "*makeindex*" "*Occur*"))
		   "done"))

    (message "Finished with status = %s. %s exists = %s in %s."
	     status pdf-file (file-exists-p pdf-file) default-directory)
    0))

;; We use our function for building the manuscript
(setq org-latex-pdf-process #'ox-manuscript-latex-pdf-process)


(defun ox-manuscript-build ()
  "Build manuscript.
This is done manually here for building the submission manuscript
pdf."
  (interactive)

  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-latex tex-file)

    (when (ox-manuscript-run-bibtex-p)
      (ox-manuscript-bibtex tex-file))

    (ox-manuscript-latex tex-file)
    (ox-manuscript-latex tex-file)

    (ox-manuscript-cleanup)
    (format "Manuscript built on %s with org-mode %s"
	    (current-time-string) (org-version))

    ;; return pdf name
    pdf-file))

;; * Export functions
(defun ox-manuscript-export-and-build (&optional async subtreep visible-only body-only options)
  "Cleans up, then exports the latex and builds using the org-mode machinery."
  (interactive)
  (ox-manuscript-cleanup 'deep)

  ;; insert bibliography if needed
  ;; (save-excursion
  ;;   (beginning-of-buffer)
  ;;   (unless (re-search-forward "^bibliography:" nil t)
  ;;     (end-of-buffer)
  ;;     (insert
  ;;      (format
  ;;	"\n\nbibliography:%s"
  ;;	(mapconcat (lambda (x)
  ;;		     (file-relative-name x (file-name-directory (buffer-file-name))))
  ;;		   org-ref-default-bibliography ",")))))
  (save-buffer)
  (prog1
      (org-latex-export-to-pdf async subtreep visible-only body-only options)
    (ox-manuscript-cleanup)))


(defun ox-manuscript-export-and-build-and-open (&optional async subtreep visible-only body-only options)
  "Cleanup, export, build and open pdf."
  (interactive)
  (org-open-file (ox-manuscript-export-and-build  async subtreep visible-only body-only options)))


(defun ox-manuscript-export-submission-manuscript (&optional async subtreep visible-only body-only options)
  "Create manuscript for submission.
This removes the .png extensions from graphics, and replaces the
bibliography with the contents of the bbl file. the result is a
single, standalone tex-file."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
         (tex-file (replace-regexp-in-string "org$" "tex" org-file)))
    (ox-manuscript-cleanup 'deep)
    (org-latex-export-to-latex async subtreep visible-only body-only options)
    (ox-manuscript-remove-image-extensions)
    (ox-manuscript-bibliography-to-bbl)
    (ox-manuscript-cleanup)
    tex-file))


(defun ox-manuscript-build-submission-manuscript (&optional async subtreep visible-only body-only options)
  "Create manuscript for submission.
This removes the .png extensions from graphics, and replaces the
bibliography with the contents of the bbl file. The result is a
single, standalone tex-file, and the corresponding pdf."
  (interactive)
  (let* ((org-file (file-name-nondirectory (buffer-file-name)))
	 (tex-file (replace-regexp-in-string "org$" "tex" org-file))
         (pdf-file (replace-regexp-in-string "org$" "pdf" org-file)))
    (ox-manuscript-cleanup 'deep)
    (org-latex-export-to-latex async subtreep visible-only body-only options)
    (ox-manuscript-remove-image-extensions)
    (ox-manuscript-bibliography-to-bbl)
    (ox-manuscript-latex tex-file)
    (ox-manuscript-latex tex-file)
    (ox-manuscript-cleanup)
    (format "Manuscript built on %s with org-mode %s" (current-time-string) (org-version))
    pdf-file))


(defun ox-manuscript-build-submission-manuscript-and-open (&optional async subtreep visible-only body-only options)
  "Build manuscript for submission and open the pdf.
This removes the .png extensions from graphics, and replaces the
bibliography with the contents of the bbl file. the result is a
single, standalone tex-file, and the corresponding pdf."
  (interactive)
  (org-open-file (ox-manuscript-build-submission-manuscript async subtreep visible-only body-only options)))


(defun ox-manuscript-export-and-build-and-email (&optional async subtreep visible-only body-only options)
  "Build the manuscript and attach the pdf to an email buffer."
  (interactive)
  (let ((pdf (ox-manuscript-export-and-build async subtreep visible-only body-only options)))
	(org-open-file pdf)
	(message-mail)
	(mml-attach-file pdf)
	(message-goto-to)))


(defun ox-manuscript-make-submission-archive (&optional async subtreep visible-only body-only options &rest files)
  "Create a directory containing the tex file and images.
This is a standalone directory that is suitable for
submission. We assume the tex file in this directory is suitable
for submission, e.g. it was created from
`ox-manuscript-build-submission-manuscript-and-open'.

The optional FILES keyword is a list of additional files to copy into the archive folder."
  (interactive)
  (save-buffer)
  (let* ((org-file (buffer-name))
	 (org-file-abs-path (buffer-file-name))
	 (base-name (file-name-sans-extension org-file))
	 ;; directory to save all exports in, using the current date
	 (tex-archive (concat base-name
			      "-"
			      (format-time-string "%Y-%m-%d/" (current-time))))
	 (tex-file (concat (file-name-sans-extension org-file-abs-path) ".tex"))
	 (tex-bak-file (concat (file-name-sans-extension org-file) ".tex.bak"))
	 (base-tex-file (file-name-nondirectory tex-file))
	 (bbl-file (replace-regexp-in-string "tex$" "bbl" tex-file))
	 (tex-contents (with-temp-buffer
			 (insert-file-contents tex-file)
			 (buffer-string)))
	 (figure-count 0)
	 beg start end latex-file-path)

    ;; Make sure we have a tex-file and it is newer
    (unless (and  (file-exists-p tex-file)
		  (file-newer-than-file-p tex-file org-file))
      ;;  and if not, build a tex file
      (ox-manuscript-export-and-build
       async subtreep
       visible-only body-only options)
      ;; remove image extensions
      (ox-manuscript-remove-image-extensions)
      ;; fix bibliography
      (ox-manuscript-bibliography-to-bbl))

    ;; make backup of tex file so we can restore later
    (copy-file tex-file tex-bak-file t)

    ;; delete tex-archive if it exists then make a new one
    (when (file-exists-p tex-archive)
      (delete-directory tex-archive t))
    (make-directory tex-archive t)

    ;; find images and flatten their paths
    (with-temp-file tex-file
      (insert tex-contents)
      (goto-char (point-min))
      (while (re-search-forward
	      ;; We get to the { in this, and then use parsing to get what is in
	      ;; the {}. The regexp was not reliable in some cases.
	      "\\\\includegraphics"
	      nil t)
	(search-forward "{")
	(setq start (point))
	(backward-char)
	(forward-sexp)
	(setq end (point))
	(setq latex-file-path (buffer-substring-no-properties
			       start (- end 1)))
	
	(cl-incf figure-count)
	(let* ((eps-file (concat latex-file-path ".eps"))
	       (pdf-file (concat latex-file-path ".pdf"))
	       (png-file (concat latex-file-path ".png"))
	       (jpg-file (concat latex-file-path ".jpg"))
	       (jpeg-file (concat latex-file-path ".jpeg"))
	       (fname (file-name-nondirectory  latex-file-path)))
	  ;;  Copy the images to the tex-archive.
	  
	  (when (file-exists-p eps-file)
	    (copy-file eps-file (expand-file-name
				 (format "%02d-%s.eps" figure-count fname)
				 tex-archive)
		       t))
	  (when (file-exists-p pdf-file)
	    (copy-file pdf-file (expand-file-name
				 (format "%02d-%s.pdf" figure-count fname)
				 tex-archive)
		       t))
	  (when (file-exists-p png-file) 
	    (copy-file png-file (expand-file-name
				 (format "%02d-%s.png" figure-count fname)
				 tex-archive)
		       t))
	  (when (file-exists-p jpg-file)
	    (copy-file jpg-file (expand-file-name
				 (format "%02d-%s.jpg" figure-count fname)
				 tex-archive)
		       t))
	  (when (file-exists-p jpeg-file)
	    (copy-file jpeg-file (expand-file-name
				  (format "%02d-%s.jpeg" figure-count fname)
				  tex-archive)
		       t))

	  ;; flatten the filename in the tex-file
	  (cl--set-buffer-substring
	   start (- end 1)
	   (format "%02d-%s" figure-count fname)))))

    ;; the tex-file is no longer valid in the current directory
    ;; because the paths to images are wrong. So we move it to where
    ;; it belongs.
    (rename-file tex-file (expand-file-name (file-name-nondirectory tex-file) tex-archive) t)

    ;; restore the original version
    (rename-file tex-bak-file tex-file)

    ;; copy the optional additional files
    (mapcar (lambda (f)
	      (copy-file f (file-name-as-directory tex-archive)))
	    files)

    ;; We should build and open the pdf-file. That should just be
    ;; running latex twice.  we do that manually in the archive
    ;; directory.
    (let ((default-directory (file-name-as-directory
			      (expand-file-name tex-archive))))
      ;; I do not know why shell-command does not work here.
      (message "Building %s in %s" base-tex-file default-directory)
      (call-process "latexmk" nil nil nil "-f" "-pdf" "-shell-escape" "-interaction=nonstopmode" base-tex-file)
      ;; (ox-manuscript-cleanup)
      
      (org-open-file (concat
		      (file-name-sans-extension
		       base-tex-file)
		      ".pdf")))
    ;; return directory
    tex-archive))


(defun ox-manuscript-build-with-comments (&optional async subtreep visible-only body-only options)
  "Builds a manuscript with comments.
This assumes you use `org-editmarks'.

This function modifies the exported LaTeX so you do not have to
put the packages in the org file.
"
  (interactive)
  (let* ((tex-file (org-latex-export-to-latex async subtreep visible-only body-only options))
	 (tex-contents (with-temp-buffer
			 (insert-file-contents tex-file)
			 (buffer-string))))
    (with-temp-file tex-file
      (insert tex-contents)
      (goto-char (point-min))
      (re-search-forward "\\\\begin{document}")
      (insert "
\\presetkeys{todonotes}{color=blue!30}{}
\\todotoc
\\listoftodos")
      (re-search-backward "\\\\begin{document}")
      (backward-char)
      (end-of-line)
      (insert "\\usepackage[colorinlistoftodos]{todonotes}")))
  (org-open-file  (ox-manuscript-build)))


;; * The backend options
(org-export-define-derived-backend 'cmu-manuscript 'latex
  :menu-entry
  '(?j "Export with ox-manuscript"
       ((?L "As LaTeX buffer" org-latex-export-as-latex)
	(?l "As LaTeX file" org-latex-export-to-latex)
	(?p "As manuscript PDF file" ox-manuscript-export-and-build)
	(?o "As manuscript PDF and open" ox-manuscript-export-and-build-and-open)
	(?e "As PDF and email" ox-manuscript-export-and-build-and-email)
	(?s "As submission manuscript tex"
	    ox-manuscript-export-submission-manuscript)
	(?M "As submission manuscript pdf"
	    ox-manuscript-build-submission-manuscript)
	(?m "As submission manuscript pdf and open"
	    ox-manuscript-build-submission-manuscript-and-open)
	(?c "As manuscript PDF with comments"
	    ox-manuscript-build-with-comments)
	(?a "As submission archive" ox-manuscript-make-submission-archive))))


;; * Add templates for manuscripts
(add-hook
 'org-mode-hook
 (lambda ()
   ;; This is a pattern for things that should be replaced in snippets
   ;; in the templates.
   (font-lock-add-keywords
    nil
    '(("<replace:?.*?>" 0 font-lock-warning-face t))
    t)))


(defvar ox-manuscript-templates-dir
  (expand-file-name
   "ox-manuscript-templates/" (or (when load-file-name
				    (file-name-directory load-file-name))
				  (file-name-directory (buffer-file-name))))
  "Directory where manuscript templates are.
The templates are just org-files that can be inserted into a
  buffer.")


(defun ox-manuscript-get-filetag (tag fname)
  "Return value for TAG in FNAME."
  (interactive "sTag: fFile: ")
  (with-current-buffer (find-file-noselect fname)
    (setq kwds (org-element-map (org-element-parse-buffer 'element) 'keyword
		 (lambda (keyword)
		   (cons (org-element-property :key keyword)
			 (org-element-property :value keyword)))))
    (cdr (assoc (upcase tag) kwds))))


(defun ox-manuscript-parse-template-file (template-file)
  "Parse the TEMPLATE-FILE and return a plist.
Return nil if no key is found in TEMPLATE-FILE."
  (when (ox-manuscript-get-filetag "KEY" template-file)
    (list
     :key (ox-manuscript-get-filetag "KEY" template-file)
     :group (ox-manuscript-get-filetag "GROUP" template-file)
     :template (ox-manuscript-get-filetag "TEMPLATE" template-file)
     :default-filename (ox-manuscript-get-filetag
			"DEFAULT-FILENAME" template-file)
     :filename template-file)))


(defun ox-manuscript-candidates ()
  "Return a cons list of manuscript candidate templates
These are snippets in `ox-manuscript-templates-dir' in the \"manuscript\" group.
'((name . data))."
  (append
   (if ox-manuscript-templates-dir
       (cl-loop for template-file in (f-entries ox-manuscript-templates-dir
						(lambda (f)
						  (f-ext? f "org")))
		with data = nil
		do (setq data (ox-manuscript-parse-template-file template-file))
		when data
		collect data)
     '())
   (if ox-manuscript-user-template-dir
       (cl-loop for template-file in (f-entries ox-manuscript-user-template-dir
						(lambda (f)
						  (f-ext? f "org")))
		with data = nil
		do (setq data (ox-manuscript-parse-template-file template-file))
		when data
		collect data)
     '())))


(defun ox-manuscript-open (key)
  "Open entry for KEY.
KEY is a string that maps to a :key entry in `ox-manuscript-candidates'.
Open document if it exists, create it otherwise."
  (let ((entry (car (seq-filter (lambda (x) (string= key (plist-get x :key))) (ox-manuscript-candidates)))))
    (if (file-exists-p (plist-get entry :default-filename))
	(find-file (plist-get entry :default-filename))
      (find-file (plist-get entry :default-filename))
      (insert-file-contents (plist-get entry :filename))
      (goto-char (point-min))
      (font-lock-fontify-buffer))))


;;;###autoload
(defun ox-manuscript-new-ivy ()
  "Create a new manuscript from a template in
`ox-manuscript-templates-dir' or
`ox-manuscript-user-template-dir'."
  (interactive)
  (let ((candidates (ox-manuscript-candidates)))
    (ivy-read "type: " (mapcar (lambda (x)
				 (cons
				  (format "%15s | %s"
					  (plist-get x :group)
					  (plist-get x :template))
				  x))
			       candidates)
	      
	      :action (lambda (entry)
			(ox-manuscript-open (plist-get (cdr entry) :key))))))


(defun ox-manuscript-texcount ()
  "Use texcount to estimate words in an org-file if it exists.
Fall back to `tex-count-words'"
  (interactive)
  (if (executable-find "texcount")
      (let ((f (org-latex-export-to-latex)))
	(shell-command
	 (concat "texcount "
		 "-unicode "
		 "-inc "
		 (shell-quote-argument f))))
    (message (concat "Warning: texcount not found. Word estimate does not exclude cite/ref commands. "
		     (tex-count-words (region-beginning) (region-end))))))


;; * Sync-mode
(define-minor-mode ox-manuscript-sync-mode
  "Minor mode for synchronizing an org-file and pdf.
See http://pragmaticemacs.com/emacs/speed-up-pdf-export-from-org-mode-with-latexmk/
for a recipe to use for the export."
  :init-value nil
  :lighter " sync"
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html
  (let ((enable ox-manuscript-sync-mode))
    (if enable
	(add-hook 'after-save-hook (lambda ()
				     (org-latex-export-to-pdf 't)
				     (find-file-other-window
				      (concat
				       (file-name-base (buffer-file-name))
				       ".pdf")))
		  nil t)
      (remove-hook 'after-save-hook (lambda ()
				      (org-latex-export-to-pdf 't)
				      (find-file-other-window
				       (concat
					(file-name-base (buffer-file-name))
					".pdf")))))))


;; * sentence spacing

(defun ox-manuscript-one-space ()
  "Make sentences separated by one space."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (forward-sentence) (not (eobp)))
      (when (looking-at "\s+")
	(replace-match " ")))))


(defun ox-manuscript-two-space ()
  "Make sentences separated by two spaces."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (and (forward-sentence) (not (eobp)))
      (when (looking-at "\s+")
	(replace-match "  ")))))


(provide 'ox-manuscript)

;;; ox-manuscript.el ends here
