;;; scimax-latex-subfiles.el --- scimax-org-subfiles

;;; Commentary:
;; A custom exporter for org-files that uses subfiles to break larger files into
;; subfiles for LaTeX export to PDF. No other exports are supported.
;;
;; [2020-07-31 Fri] This is lightly tested
;;
;; Use #+LATEX_HEADER: \usepackage{subfiles} in the main file.

;; In the subfile org files, you need use
;; #+LATEX_CLASS: subfiles
;; #+SUBFILE-MAIN: main.org
;;
;; to indicate the main file to get the LaTeX setup from.
;;
;; Use `sos-export-as-pdf-and-open' C-c C-e s o in any file to build the PDF.
;;
;; Inspired by personal correspondence with Nicky van Foreest.


;;; Code:

(require 'ox-latex)
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
	       '("subfiles"
		 "\\documentclass{subfiles}
   [NO-DEFAULT-PACKAGES]
   [NO-PACKAGES]"
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))




;; * Define subfile link with export function
;; This allows you to have clickable subfile link in your main section.
(org-link-set-parameters
 "subfile"
 :follow (lambda (path)
	   (find-file path))
 :export (lambda (path desc backend)
	   (with-current-buffer (find-file-noselect path)
	     (org-latex-export-to-latex))
	   (format "\\subfile{%s}" (concat (file-name-sans-extension path) ".tex"))))


;; * Define export functions that are conditional for subfiles

(defun sos-subfile-p ()
  "Return the main file or nil."
  (cdr (assoc "SUBFILE-MAIN" (org-element-map (org-element-parse-buffer)
				 'keyword
			       (lambda (key)
				 (cons (org-element-property :key key)
				       (org-element-property :value key)))))))


(defun sos-export-as-latex (&optional async subtreep visible-only
				      body-only info)
  "Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((subfile-main (sos-subfile-p)))
    (when subfile-main
      ;; make sure we set class options here.
      (setq info (plist-put
		  info
		  :latex-class-options
		  (format "[%s]" (file-name-sans-extension subfile-main)))))

    (org-latex-export-as-latex async subtreep visible-only
			       body-only info)))


(defun sos-export-to-latex (&optional async subtreep visible-only
				      body-only info)
  "Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((subfile-main (sos-subfile-p)))
    (when subfile-main
      ;; make sure we set class options here.
      (setq info (plist-put
		  info
		  :latex-class-options
		  (format "[%s]" (file-name-sans-extension subfile-main)))))
    (org-latex-export-to-latex async subtreep visible-only
			       body-only info)))


(defun sos-export-to-pdf (&optional async subtreep visible-only
				    body-only info)
  "Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (let ((subfile-main (sos-subfile-p))
	(subfile-class-options (sos-subfile-class-options))
	options)
    (when subfile-main
      ;; make sure main texfile exists.
      (with-current-buffer (find-file-noselect subfile-main)
	(org-latex-export-to-latex))

      (setq info (plist-put
		  info
		  :latex-class-options
		  (format "[%s]" (file-name-sans-extension subfile-main)))))

    (org-latex-export-to-pdf async subtreep visible-only
			     body-only info)))


(defun sos-export-as-pdf-and-open (&optional async subtreep visible-only
					     body-only info)
  "Optional argument ASYNC to asynchronously export.
Optional argument SUBTREEP to export current subtree.
Optional argument VISIBLE-ONLY to only export visible content.
Optional argument BODY-ONLY export only the body.
Optional argument INFO is a plist of options."
  (org-open-file (sos-export-to-pdf)))




;; * An exporter menu

(org-export-define-derived-backend 'subfile 'org
  :menu-entry
  '(?s "Export to with subfiles"
       ((?b "to LaTeX buffer" sos-export-as-latex)
	(?l "to LaTeX file" sos-export-to-latex)
	(?p "to PDF" sos-export-to-pdf)
	(?o "to PDF and open" sos-export-as-pdf-and-open))))



(provide 'scimax-org-subfiles)

;;; scimax-org-subfiles.el ends here
