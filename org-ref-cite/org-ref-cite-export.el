;;; org-ref-cite-export.el --- org-cite export processor
;;
;; Copyright(C) 2021 John Kitchin
;;
;; This file is not currently part of GNU Emacs.
;;
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
;;
;;; Commentary:
;; prefix and suffix text on single citations works fine, for example [cite/t:See @kitchin-2015-examp-effec last page]
;;  exports to \cite[See][last page]{kitchin-2015-examp-effec}.
;;
;; Multiple cites work well: [cite/t: @kitchin-2018-machin-learn-catal;@rose-2019-pybliom;]
;; exports to \cite{kitchin-2018-machin-learn-catal,rose-2019-pybliom}
;;
;; It is somewhat unclear what multiple citations with prefix/suffix should export to.
;;
;; The following org-keywords fine tune the export:
;;
;; You need to use the natbib package with this exporter. The export processor will check to make sure you have
;; NATBIB_OPTIONS:
;;  If you want to overwrite the defaults in `org-latex-packages-alist' you can set them in the keywords
;;
;; BIBLIOGRAPHYSTYLE:
;;  Use this keyword to specify the bibliography style.
;;
;; PRINT_BIBLIOGRAPHY:
;;
;; You can use :title "References Cited" to change the title of the references section
;; You can use :numbered t to change the section from unnumbered to numbered
;;
;; You can use :nobibliography t to turn off the bibliography in the output, but
;; have correct citations. The best way to get to the bibliography is in the
;; `ox-manuscript' library in scimax. This option does not currently work with
;; the :title or :numbered options.

;;; Code:
(require 'oc)

;; * Exporting

(defun org-ref-cite--build-arguments (citation)
  "Build arguments for citation command for CITATION object.
These are the cite keys"
  (format "{%s}"
          (mapconcat #'identity
                     (org-cite-get-references citation t)
                     ",")))


(defun org-ref-cite--build-optional-arguments (citation info)
  "Build optional arguments for citation command.
CITATION is the citation object.  INFO is the export state, as a property list."
  (let* ((origin (pcase (org-cite-get-references citation)
                   (`(,reference) reference)
                   (_ citation)))
         (suffix (org-element-property :suffix origin))
         (prefix (org-element-property :prefix origin)))
    (concat (and prefix (format "[%s]" (org-trim (org-export-data prefix info))))
            (cond
             (suffix (format "[%s]" (org-trim (org-export-data suffix info))))
             (prefix "[]")
             (t nil)))))


(defun org-ref-cite-export-citation (citation _style _ info)
  "Export CITATION object.
We ignore _STYLE here for the simpler way of getting it from the
citation. INFO is the export state, as a property list."
  (let ((style (org-element-property :style citation)))
    (concat (cdr (assoc style oc-bibtex-styles))
	    (org-ref-cite--build-optional-arguments citation info)
	    (org-ref-cite--build-arguments citation))))


(defun org-ref-cite-use-package (output &rest _)
  "Ensure output requires \"natbib\" package.
OUTPUT is the final output of the export process.
Use the keyword NATBIB_OPTIONS to overwrite any default put in."
  (let* ((re (rx "\\usepackage" (opt "[" (*? nonl) "]") "{natbib}"))
	 (natbib-options (cadr (assoc
				"NATBIB_OPTIONS"
				(org-collect-keywords
				 '("NATBIB_OPTIONS")))))
	 (usepackage (format "\\usepackage%s{natbib}\n"
			     (if (null natbib-options)
				 ""
			       (format "[%s]" natbib-options)))))
    (with-temp-buffer
      (save-excursion (insert output))
      (when (search-forward "\\begin{document}" nil t)
	;; Ensure there is a \usepackage{natbib} somewhere or add one.
	(goto-char (match-beginning 0))
	(if (re-search-backward re nil t)
	    ;; with a local setup replace what is there
	    (when natbib-options
	      (setf (buffer-substring (line-beginning-position) (line-end-position))
		    usepackage))
	  ;; it was not found, insert the package line.
          (insert usepackage)))
      (buffer-string))))


(defun org-ref-cite-export-bibliography (_keys files &rest _)
  "Print references from bibliography FILES.
FILES is a list of absolute file names.  STYLE is the bibliography style, as
a string or nil.

The actual bibliography command is determined by the
PRINT_BIBLIOGRAPHY keyword. If it contains a non-nil value for
:nobibliography then the command is \\nobibliography otherwise it
is \\bibliography.

You can use a :title option to set the title of the bibliography. The default is Bibliography.
You can use a :numbered option to set if the Bibliography section should be numbered. The default is not numbered."
  (let* ((bibtitle (or (plist-get (org-export-read-attribute
				   :attr
				   `(nil (:attr (,(cadr (assoc
							 "PRINT_BIBLIOGRAPHY"
							 (org-collect-keywords
							  '("PRINT_BIBLIOGRAPHY"))))))))
				  :title)))
	 (numbered (plist-get (org-export-read-attribute
			       :attr
			       `(nil (:attr (,(cadr (assoc
						     "PRINT_BIBLIOGRAPHY"
						     (org-collect-keywords
						      '("PRINT_BIBLIOGRAPHY"))))))))
			      :numbered))

	 (bibcmd (if  (plist-get (org-export-read-attribute
				  :attr
				  `(nil (:attr (,(cadr (assoc
							"PRINT_BIBLIOGRAPHY"
							(org-collect-keywords
							 '("PRINT_BIBLIOGRAPHY"))))))))
				 :nobibliography)
		     "nobibliography"
		   "bibliography"))
	 (style (cadr (assoc "BIBLIOGRAPHYSTYLE"
			     (org-collect-keywords '("BIBLIOGRAPHYSTYLE"))))))

    (concat
     (and style (format "\\bibliographystyle{%s}\n" style))
     (format "\\renewcommand{\\bibsection}{\\section%s{%s}}\n"
	     (if numbered  "" "*")
	     (if bibtitle (org-strip-quotes bibtitle) "References"))
     (format "\\%s{%s}"
	     bibcmd
             (mapconcat #'file-name-sans-extension
			(mapcar #'expand-file-name files)
			",")))))




(provide 'org-ref-cite-export)

;;; org-ref-cite-export.el ends here
