;;; ox-word.el --- Collection of functions to export org-mode to MS Word documents
;; Collection of ideas to get Word documents from org-documents
;;
;;; Commentary:
;; This library has only been tested in scimax. It may not work reliably outside
;; of scimax.

;; * Using pandoc via LaTeX

;; In this approach we export the org-document to latex, and use pandoc to
;; convert to Word. This leverages Pandoc's ability to get pretty reasonable
;; bibliographies and citations using CSL. The Pandoc I have tested with does
;; not do a good job with cross-references (figures and tables are linked ,but
;; not numbered), so here we post-process the tex file to hard code figure and
;; table numbers in it, and to replace references to them in the text. This has
;; the benefit that they are numbered, but they are not links in the word
;; document, so they will not be updatable in the Word document if you edit it
;; further.

;; Use #+PANDOC-CSL: /full/path/to/some/style.csl to set
;; the bibliography style. You can get the csl files from
;; https://www.zotero.org/styles

;;; Code:
(require 'org-ref)

(defun ox-export-get-pandoc-version ()
  "Returns the major version of pandoc."
  (string-to-number
   (substring (shell-command-to-string "pandoc --version") 7 8)))


(defun ox-export-call-pandoc-tex-to-docx (biboption csl tex-file docx-file)
  "Run pandoc to convert the exported tex file to docx."
  (let* ((pandoc-version (ox-export-get-pandoc-version))
         (pandoc-command
          (if (>= pandoc-version 2)
              "pandoc -s %s%s\"%s\" --to=docx+smart -o \"%s\""
            "pandoc -s -S %s%s\"%s\" -o \"%s\"")))
    (shell-command (format pandoc-command biboption csl tex-file docx-file))))


(defun ox-export-call-pandoc-tex-to-html (biboption csl tex-file html-file)
  "Run pandoc to convert the exported tex file to html."
  (let* ((pandoc-version (ox-export-get-pandoc-version))
         (pandoc-command
          (if (>= pandoc-version 2)
              "pandoc -s %s%s\"%s\" --to=html+smart -o \"%s\""
            "pandoc -s -S %s%s\"%s\" -o \"%s\"")))
    (shell-command (format pandoc-command biboption csl tex-file html-file))))


(defun ox-export-via-latex-pandoc-to-docx-and-open (&optional async subtreep visible-only body-only options)
  "Export the current org file as a docx via LaTeX."
  (interactive)
  (let* ((bibfiles (mapcar 'expand-file-name (org-ref-find-bibliography)))
	 (temp-bib)
	 (bibtex-entries)
	 biboption
	 csl
	 ;; this is probably a full path
	 (current-file (buffer-file-name))
	 (basename (file-name-sans-extension current-file))
	 (tex-file (concat basename ".tex"))
	 (docx-file (concat basename ".docx")))

    (save-buffer)

    ;; I make a temp bibfile because my big one causes pandoc to choke. This
    ;; should only create a file with the required entries.
    (when bibfiles
      (setq bibtex-entries (let* ((bibtex-files bibfiles)
				  (keys (reverse (org-ref-get-bibtex-keys)))
				  (bibtex-entry-kill-ring-max (length keys))
				  (bibtex-entry-kill-ring '()))

			     (save-window-excursion
			       (cl-loop for key in keys
					do
					(bibtex-search-entry key t)
					(bibtex-kill-entry t)))
			     (mapconcat
			      'identity
			      bibtex-entry-kill-ring
			      "\n\n"))
	    temp-bib (make-temp-file "ox-word-" nil ".bib")
	    biboption (format " --bibliography=%s " temp-bib))
      (with-temp-file temp-bib
	(insert bibtex-entries)))

    (setq csl (cdr (assoc "PANDOC-CSL"
			  (org-element-map (org-element-parse-buffer) 'keyword
			    (lambda (key) (cons
					   (org-element-property :key key)
					   (org-element-property :value key)))))))
    (if csl (setq csl (format " --csl=%s " csl))
      (setq csl " "))

    (org-latex-export-to-latex async subtreep visible-only body-only options)
    ;; Now we do some post-processing on the tex-file
    ;; Tables first.
    (let* ((table-regex "\\\\begin{table}.*
\\\\caption{\\(?1:\\(?2:.*\\)\\\\label{\\(?3:.*\\)}\\)}")
    	   (buf (find-file-noselect tex-file))
    	   (i 0)
    	   labels)
      (with-current-buffer buf
    	(goto-char (point-min))
    	(while (re-search-forward table-regex nil t)
    	  (incf i)
    	  (push (cons (match-string 3) i) labels)
    	  (replace-match (format "Table %d. \\2" i) nil nil nil 1))
	;; Now replace the refs.
	(goto-char (point-min))
	(while (re-search-forward "\\\\ref{\\(?1:.*?\\)}" nil t)
	  (when (cdr (assoc (match-string 1) labels))
	    (replace-match (format "%d" (cdr (assoc (match-string 1) labels))))))
    	(save-buffer))
      (message "done with tables."))

    ;; Now figures
    (let* ((fig-regex "\\includegraphics.*
\\\\caption{\\(?3:\\(?1:.*\\)\\\\label{\\(?2:.*\\)}\\)}")
	   (buf (find-file-noselect tex-file))
	   (i 0)
	   labels)
      (with-current-buffer buf
	(goto-char (point-min))
	(while (re-search-forward fig-regex nil t)
	  (incf i)
	  (push (cons (match-string 2) i) labels)
	  (replace-match (format "Figure %d. \\1." i) nil nil nil 3))
    	;; Now replace the refs.
    	(goto-char (point-min))
    	(while (re-search-forward "\\\\ref{\\(?1:.*?\\)}" nil t)
    	  (when (cdr (assoc (match-string 1) labels))
    	    (replace-match (format "%d" (cdr (assoc (match-string 1) labels))))))
	(save-buffer)
	(kill-buffer buf)))


    (when (file-exists-p docx-file) (delete-file docx-file))
    (ox-export-call-pandoc-tex-to-docx biboption csl tex-file docx-file)
    (when (file-exists-p temp-bib)
      (delete-file temp-bib))
    (org-open-file docx-file '(16))))


(defun ox-export-via-latex-pandoc-to-html-and-open (&optional async subtreep visible-only body-only options)
  "Export the current org file as a html via LaTeX."
  (interactive)
  (let* ((bibfile (expand-file-name (car (org-ref-find-bibliography))))
	 (temp-bib)
	 (bibtex-entries)
	 biboption
	 csl
	 ;; this is probably a full path
	 (current-file (buffer-file-name))
	 (basename (file-name-sans-extension current-file))
	 (tex-file (concat basename ".tex"))
	 (html-file (concat basename ".html")))

    (save-buffer)

    ;; I make a temp bibfile because my big one causes pandoc to choke. This
    ;; should only create a file with the required entries.
    (when bibfile
      (setq bibtex-entries (let* ((bibtex-files (org-ref-find-bibliography))
				  (keys (reverse (org-ref-get-bibtex-keys)))
				  (bibtex-entry-kill-ring-max (length keys))
				  (bibtex-entry-kill-ring '()))

			     (save-window-excursion
			       (cl-loop for key in keys
					do
					(bibtex-search-entry key t)
					(bibtex-kill-entry t)))
			     (mapconcat
			      'identity
			      bibtex-entry-kill-ring
			      "\n\n"))
	    temp-bib (make-temp-file "ox-html-" nil ".bib")
	    biboption (format " --bibliography=%s " temp-bib))
      (with-temp-file temp-bib
	(insert bibtex-entries)))

    (setq csl (cdr (assoc "PANDOC-CSL"
			  (org-element-map (org-element-parse-buffer) 'keyword
			    (lambda (key) (cons
					   (org-element-property :key key)
					   (org-element-property :value key)))))))
    (if csl (setq csl (format " --csl=%s " csl))
      (setq csl " "))

    (org-latex-export-to-latex async subtreep visible-only body-only options)

    (when (file-exists-p html-file) (delete-file html-file))
    (ox-export-call-pandoc-tex-to-html biboption csl tex-file html-file)

    (when (file-exists-p temp-bib)
      (delete-file temp-bib))
    (browse-url html-file)))


(org-export-define-derived-backend 'MSWord 'latex
  :menu-entry
  '(?w "Export to MS Word"
       ((?p "via Pandoc/LaTeX" ox-export-via-latex-pandoc-to-docx-and-open))))


(org-export-define-derived-backend 'pandoc-html 'latex
  :menu-entry
  '(?h "Export to HTML"
       ((?p "via Pandoc/LaTeX" ox-export-via-latex-pandoc-to-html-and-open))))

(provide 'ox-word)

;;; ox-word.el ends here
