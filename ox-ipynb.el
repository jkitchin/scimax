;;; ox-ipynb.el --- Convert an org-file to an ipynb.

;;; Commentary:
;; 

;;; Code:
(require 'ox-md)

(defun export-ipynb-code-cell (src-result)
  "Return a lisp code cell for the org-element SRC-BLOCK."
  (let* ((src-block (car src-result))
	 (results-end (cdr src-result))
	 (results (org-no-properties (car results-end)))
	 (output-cells '())
	 img-path img-data
	 (start 0))

    ;; Handle inline images first
    (while (string-match "\\[\\[file:\\(.*?\\)\\]\\]" (or results "") start)
      (setq start (match-end 0))
      (setq img-path (match-string 1 results) 
	    img-data (base64-encode-string
		      (encode-coding-string
		       (with-temp-buffer
			 (insert-file-contents img-path)
			 (buffer-string))
		       'binary)
		      t))
      (add-to-list 'output-cells `((data . ((image/png . ,img-data)
					    ("text/plain" . "<matplotlib.figure.Figure>")))
				   (metadata . ,(make-hash-table))
				   (output_type . "display_data"))
		   t))
    ;; now remove the inline images and put the results in.
    (setq results (s-trim (replace-regexp-in-string "\\[\\[file:\\(.*?\\)\\]\\]" ""
						    (or results ""))))
    (setq output-cells (append `(((name . "stdout")
				  (output_type . "stream")
				  (text . ,results)))
			       output-cells))
    
    
    `((cell_type . "code")
      (execution_count . 1)
      ;; the hashtable trick converts to {} in json. jupyter can't take a null here.
      (metadata . ,(make-hash-table)) 
      (outputs . ,(if (null output-cells)
		      ;; (vector) json-encodes to  [], not null which
		      ;; jupyter does not like.
		      (vector)
		    (vconcat output-cells)))
      (source . ,(vconcat
		  (list (s-trim (org-element-property :value src-block))))))))


(defun ox-ipynb-filter-latex-fragment (text back-end info)
  "Export fragments the right way for markdown."
  (if (s-starts-with? "\\(" text)
      (format " $%s$ " (substring (s-trim text) 2 -2))
    text))


(defun ox-ipynb-filter-link (text back-end info)
  "Make a link into markdown.
For some reason I was getting angle brackets in them I wanted to remove.
This only fixes file links with no description I think."
  (if (s-starts-with? "<" text)
      (let ((path (substring text 1 -1)))
	(format "[%s](%s)" path path))
    text))


(defun export-ipynb-markdown-cell (beg end)
  "Return the markdown cell for the region defined by BEG and END."
  (let* ((org-export-filter-latex-fragment-functions '(ox-ipynb-filter-latex-fragment))
	 (org-export-filter-link-functions '(ox-ipynb-filter-link))
	 (org-export-filter-keyword-functions '(ox-ipynb-keyword-link)) 
	 (md (org-export-string-as
	      (buffer-substring-no-properties
	       beg end)
	      'md t '(:with-toc nil))))

    `((cell_type . "markdown")
      (metadata . ,(make-hash-table))
      (source . ,(vconcat
		  (list md))))))

(defun export-ipynb-keyword-cell ()
  "Make a markdown cell containing org-file keywords."
  (let* ((keywords (org-element-map (org-element-parse-buffer)
		       'keyword
		     (lambda (key)
		       (cons (org-element-property :key key)
			     (org-element-property :value key))))))
    (loop for key in '("RESULTS" "OPTIONS" "LATEX_HEADER" "ATTR_ORG")
	  do
	  (setq keywords (delq (assoc key keywords) keywords)))

    (setq keywords
	  (loop for (key . value) in keywords
		collect
		(format "- %s: %s\n"
			key
			(replace-regexp-in-string
			 "<\\|>" ""
			 value))))
    (when keywords
      `((cell_type . "markdown")
	(metadata . ,(make-hash-table))
	(source . ,(vconcat keywords))))))

(defun export-ipynb-buffer ()
  "Export the current buffer to ipynb format.
Only ipython source blocks are exported as code cells. Everything
else is exported as a markdown cell.
"
  (interactive)
  (let ((cells (if (export-ipynb-keyword-cell) (list (export-ipynb-keyword-cell)) '()))
	(metadata `(metadata . ((org . ,(org-element-map (org-element-parse-buffer)
					    'keyword
					  (lambda (key)
					    (cons (org-element-property :key key)
						  (org-element-property :value key)))))
				(kernelspec . ((display_name . "Python 3")
					       (language . "python")
					       (name . "python3")))
				(language_info . ((codemirror_mode . ((name . ipython)
								      (version . 3)))
						  (file_extension . ".py")
						  (mimetype . "text/x-python")
						  (name . "python")
						  (nbconvert_exporter . "python")
						  (pygments_lexer . "ipython3")
						  (version . "3.5.2")))))) 
	(ipynb (concat (file-name-base (buffer-file-name)) ".ipynb"))
	src-blocks
	src-results
	current-src
	result
	result-end
	end
	data)

    (setq src-blocks (org-element-map (org-element-parse-buffer) 'src-block
		       (lambda (src)
			 (when (string= "ipython" (org-element-property :language src))
			   src))))

    ;; Get a list of (src . results)
    (setq src-results
	  (loop for src in src-blocks
		with result=nil
		do
		(setq result
		      (save-excursion
			(goto-char (org-element-property :begin src))
			(let ((location (org-babel-where-is-src-block-result nil nil))
			      start end
			      result-content)
			  (when location
			    (save-excursion
			      (goto-char location)
			      (when (looking-at
				     (concat org-babel-result-regexp ".*$")) 
				(setq start (1- (match-beginning 0))
				      end (progn (forward-line 1) (org-babel-result-end))
				      result-content (buffer-substring-no-properties start end))
				;; clean up the results a little. This gets rid
				;; of the RESULTS markers for output and drawers
				(loop for pat in '("#\\+RESULTS:" "^: " "^:RESULTS:\\|^:END:")
				      do
				      (setq result-content (replace-regexp-in-string
							    pat
							    ""
							    result-content)))
				;; the results and the end of the results.
				;; we use the end later to move point.
				(cons (s-trim result-content) end))))))) 
		collect
		(cons src result)))
    
    (setq current-source (pop src-results))

    ;; First block before a src is markdown
    (if (car current-source)
	(unless (string= "" (s-trim (buffer-substring-no-properties (point-min) (org-element-property :begin (car current-source)))))
	  (push (export-ipynb-markdown-cell
		 (point-min) (org-element-property :begin (car current-source)))
		cells))
      (push (export-ipynb-markdown-cell
	     (point-min) (point-max))
	    cells))
    
    (while current-source
      ;; add the src cell
      (push (export-ipynb-code-cell current-source) cells)
      (setq result-end (cdr current-source)
	    result (car result-end)
	    result-end (cdr result-end))
      
      (setq end (max
		 (or result-end 0)
		 (org-element-property :end (car current-source))))
      
      (setq current-source (pop src-results))
      
      (if current-source
	  (when (not (string= "" (s-trim (buffer-substring
					  end
					  (org-element-property :begin
								(car current-source))))))
	    (push (export-ipynb-markdown-cell 
		   end
		   (org-element-property :begin
					 (car current-source)))
		  cells))
	;; on last block so add rest of document
	(push (export-ipynb-markdown-cell end (point-max)) cells)))

    (setq data (append
		`((cells . ,(reverse cells)))
		(list metadata)
		'((nbformat . 4)
		  (nbformat_minor . 0))))
    (with-temp-file ipynb
      (insert (json-encode data)))))



(defun export-ipynb-buffer-and-open ()
  (interactive)
  (export-ipynb-buffer)
  (shell-command (format "nbopen %s.ipynb" (file-name-base))))

(provide 'ox-ipynb)

;;; ox-ipynb.el ends here
