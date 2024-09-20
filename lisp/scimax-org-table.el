;;; scimax-org-table.el --- Scimax utility functions for org tables
;;
;;; Commentary:
;; These work for tables in the current file

;;; Code:

(defun scimax-org-goto-named-table (name)
  "Move point to a table with NAME."
  (let ((pos (catch 'pos
	       (cl-loop for table in  (org-element-map (org-element-parse-buffer) 'table 'identity)
			do
			(when (string= name (org-element-property :name table))
			  (throw 'pos (org-element-property :contents-begin table)))))))
    (when pos (goto-char pos))))


(defun scimax-org-get-named-table (name)
  "Return the table with NAME as Lisp."
  (org-babel-ref-resolve name))


(defun scimax-org-table-row (name row)
  "For the table in NAME return the row number ROW.
This includes hlines, etc."
  (nth row (scimax-org-get-named-table name)))


(defun scimax-org-table-column (name col)
  "For the table in NAME return column number COL."
  (cl-loop for row in (scimax-org-get-named-table name)
	   if (and (sequencep row)
		   (not (string= "!" (car row)))
		   (not (string= "^" (car row)))
		   (not (string= "$" (car row)))
		   (not (string= "_" (car row)))
		   (> (length row) col))
	   collect
	   (nth col row)))


(defun scimax-org-table-local-parameters (name)
  "Get named local parameters in table NAME."
  (save-excursion
    (scimax-org-goto-named-table name)
    (org-table-analyze)
    org-table-local-parameters))

(provide 'scimax-org-table)

;;; scimax-org-table.el ends here
