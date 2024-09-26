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

;; * Convenience

;; TODO C-<tab> "Like <tab>, but add a column at end of row instead of new row."

(defun scimax-org-table-insert-column (&optional right)
  "Insert a new column into the table.
Inserts to the left.
With prefix arg, insert to the right."
  (interactive "P")
  (unless (org-at-table-p) (user-error "Not at a table"))
  (when (eobp) (save-excursion (insert "\n")))
  (unless (string-match-p "|[ \t]*$" (org-current-line-string))
    (org-table-align))
  (org-table-find-dataline)
  (let ((col (max 1 (org-table-current-column)))
	(beg (org-table-begin))
	(end (copy-marker (org-table-end)))
	(shrunk-columns (org-table--list-shrunk-columns)))
    (org-table-expand beg end)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
	(unless (org-at-table-hline-p)
	  (org-table-goto-column (if right (+ 1 col) col) t) 
	  (insert "|"))
	(forward-line)))
    (org-table-goto-column col)
    (org-table-align)
    ;; Shift appropriately stored shrunk column numbers, then hide the
    ;; columns again.
    (org-table--shrink-columns (mapcar (lambda (c) (if (< c col) c (1+ c)))
				       shrunk-columns)
			       beg end)
    (set-marker end nil)
    ;; Fix TBLFM formulas, if desirable.
    (when (or (not org-table-fix-formulas-confirm)
	      (funcall org-table-fix-formulas-confirm "Fix formulas? "))
      (org-table-fix-formulas "$" nil (1- col) 1))))

;; need to add hook function to org-shiftmetaleft-hook and
;; org-shiftmetaright-hook to change these.
(defun sot-M-S-left ()
  (when (org-at-table-p)
    (scimax-org-table-insert-column)
    t))

(defun sot-M-S-right ()
  (when (org-at-table-p)
    (scimax-org-table-insert-column t)
    t))

(defun sot-M-S-down ()
  (when (org-at-table-p) 
    (org-table-insert-row t)
    t))

(defun sot-M-S-up ()
  (when (org-at-table-p)
    (org-table-insert-row)
    t))

(progn
  (add-hook 'org-shiftmetaleft-hook #'sot-M-S-left)
  (add-hook 'org-shiftmetaright-hook #'sot-M-S-right)
  (add-hook 'org-shiftmetadown-hook #'sot-M-S-down)
  (add-hook 'org-shiftmetaup-hook #'sot-M-S-up))

;; * Table export hydra

(defun sot-export (fmt target )
  "Export table at point to FMT (string: csv, tsv, latex, html).
TARGET is a symbol: 'buffer or 'file."
  (let ((fname)
	(extension '(("latex" . "tex"))))
    (cond
     ((eq target 'file)
      (setq fname (read-file-name (format "%s: " fmt)
				  nil nil nil
				  (format "tab.%s" (or (cdr (assoc fmt extension))
						       fmt))))
      (org-table-export fname
			(format
			 "orgtbl-to-%s" fmt))
      fname)
     ((eq target 'buffer)
      (setq fname (make-temp-file
		   "sot" nil
		   (concat "." (or (cdr (assoc fmt extension))
				   fmt))))
      (org-table-export fname (concat "orgtbl-to-" fmt))
      (pop-to-buffer (get-buffer-create (concat "*" fmt "*")))
      (erase-buffer)
      (insert-file-contents fname)
      (delete-file fname)))))


(defhydradio scimax-org-table-export-target ()
  (target "To buffer" [buffer file]))


(defhydra scimax-org-table-export (:color blue :hint nil)
  "
_C-b_ Export target (buffer / file) % -15`scimax-org-table-export-target/target

_c_: csv   _t_: tsv  _l_: LaTeX _h_: html
"
  ("C-b" (scimax-org-table-export-target/target) nil :color red)
  ("c" (sot-export "csv" scimax-org-table-export-target/target) "csv")
  ("t" (sot-export "tsv" scimax-org-table-export-target/target) "tsv")
  ("l" (sot-export "latex" scimax-org-table-export-target/target) "latex")
  ("h" (sot-export "html" scimax-org-table-export-target/target) "html"))

(provide 'scimax-org-table)

;;; scimax-org-table.el ends here
