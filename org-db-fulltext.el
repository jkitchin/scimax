;;; org-db-fulltext.el --- Fulltext search for org-db

;;; Commentary:
;;
;; Add fulltext search for org-db.

(defcustom org-db-fulltext "org-db-fulltext.sqlite"
  "Name of the sqlite database file for full-text search."
  :type 'string
  :group 'org-db)


(defvar org-db-ft (emacsql-sqlite (expand-file-name org-db-fulltext org-db-root))
  "Variable for the ‘org-db’ full-text connection.")

;; Make sure the database and table exists
(emacsql org-db-ft [:create-virtual-table :if :not :exists fulltext
					  :using :fts5
					  ([filename contents])])



(defun org-db-fulltext-update (_filename-id _parse-tree)
  "Insert the full text for searching."
  (emacsql org-db-ft [:delete :from fulltext :where (= filename $s1)] (buffer-file-name))

  (emacsql org-db-ft [:insert :into fulltext :values [$s1 $s2]]
	   (buffer-file-name)
	   (save-restriction
	     (widen)
	     (buffer-substring-no-properties (point-min) (point-max)))))


(add-to-list 'org-db-update-functions #'org-db-fulltext-update t)


(defun org-db-fulltext-search ()
  "Search the fulltext database.
This uses ivy right now, since it is so fast. On day I might have
to change this. It could be nice to find some way to read in an
emacsql query like :match '\"something.\" Alternatively, I may
need a better async version."
  (interactive)
  (let ((candidates (emacsql org-db-ft [:select [contents filename] :from fulltext])))
    (ivy-read "query: " candidates)))


(provide 'org-db-fulltext)

;;; org-db-fulltext.el ends here
