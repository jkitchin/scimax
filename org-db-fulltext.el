;;; org-db-fulltext.el --- Fulltext search for org-db

;;; Commentary:
;;
;; Add fulltext search for org-db.
;;
;; TODO replace org-db-ft

(require 'org-db)

(defcustom org-db-fulltext "org-db-fulltext.sqlite"
  "Name of the sqlite database file for full-text search."
  :type 'string
  :group 'org-db)


(defvar org-db-ft nil
  "Variable for the ‘org-db’ full-text connection.")


(defmacro with-org-db-ft (&rest body)
  "Run BODY commands in an org-db context where we open the database, and close it at the end.
BODY should be regular commands. They can use the variable `org-db' in them."
  `(progn
     (unless (and org-db-ft (emacsql-live-p org-db-ft))
       (setq org-db-ft (emacsql-sqlite (expand-file-name org-db-fulltext org-db-root) :debug t)))
     (prog1
	 (progn
	   ,@body)
       (emacsql-close org-db-ft)
       (emacsql-close org-db-ft)
       (setq org-db-ft nil)))
  
  ;; `(let* ((org-db-ft (emacsql-sqlite (expand-file-name org-db-fulltext org-db-root)))
  ;; 	  (results))
  ;;    (prog1
  ;; 	 (condition-case err
  ;; 	     ,@body
  ;; 	   (org-db-log "Error fulltext: %s" err))
  ;;      ;; It seems like you need to call this twice before org-db is nil
  ;;      (emacsql-close org-db-ft)
  ;;      (emacsql-close org-db-ft)))
  )

;; Make sure the database and table exists
(with-org-db-ft
 (emacsql org-db-ft [:create-virtual-table :if :not :exists fulltext
					   :using :fts5
					   ([filename contents])]))



(defun org-db-fulltext-update (_filename-id _parse-tree)
  "Insert the full text for searching."
  (with-org-db-ft
   (emacsql org-db-ft [:delete :from fulltext :where (= filename $s1)] (buffer-file-name))

   (emacsql org-db-ft [:insert :into fulltext :values [$s1 $s2]]
	    (buffer-file-name)
	    (save-restriction
	      (widen)
	      (buffer-substring-no-properties (point-min) (point-max))))))


(add-to-list 'org-db-update-functions #'org-db-fulltext-update t)


(defun org-db-fulltext-search ()
  "Search the fulltext database.
This uses ivy right now, since it is so fast. On day I might have
to change this. It could be nice to find some way to read in an
emacsql query like :match '\"something.\" Alternatively, I may
need a better async version."
  (interactive)
  (let ((candidates (with-org-db-ft
		     (emacsql org-db-ft [:select [contents filename] :from fulltext]))))
    (ivy-read "query: " candidates
	      :action (lambda (x)
			(find-file (cadr x))
			(re-search-forward ivy-text)
			(goto-char (match-beginning 0))))))


(defun org-db-ft-transformer (s)
  "This only shows lines that match the selected pattern."
  (s-join "\n"
	  (cl-loop for x in (split-string s "\n")
		   if (string-match-p ivy-text x)
		   collect x)))


(ivy-set-display-transformer
 'org-db-fulltext-search
 'org-db-ft-transformer)


(provide 'org-db-fulltext)

;;; org-db-fulltext.el ends here
