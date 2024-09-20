;;; org-db-fulltext.el --- Fulltext search for org-db

;;; Commentary:
;;
;; Add fulltext search for org-db. There are two main entry points.
;;
;; `org-db-fulltext-search' uses fts5 MATCH syntax for searching with dynamic
;; candidate completion. The candidates are short snippets of context.
;;
;; `org-db-fulltext-search-ivy' uses ivy on the full candidates, with ivy
;; matching. It is surprisingly fast. I like this one best.

(require 'org-db)

(defcustom org-db-fulltext "org-db-fulltext-v2.sqlite"
  "Name of the sqlite database file for full-text search."
  :type 'file
  :group 'org-db)


(defun org-db-fulltext-setup ()
  ;; Make sure the database and table exists
  (let ((org-db-ft (sqlite-open (expand-file-name org-db-fulltext org-db-root))))
    (sqlite-execute org-db-ft "create virtual table if not exists fulltext using fts5(filename, contents)")
    (sqlite-close org-db-ft)))


(defun org-db-fulltext-update (_filename-id _parse-tree _org-db)
  "Insert the full text for searching.
This does not use the arguments pased in update functions."
  (org-db-fulltext-setup)
  (let ((org-db-ft (sqlite-open (expand-file-name org-db-fulltext org-db-root))))
    (sqlite-execute org-db-ft "delete from fulltext where filename = ?"
		    (list (buffer-file-name)))
    (sqlite-execute org-db-ft "insert into fulltext values (?, ?)"
		    (list (buffer-file-name)
			  (save-restriction
			    (widen)
			    (buffer-substring-no-properties (point-min) (point-max)))))
    (sqlite-close org-db-ft)))


(add-to-list 'org-db-update-functions #'org-db-fulltext-update t)


(defun org-db-fulltext-candidates (query)
  "Find candidates for fulltext search.
QUERY should be in fts5 syntax. We use MATCH for the select.
https://www.sqlite.org/fts5.html"
  (or
   (ivy-more-chars) 
   (let ((org-db-ft (sqlite-open (expand-file-name org-db-fulltext org-db-root)))
	 (statement (format "select snippet(fulltext, 1, '', '', '', 8), filename
from fulltext where contents match '%s'%s"
			    query
			    (if org-db-project-p
				(format "  and filename match '\"^%s\"'" (projectile-project-root))
			      ""))))
     
     (prog1
	 (cl-loop for (snippet fname) in
		  (sqlite-select org-db-ft statement)
		  ;; I don't love this, but for some reason in the ivy command,
		  ;; I don't get a cons result, so this is the only way I know
		  ;; to get the filename. Its probably not even reliable because
		  ;; the full text might have :: in it.
		  collect
		  ;; (cons snippet fname)
		  (format "%s :: %s" snippet fname))
       (sqlite-close org-db-ft)))))


(defun org-db-fulltext-open (cand)
  "Open the file in CAND."
  (cl-destructuring-bind (snippet fname)
      (mapcar 'string-trim (split-string cand "::"))
    (find-file fname)
    (goto-char (point-min))
    (search-forward snippet nil t)))


(defvar org-db-project-p nil
  "Boolean for limiting search to current project.")

(defun org-db-fulltext-search (&optional project)
  "Search the fulltext database.
This function opens the file that matches your query string.
With optional prefix arg PROJECT limit query to current project."
  (interactive "P")
  (setq org-db-project-p project)
  (ivy-read "query: " #'org-db-fulltext-candidates 
	    :dynamic-collection t
	    :action #'org-db-fulltext-open))


(defun org-db-fulltext-search-ivy ()
  "Search the fulltext database.
This function opens the file that matches your query string."
  (interactive)
  ;; this uses ivy for completion. 
  (let ((candidates (let ((org-db-ft (sqlite-open (expand-file-name org-db-fulltext org-db-root))))
		      (prog1
			  (sqlite-select org-db-ft "select contents, filename from fulltext")
			(sqlite-close org-db-ft)))))
    
    (ivy-read "query: " candidates 
	      
	      :action #'org-db-fulltext-open)))


(defun org-db-ft-transformer (s)
  "This only shows lines that match the selected pattern."
  (s-join "\n"
	  (cl-loop for x in (split-string s "\n")
		   if (string-match-p ivy-text x)
		   collect x)))


(ivy-set-display-transformer
 'org-db-fulltext-search-ivy
 'org-db-ft-transformer)


(provide 'org-db-fulltext)

;;; org-db-fulltext.el ends here
;; Local Variables:
;; eval: (sem-mode)
;; End:
