;;; org-db-agenda.el --- Agenda from org-db

;;; Commentary:
;;

(require 'org-archive)
(require 'org-db)
(require 'pcache)


(defun org-db-agenda--candidates (before-date)
  "Get headings with deadlines before BEFORE-DATE
Examples:
today Due by today
+1w   Due in a week
+1m   Due in a month
"
  (interactive) 
  (let* ((deadline-headings (with-org-db
			     (emacsql org-db [:select [headlines:level headlines:title headlines:tags
								       files:filename headlines:begin
								       headlines:deadline
								       files:last-updated
								       headlines:todo-keyword]
						      :from headlines
						      :inner :join files
						      :on (= files:rowid headlines:filename-id)
					;only get deadlines before now
						      :where (and
							      (= headlines:todo-keyword "TODO")
							      (is headlines:archivedp (not nil))
							      (< headlines:deadline $s1))
						      :order :by headlines:deadline :desc]
				      (float-time (org-read-date t t before-date)))))
	 
	 (todo-headings (with-org-db
			 (emacsql org-db [:select [headlines:level headlines:title headlines:tags
								   files:filename headlines:begin
								   headlines:deadline
								   files:last-updated
								   headlines:todo-keyword]
						  :from headlines
						  :inner :join files
						  :on (= files:rowid headlines:filename-id)
						  ;; headlines with TODO state but no deadline, and not archived
						  :where (and
							  (= headlines:todo-keyword "TODO")
							  (is headlines:deadline (not nil))
							  (is headlines:archivedp (not nil)))
						  :order :by files:last-updated :desc])))
	 (ignores (pcache-get (pcache-repository :file "org-db-agenda-ignore") 'ignores))
	 (candidates (cl-loop for (level title tags filename begin deadline last-updated todo-keyword)
			      in (append deadline-headings todo-headings)
			      when (not (member filename ignores))
			      collect
			      (cons
			       (format "%28s|%100s|%20s|%s|%s"
				       (s-pad-right 28 " "
						    (if deadline
							(format-time-string
							 "<%Y-%m-%d %a %H:%M:%S>"
							 (seconds-to-time deadline))
						      " "))
				       
				       (s-pad-right 100 " " (concat  (make-string
								      level
								      (string-to-char "*"))
								     " "
								     todo-keyword " "
								     title))
				       (s-pad-right 20 " " (or tags ""))
				       filename last-updated)
			       (list
				:file filename
				:last-updated last-updated
				:begin begin
				:title title)))))
    candidates))


(defun org-db-agenda-transformer (candidate)
  "Add colors to candidates."
  (let* ((now (float-time (current-time)))
	 (ts (string-match org-element--timestamp-regexp candidate))
	 (es (when ts (float-time (org-timestamp-to-time
				   (org-timestamp-from-string
				    (match-string 0 candidate)))))))
    (cond
     ((null es) candidate)
     ((> es now) (propertize candidate 'face '(:foreground "green4")))
     ((< es now) (propertize candidate 'face '(:foreground "dark red"))))))


(ivy-configure 'org-db-agenda :display-transformer-fn
	       #'org-db-agenda-transformer)


(defun org-db-agenda--done (candidate)
  "Mark the current candidate as done."
  (let ((plist (cdr candidate)))
    (find-file (plist-get plist :file))
    (goto-char (plist-get plist :begin))
    (org-todo "DONE")
    (save-buffer)
    (org-db-update-buffer t)))


(defun org-db-agenda--archive (candidate)
  "Mark the current candidate as done."
  (let ((plist (cdr candidate)))
    (find-file (plist-get plist :file))
    (goto-char (plist-get plist :begin))
    (org-archive-set-tag)
    (save-buffer)
    (org-db-update-buffer t)))


(defun org-db-agenda--archive-subtree (candidate)
  "Mark the current candidate as done."
  (let ((plist (cdr candidate)))
    (find-file (plist-get plist :file))
    (goto-char (plist-get plist :begin))
    (org-archive-subtree)
    (save-buffer)
    (org-db-update-buffer t)))


(defun org-db-agenda--ignore (candidate)
  "add CANDIDATE to ignore list."
  (let* ((plist (cdr candidate))
	 (repo (pcache-repository :file "org-db-agenda-ignore"))
	 (ignores (pcache-get repo 'ignores)))
    (setq ignores (delete-dups (append ignores (list (plist-get plist :file)))))
    (pcache-put repo 'ignores ignores)))


(defun org-db-agenda--update (candidate)
  "Update current entry"
  (let* ((plist (cdr candidate))
	 (fname (plist-get plist :file)))
    (with-current-buffer (find-file-noselect fname)
      (org-db-update-buffer t))))


(defun org-db-agenda (before)
  (interactive (list (read-string "Before (e.g. +2w)"  "+2w")))
  (let* ((candidates (org-db-agenda--candidates before)))
    (ivy-read "heading: " candidates
	      :caller 'org-db-agenda
	      :action
	      '(1
		("o" org-db-headings--open "Open to heading.")
		("d" org-db-agenda--done "Mark entry done")
		("a" org-db-agenda--archive "Add archive tag")
		("A" org-db-agenda--archive-subtree "Archive subtree")
		("i" org-db-agenda--ignore "Ignore this file")
		("u" org-db-agenda--update "Update file in database")
		("U" (lambda (_) (org-db-agenda-refresh "+2w")) "Refresh agenda database")))))


(defun scimax-all-headings-done ()
  "Mark all entries in buffer as DONE."
  (interactive)
  (org-map-entries
   (lambda () 
     (when (org-get-repeat)
       (org-todo -1))
     (when (string= (org-get-todo-state) "TODO")
       (org-todo "DONE")))))


(defun scimax-archive-todo-headings ()
  "Make todo entries archived."
  (interactive)
  (org-map-entries
   (lambda ()
     (when (string= (org-get-todo-state) "TODO") 
       (org-archive-set-tag)))))


(defun org-db-agenda-refresh (before)
  "Refresh agenda files."
  (interactive (list (read-string "Before (e.g. +2w)"  "+2w")))
  (cl-loop for candidate in (org-db-agenda--candidates before) do
	   (let* ((fname (plist-get (cdr candidate) :file))
		  (open (get-file-buffer fname))
		  (buf (find-file-noselect fname)))
	     (with-current-buffer buf
	       (org-db-update-buffer t))
	     (when (null open)
	       (kill-buffer buf)))))



(provide 'org-db-agenda)

;;; org-db-agenda.el ends here
