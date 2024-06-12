;;; org-db-agenda.el --- Agenda from org-db

;;; Commentary:


(require 'org-archive)
(require 'org-db)
(require 'calendar)
(require 'color)


(defun org-db-agenda-setup ()
  (with-org-db
   (sqlite-execute org-db "create table if not exists agenda_ignores(
fname text)")))


(defun org-db-agenda--candidates (before-date)
  "Get headings with deadlines before BEFORE-DATE
Examples:
today Due by today
+1w   Due in a week
+1m   Due in a month
"
  (interactive) 
  (let* ((deadline-headings (with-org-db
			     (sqlite-select org-db "select
headlines.level, headlines.title, headlines.tags,
files.filename, headlines.begin,
strftime('<%Y-%m-%d %H:%M:%S>', headlines.deadline),
files.last_updated, headlines.todo_keyword, headlines.priority
from headlines
inner join files on files.rowid = headlines.filename_id
where headlines.todo_keyword = \"TODO\"
-- and headlines.archivedp is null
and headlines.deadline < date(?)
order by headlines.deadline desc
"
					    (list (org-format-time-string
						   "%Y-%m-%d %H:%M:%S"
						   (org-read-date t t before-date))))))
	 
	 (scheduled-headings (with-org-db
			      (sqlite-select org-db "select
headlines.level,
format('%s (scheduled)', headlines.title),
headlines.tags,
files.filename, headlines.begin,
strftime('<%Y-%m-%d %H:%M:%S>', headlines.scheduled),
files.last_updated, headlines.todo_keyword, headlines.priority
from headlines
inner join files on files.rowid = headlines.filename_id
where headlines.todo_keyword = \"TODO\"
-- and headlines.archivedp is null
and headlines.scheduled < date(?)
order by headlines.scheduled desc
"
					     (list (org-format-time-string
						    "%Y-%m-%d %H:%M:%S"
						    (org-read-date t t before-date))))))
	 
	 (todo-headings (with-org-db
			 (sqlite-select org-db "select
headlines.level, headlines.title, headlines.tags,
files.filename, headlines.begin,
null,
files.last_updated, headlines.todo_keyword, headlines.priority
from headlines
inner join files on files.rowid = headlines.filename_id
where headlines.todo_keyword = \"TODO\"
-- and headlines.archivedp is null
and headlines.scheduled is null
and headlines.deadline is null
")))
	 (ignores (mapcar 'car (with-org-db
				(sqlite-execute org-db "select * from agenda_ignores"))))
	 (candidates (cl-loop for (level title tags filename begin deadline last-updated todo-keyword priority)
			      in (-uniq (append deadline-headings scheduled-headings
						todo-headings))
			      when (not (member filename ignores))
			      collect
			      (cons
			       (format "%28s|%100s|%20s|%s|%s"
				       ;; deadline
				       (s-pad-right 28 " "
						    (or deadline " "))
				       ;; title
				       (s-pad-right 100 " " (concat  (make-string
								      level
								      (string-to-char "*"))
								     (if priority
									 (format " [#%s] " priority)
								       " ")
								     todo-keyword " "
								     title))
				       (s-pad-right 20 " " (or tags ""))
				       filename last-updated)
			       (list
				:file filename
				:deadline deadline
				:last-updated last-updated
				:begin begin
				:title title)))))
    (sort candidates (lambda (a b)
		       (let ((d1 (plist-get (cdr a) :deadline))
			     (d2 (plist-get (cdr b) :deadline)))
			 (org-time< d1 d2))))))


(defun org-db-agenda-transformer (candidate)
  "Add colors to candidates.
Things that are overdue are dark red, and things todo are in green.
CANDIDATE is a string, possibly with a timestamp in it."
  (let* ((now (float-time (current-time)))
	 ;; use a regexp to find the timestamp
	 (ts (string-match org-element--timestamp-regexp candidate))
	 ;; if we have a timestamp, convert it to time
	 (es (when ts (float-time (org-timestamp-to-time
				   (org-timestamp-from-string
				    (match-string 0 candidate)))))))
    (cond
     ((null es) candidate)
     ;; Make priority red
     ((string-match "\\[#A\\]" candidate) (propertize candidate 'face '(:foreground "red1" :weight bold)))
     ((string-match "scheduled" candidate) (propertize candidate 'face '(:foreground "DarkOrange3")))
     ;; Calendar entries are not meant to be changed
     ((string-match ":gcal:" candidate) (propertize candidate 'face '(:foreground "DodgerBlue3")))
     
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
  "Archive the CANDIDATE headline."
  (let ((plist (cdr candidate)))
    (find-file (plist-get plist :file))
    (goto-char (plist-get plist :begin))
    (org-archive-set-tag)
    (save-buffer)
    (org-db-update-buffer t)))


(defun org-db-agenda--archive-subtree (candidate)
  "Archive the subtree indicated by CANDIDATE."
  (let ((plist (cdr candidate)))
    (find-file (plist-get plist :file))
    (goto-char (plist-get plist :begin))
    (org-archive-subtree)
    (save-buffer)
    (org-db-update-buffer t)))


(defun org-db-agenda--ignore (candidate)
  "Add CANDIDATE to ignore list."
  (let* ((plist (cdr candidate)))
    (with-org-db
     (sqlite-execute org-db "insert into agenda_ignores values(?)"
		     (list (plist-get plist :file))))))


(defun org-db-agenda--update (candidate)
  "Update current entry"
  (let* ((plist (cdr candidate))
	 (fname (plist-get plist :file)))
    (if (file-exists-p fname)
	(with-current-buffer (find-file-noselect fname)
	  (org-db-update-buffer t))
      (org-db-remove-file fname))))


(defun org-db-agenda--remove (candidate)
  (let* ((plist (cdr candidate))
	 (fname (plist-get plist :file)))
    (org-db-remove-file fname)))


(defun org-db-agenda-marked-candidates (candidates)
  (let* ((actions (cl-loop for (key cmd doc) in 
			   (cdr (ivy-state-action ivy-last))
			   collect
			   (cons doc cmd)))
	 (cmd (cdr (assoc (completing-read "action: " actions) actions))))
    
    (cl-loop for candidate in candidates do 
	     (funcall cmd candidate))
    (setq ivy-marked-candidates nil)))


(defun org-db-agenda (before)
  (interactive (list (read-string "Before (e.g. +2w)"  "+2w")))
  (let* ((candidates (org-db-agenda--candidates before)))
    (ivy-read "heading: " candidates
	      :caller 'org-db-agenda
	      :multi-action #'org-db-agenda-marked-candidates
	      :action
	      '(1
		("o" org-db-headings--open "Open to heading")
		("O" org-db-ogenda "open in org-agenda")
		("a" (lambda (_) (call-interactively #'org-db-agenda))  "New interval")
		("d" org-db-agenda--done "Mark entry done")
		("v" org-db-agenda--archive "Add archive tag")
		("V" org-db-agenda--archive-subtree "Archive subtree")
		("i" org-db-agenda--ignore "Ignore this file")
		("u" org-db-agenda--update "Update file in database")
		("r" org-db-agenda--remove "Remove file")
		("c" (lambda (_) (org-db-agenda-calendar-view)) "Calendar view")
		("q" (lambda (_)
		       (org-db-process-queue t)
		       (call-interactively #'org-db-agenda))
		 "Process queue")))))


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


;; Note this did not work as well as I hoped. Maybe some things happen like
;; updating the deadline after this is done, like updating the CLOSED property.
;; It is also pretty slow to update a buffer, so it feels laggy when using it.
;; Leaving it here to remind me I tried it.

;; (defun org-db-todo-hook-fn ()
;;   "Run when you change a TODO state.
;; Triggers updating the buffer so your agenda is up to date."
;;   (org-db-update-buffer t))

;; (add-hook  'org-after-todo-state-change-hook 'org-db-todo-hook-fn)

;; * org-db-ogenda
;; use org-db to set `org-agenda-files' then use org-agenda

(defun org-db-ogenda (t1 t2)
  (interactive (list
		(org-read-date nil nil nil "First date: ")
		(org-read-date nil nil nil "Second date: ")))
  (let ((org-agenda-files (cl-delete-duplicates
			   (flatten-list (with-org-db
					  (sqlite-select org-db "select (files.filename) from headlines
inner join files on files.rowid = headlines.filename_id
where headlines.deadline > date(?)
and headlines.deadline < date(?)
and headlines.todo_keyword = \"TODO\""
							 (list t1 t2))))
			   :test #'equal)))
    (org-agenda)))


;; * calendar view

(defun org-db-agenda-mark-calendar ()
  "marks days in the calendar when there are things due"
  (cl-loop for group in 
	   (seq-group-by #'identity
			 (with-org-db
			  (sqlite-select org-db "select
strftime('%Y', headlines.deadline),
strftime('%m', headlines.deadline),
strftime('%d', headlines.deadline)
from headlines
where headlines.todo_keyword = \"TODO\"
and headlines.deadline is not null
order by headlines.deadline desc")))
	   collect (cons (car group) (length group))
	   do
	   (let* ((g (car group))
		  (year (nth 0 g))
		  (month (nth 1 g))
		  (day (nth 2 g))
		  (count (length group))
		  ;; delta * 4%
		  (percent (* (- count  2) 4))
		  (color)
		  (d (mapcar 'string-to-number (list month day year)))
		  (calendar-date-echo-text (format "test %d" count)))
	     (when (calendar-date-is-visible-p d)
	       (save-excursion
		 (calendar-cursor-to-visible-date d)
		 (setq color (if (time-less-p (org-read-date nil t (string-join (car group) "-"))
					      (org-read-date nil t "+1w"))
				 (color-darken-name "red" percent)
			       (color-darken-name "DarkOliveGreen4" percent)))
		 (add-text-properties (1- (point)) (1+ (point))
				      `(font-lock-face '(:foreground ,color :weight bold)
						       help-echo (format "%s tasks" ,count))))))))


(defun org-db-agenda-calendar-view ()
  "Show agenda in calendar view."
  (interactive)
  (let ((calendar-today-visible-hook))
    (add-hook 'calendar-today-visible-hook
	      'org-db-agenda-mark-calendar)
    ;; What do we do with the selected date? This adds a day to what you
    ;; selected which then shows entries on that day or earlier.
    ;; (org-db-agenda
    ;;  (org-format-time-string
    ;;   "%Y-%m-%d %H:%M:%S"
    ;;   (time-add (org-read-date t t) (* 60 60 24))))
    (let* ((selection (org-read-date t t))
	   (entries (with-org-db
		     (sqlite-select org-db "select
headlines.level, headlines.title, headlines.tags,
files.filename, headlines.begin,
strftime('<%Y-%m-%d %H:%M:%S>', headlines.deadline),
files.last_updated, headlines.todo_keyword
from headlines
inner join files on files.rowid = headlines.filename_id
where headlines.todo_keyword = \"TODO\"
-- and headlines.archivedp is null
and headlines.deadline > date(?)
and headlines.deadline < date(?)
order by headlines.deadline desc
"
				    (list (format-time-string "%Y-%m-%d" selection)
					  (format-time-string "%Y-%m-%d" (time-add
									  selection
									  (* 60 60 24)))))))
	   (candidates (cl-loop for (level title tags filename begin deadline last-updated todo-keyword)
				in entries
				collect
				(cons
				 (format "%28s|%100s|%20s|%s|%s"
					 (s-pad-right 28 " "
						      (or deadline " "))
					 
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
				  :deadline deadline
				  :last-updated last-updated
				  :begin begin
				  :title title)))))
      (ivy-read "Agenda: " candidates
		:caller 'org-db-agenda
		:action '(1
			  ("o" org-db-headings--open "Open to heading")
			  ("O" org-db-ogenda "open in org-agenda")
			  ("a" (lambda (_) (call-interactively #'org-db-agenda))  "New interval")
			  ("d" org-db-agenda--done "Mark entry done")
			  ("v" org-db-agenda--archive "Add archive tag")
			  ("V" org-db-agenda--archive-subtree "Archive subtree")
			  ("i" org-db-agenda--ignore "Ignore this file")
			  ("u" org-db-agenda--update "Update file in database")
			  ("r" org-db-agenda--remove "Remove file")
			  ("c" (lambda (_) (org-db-agenda-calendar-view)) "Calendar view")
			  ("q" (lambda (_)
				 (org-db-process-queue t)
				 (call-interactively #'org-db-agenda))
			   "Process queue"))))))



(provide 'org-db-agenda)

;;; org-db-agenda.el ends here
