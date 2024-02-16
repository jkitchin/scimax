;;; scimax-org-feed.el --- A scimax library for consuming org-feeds

;;; Commentary:
;;
;; You need an Emacs with sqlite compiled in, probably version 29+.
;;
;; The idea here is to use org-mode as a feed. You need a url that contains the
;; feed as an org-file. This pulls the org files down and updates a local
;; org-file. You consume the org-feed from that file.
;;
;; Define a list of feeds in `scimax-org-feeds'. Then open the feed file with
;; `scimax-org-feed'. This will download the entries to `scimax-org-feed-file'
;; and cache the entries in `scimax-org-feed-db-file'.
;; 
;; * Consuming the feed
;;
;; You can edit the contents in `scimax-org-feed-file' if you want. They should
;; not get over-written. The intention though is you move headings out of this
;; file and refile them in places. An empty file means you have nothing to
;; review. You can do this with `scimax-org-feed-refile-to-open-org-buffer' to
;; move a heading to an open buffer, or `scimax-org-feed-refile-to-project' to
;; an org-file in a project. There are some speed commands for those, "b" and
;; "p" respectively if your cursor is at the beginning of a heading.
;; `scimax-org-feed-delete-entry' simply deletes it. The original entries are
;; always saved in `scimax-org-feed-db-file'.
;;
;; You can add hook functions to `scimax-org-feed-post-update-hook' that get run
;; in `scimax-org-feed-file' after an update. You might use this to add tags,
;; sort by something, add a score, delete entries, etc. For example, to mark
;; each entry as TODO, you can use (setq scimax-org-feed-post-update-hook
;; '(scimax-org-feed-todo)).
;;
;; * Searching `scimax-org-feed-db-file'
;;
;; You may want to go back to the entries for some reason later.
;; `scimax-org-feed-content-search' allows full-text search of the whole entry,
;; and shows a new buffer of matches. `scimax-org-feed-tag-search' will show you
;; all matching entries for a tag in the database.
;; `scimax-org-feed-property-search' will prompt you for known property. You
;; specify the value to search for. This isn't super helpful at the moment, but
;; here for completeness.
;;
;; RSS to org? use this to consume RSS feeds? Somthing like this has been done
;; here: https://orgmode.org/worg/org-contrib/org-feed.html
;;
;; Should there be a filter mechanism to prevent adding entries?
;;
;; [2024-02-05 Mon] This is still alpha code and I expect to make some changes.
;; For example, I use the md5sum of content instead of a uuid as the unique key.
;; That is fragile if the upstream content changes, and RSS uses a uuid instead.
;; I don't really want to generate these at the moment though.

;;; Code:

(defcustom scimax-org-feeds
  nil
  "A list of urls that point to org-files."
  :type '(list string)
  :group 'scimax-org-feed)


(defcustom scimax-org-feed-file
  (expand-file-name "scimax-org-feed.org" "~/.cache")
  "A filename for storing the feeds in."
  :type 'file
  :group 'scimax-org-feed)


(defcustom scimax-org-feed-db-file
  (concat scimax-org-feed-file ".sqlite")
  "Filename for the db cache."
  :type 'file
  :group 'scimax-org-feed)


(defcustom scimax-org-feed-speed-commands
  '(("r" . scimax-org-feed-refile-to-project)
    ("b" . scimax-org-feed-refile-to-open-org-buffer)
    ("d" . scimax-org-feed-delete-entry)
    ("a" . scimax-org-feed-done-and-archive))
  "List of speed commands.
These only operate on headlines in `scimax-org-feed-file'"
  :type '(list (cons string function))
  :group 'scimax-org-feed)


(defcustom scimax-org-feed-post-update-hook
  '()
  "Hook functions to run after an update.
Each function is run with no arguments in `scimax-org-feed-file'.
Some typical ideas would be to map over the org-entries and add
additional tags, score them, etc."
  :type '(list function)
  :group 'scimax-org-feed)


(defun scimax-org-feed-todo ()
  "Sample function to mark all entries with TODO.
Meant to be used in `scimax-org-feed-post-update-hook'."
  (org-map-entries
   (lambda ()
     (org-todo "TODO"))))


(defun scimax-org-feed-speed-keys (keys)
  "Find the command to run for KEYS."
  (when (and (string= scimax-org-feed-file
		      (or (buffer-file-name (current-buffer)) ""))
	     (and (bolp) (looking-at org-outline-regexp)))
    (cdr (assoc keys scimax-org-feed-speed-commands))))


(add-hook 'org-speed-command-hook 'scimax-org-feed-speed-keys)


;; * setup db
;; [[info:elisp#Database]]

;; Tables:
;; entries: (md5 content)
;; entry_tags: (md5 tag)
;; entry_property: (md5 propertyid value)

(defun scimax-org-feed-setup-db ()
  "Create the db."
  (let ((scimax-org-feed-db  (sqlite-open  scimax-org-feed-db-file)))
    (sqlite-execute scimax-org-feed-db "create virtual table if not exists entries using fts5 (md5, content, date_added)")
    (sqlite-execute scimax-org-feed-db "create table if not exists entry_tag (md5, tag)")
    (sqlite-execute scimax-org-feed-db "create table if not exists entry_property (md5, property, value)")
    (sqlite-close scimax-org-feed-db)))


(scimax-org-feed-setup-db)


(defun scimax-org-feed-reset-db ()
  "Remove and re-create the db."
  (interactive)
  (delete-file scimax-org-feed-db-file)
  (scimax-org-feed-setup-db))


(defun scimax-org-feed-update ()
  "Update all the feeds."
  (let ((scimax-org-feed-db (sqlite-open  scimax-org-feed-db-file)))
    (cl-loop for feed-url in scimax-org-feeds do
	     (let* ((org-feed-content (with-current-buffer
					  (url-retrieve-synchronously
					   feed-url)
					(buffer-substring
					 url-http-end-of-headers (point-max))))
		    (entries (with-temp-buffer
			       (insert org-feed-content)
			       (org-mode)
			       ;; map over entries and get entry properties
			       (cl-loop for el in (reverse
						   (org-element-map
						       (org-element-parse-buffer)
						       'headline 'identity))
					collect
					(progn
					  (goto-char (org-element-property :begin el))
					  (let* ((tags (mapcar 'org-no-properties (org-get-tags)))
						 (properties (org-entry-properties
							      (org-element-property :begin el) 'all))
						 (content (buffer-substring
							   (org-element-property :begin el)
							   (org-element-property :end el)))
						 (md5 (md5 content)))
					    (list :content content
						  :tags tags
						  :properties properties
						  :md5 md5)))))))
	       
	       (with-current-buffer (find-file-noselect scimax-org-feed-file)
		 (set-buffer-file-coding-system 'utf-8)
		 (cl-loop for entry in entries do
			  (let ((md5 (plist-get entry :md5))
				(content (plist-get entry :content)))
			    ;; this means we do not have the entry, so we insert
			    ;; it, and save the entry
			    (unless (sqlite-select scimax-org-feed-db
						   "select * from entries where md5=?"
						   (list md5))
			      (goto-char (point-max))
			      (insert (plist-get entry :content))
			      (sqlite-execute scimax-org-feed-db
					      "insert into entries (md5, content, date_added) values (?, ?, datetime('now','localtime'))"
					      (list md5 content))

			      ;; update tags
			      (cl-loop for tag in (plist-get entry :tags)
				       do
				       (sqlite-execute scimax-org-feed-db
						       "insert into entry_tag (md5, tag) values (?, ?)"
						       (list md5 tag)))
			      
			      ;; update properties
			      (cl-loop for (property . value) in (plist-get entry :properties)
				       do
				       (when value
					 (sqlite-execute
					  scimax-org-feed-db
					  "insert into entry_property (md5, property, value) values (?, ?, ?)"
					  (list md5 property value))))))))))
    (sqlite-close scimax-org-feed-db)
    (with-current-buffer (find-file-noselect scimax-org-feed-file)
      ;; This fixes some display issues where raw unicode characters are shown
      ;; instead of the glyphs.
      (let ((coding-system-for-write 'utf-8)
    	    (coding-system-require-warning nil)
    	    (current-prefix-arg nil)
	    (revert-without-query (list scimax-org-feed-file)))
	(save-buffer)
	(revert-buffer-with-coding-system 'utf-8))
      (run-hooks 'scimax-org-feed-post-update-hook))))


(defun scimax-org-feed-header ()
  "Add/update a header with number of entries."
  (setq header-line-format
	(format "%s entries - Click to update" (count-matches org-heading-regexp (point-min) (point-max)))))


(local-set-key [header-line down-mouse-1]
	       `(lambda ()
		  (interactive)
		  (scimax-org-feed-header)))


(defun scimax-org-feed ()
  "Open the `scimax-org-feed-file'."
  (interactive)
  (pop-to-buffer (find-file-noselect scimax-org-feed-file))
  (scimax-org-feed-update)
  (scimax-org-feed-header)
  (goto-char (point-min)))


;; * Acting on entries: delete, refile
;; Convenience functions, mostly for use as speed keys.

(defun scimax-org-feed-delete-entry ()
  "Delete the org-entry at point.
This is different than killing the entry, which puts it on your
clipboard."
  (interactive)
  (unless (org-at-heading-p)
    (org-previous-visible-heading 1))
  (let ((hl (org-element-context)))
    (cl--set-buffer-substring (org-element-property :begin hl)
			      (org-element-property :end hl)
			      ""))
  (scimax-org-feed-header))


(defun scimax-org-feed-done-and-archive ()
  "Mark the entry done and archive it.
I don't like TODO items in the archive, so this must marks them
done and archives it at once."
  (org-todo "DONE")
  (org-archive-subtree)
  (scimax-org-feed-header))


(defun scimax-org-feed-refile-to-open-org-buffer ()
  "Refile current heading to a heading in open org-buffer."
  (interactive)
  (let* ((org-file (ivy-read "Buffer: " (cl-loop for buf in (buffer-list)
						 if (and (buffer-file-name buf)
							 (string= "org"
								  (file-name-extension
								   (buffer-file-name buf))))
						 collect (buffer-file-name buf))))
	 (headlines (with-current-buffer (find-file-noselect org-file)
		      (save-excursion
			(let ((hl '()))
			  (goto-char (point-min))
			  (while (re-search-forward org-heading-regexp nil t)
			    (cl-pushnew
			     (list
			      (format "%-80s (%s)"
				      (match-string 0)
				      (file-name-nondirectory org-file))
			      :file org-file
			      :headline (match-string 0)
			      :position (match-beginning 0))
			     hl))
			  hl))))
	 (selection (ivy-read "Heading: " headlines))
	 (candidate (cdr (assoc selection headlines)))
	 (rfloc (list
		 (plist-get candidate :headline)
		 (plist-get candidate :file)
		 nil
		 (plist-get candidate :position))))
    
    (org-refile nil nil rfloc))
  (scimax-org-feed-header))


(defun scimax-org-feed-refile-to-project ()
  "Refile current heading to a heading in a known project."
  (interactive)
  (let* ((default-directory (ivy-read "Project: " projectile-known-projects))
	 (org-files (mapcar
		     (lambda (f) (expand-file-name f (projectile-project-root)))
		     (-filter (lambda (f)
				(and
				 (f-ext? f "org")
				 (not (s-contains? "#" f))))
			      (projectile-current-project-files))))
	 (headlines (cl-loop for file in org-files
			     append
			     (let ((hl '()))
			       
			       (when (file-exists-p file)
				 (with-temp-buffer
				   (insert-file-contents file)
				   ;; (org-mode)
				   ;; (font-lock-ensure)
				   (goto-char (point-min))
				   (while (re-search-forward org-heading-regexp nil t)
				     (cl-pushnew
				      (list
				       (format "%-80s (%s)"
					       (match-string 0)
					       (file-name-nondirectory file))
				       :file file
				       :headline (match-string 0)
				       :position (match-beginning 0))
				      hl))))
			       hl)))
	 (selection (ivy-read "Heading: " headlines))
	 (candidate (cdr (assoc selection headlines)))
	 (rfloc (list
		 (plist-get candidate :headline)
		 (plist-get candidate :file)
		 nil
		 (plist-get candidate :position))))
    (org-refile nil nil rfloc))
  (scimax-org-feed-header))



;; * DB search functions

(defun scimax-org-feed-content-search (query)
  "Open a buffer with full-text search matches for QUERY.
See https://www.sqlite.org/fts5.html for query syntax."
  (interactive "sQuery: ")
  (let ((buf (get-buffer-create "*scimax-org-feed-search*")))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for result in
	       (let ((scimax-org-feed-db  (sqlite-open  scimax-org-feed-db-file)))
		 (prog1
		     (sqlite-execute scimax-org-feed-db
				     "select content from entries where content match ?"
				     (list query))
		   (sqlite-close scimax-org-feed-db)))
	       
	       do
	       (insert (car result)))
      (org-mode)
      (scimax-org-feed-header))
    (pop-to-buffer buf)
    (goto-char (point-min))))


(defun scimax-org-feed-tag-search ()
  "Open a buffer with all entries with a tag."
  (interactive)
  (let* ((scimax-org-feed-db  (sqlite-open scimax-org-feed-db-file))
	 (tag (ivy-read "Tag: " (mapcar 'car (sqlite-execute
					      scimax-org-feed-db
					      "select distinct tag from entry_tag"))))
	 (md5s (let ()
		 (prog1
		     (mapcar 'car
			     (sqlite-execute scimax-org-feed-db
					     "select distinct md5 from entry_tag where tag=?"
					     (list tag)))
		   (sqlite-close scimax-org-feed-db))))
	 (buf (get-buffer-create "*scimax-org-feed-tag*"))
	 (scimax-org-feed-db  (sqlite-open  scimax-org-feed-db-file)))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for md5 in md5s
	       do
	       (insert (caar (sqlite-execute scimax-org-feed-db
					     "select content from entries where md5=?"
					     (list md5)))))
      (org-mode)
      (scimax-org-feed-header))
    (sqlite-close scimax-org-feed-db)
    (pop-to-buffer buf)
    (goto-char (point-min))))


(defun scimax-org-feed-property-search ()
  "Show a buffer of matching properties.
The search on property value is done with LIKE.
Use % for a wildcard."
  (interactive)
  (let* ((scimax-org-feed-db  (sqlite-open scimax-org-feed-db-file))
	 (property (ivy-read "Property: " (mapcar 'car (sqlite-execute
							scimax-org-feed-db
							"select distinct property from entry_property"))))
	 (value (read-string "Value: "))
	 (md5s (let ()
		 (prog1
		     (mapcar 'car
			     (or
			      (sqlite-execute
			       scimax-org-feed-db
			       "select distinct md5 from entry_property where property=? and value like ?"
			       (list property value))
			      '()))
		   (sqlite-close scimax-org-feed-db))))
	 (buf (get-buffer-create "*scimax-org-feed-property*"))
	 (scimax-org-feed-db  (sqlite-open  scimax-org-feed-db-file)))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for md5 in md5s
	       do
	       (insert (caar (sqlite-execute scimax-org-feed-db
					     "select content from entries where md5=?"
					     (list md5)))))
      (org-mode)
      (scimax-org-feed-header))
    (sqlite-close scimax-org-feed-db)
    (pop-to-buffer buf)
    (goto-char (point-min))))


;; ** searching by date
;; Leaving this here so I don't forget.
(defun scimax-org-feed-date (start end)
  "Show entries in the database for dates from START to END.
These are dates when the entries were added to the database.
These are not the same as publication dates, but they should be
close. Publication dates are not totally reliable, and the dates
you add them are."
  (interactive
   (list (org-read-date nil nil nil "Start: ")
	 (org-read-date nil nil nil "End: ")))
  
  (let* ((scimax-org-feed-db (sqlite-open  scimax-org-feed-db-file))
	 (entries (sqlite-select scimax-org-feed-db"select content from entries where date_added >= date(?) and date_added <= date(?)"
				 (list start end)))
	 (buf (get-buffer-create "*scimax-org-feed-date*")))
    (with-current-buffer buf
      (erase-buffer)
      (cl-loop for entry in entries
	       do
	       (insert (car entry)))
      (org-mode)
      (scimax-org-feed-header))
    (sqlite-close scimax-org-feed-db)
    (pop-to-buffer buf)
    (goto-char (point-min))))





(provide 'scimax-org-feed)

;;; scimax-org-feed.el ends here