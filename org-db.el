;;; org-db.el --- An org database

;;; Commentary:
;; org-db is used to index org-files into a sqlite database to make searching
;; easier. It is complementary to things like `org-agenda-files'. I have found
;; that `org-agenda' is too slow to deal with large (thousands+) of org-files
;; that are spread all over your directories.
;;
;; It works with a `save-buffer-hook' that runs a function that adds an org-file
;; to a queue for indexing when you save it. The queue is processed whenever
;; Emacs is idle. This is done for performance reasons.
;; 1. org-db parses the org-file which can be slow for large files, and I don't want to wait while working.
;; 2. sqlite can only have one process working on it at a time, so async updates is not a good idea.
;;
;; org-db balances performance and accuracy in a way that works "well enough"
;; for me. There are a number of ways it can be out of sync and inaccurate
;; though. The main way is if files get changed outside of emacs, e.g. by git,
;; cloud drive sync programs, or other users in shared drives, files are moved
;; or renamed, etc. org-db doesn't have a good way to keep up with these kinds
;; of changes. You can use `M-x C-u org-db-refresh' to update all the files in
;; the database.
;;
;; Similarly, org-db will generally not know about files you have never opened.
;;
;; The main entry points for you are:
;; `org-db-headings' actions for headings in the database - default open
;; `org-db-contacts' actions for headings with an EMAIL property - default open
;; `org-db-locations' actions for headings with an ADDRESS property - default open
;; `org-db-files'  open a file in the db
;; `org-db-recentf' open a recent file in the db
;; `org-db-links' actions for links in the db
;; `org-db-hashtags' actions for hashtags
;;
;; `org-db-toggle-org-id' If you want to use org-db to jump to an org-id instead
;; of `org-id-goto'. I find the built-in function to slow for me.
;;
;; Utilities
;; `org-db-index' will prompt you for a directory and index the org-files in that directory. Use a prefix arg to make it recursive.
;; `org-db-clean-db' will prune entries where the file no longer exists.
;; `org-db-reset-db' will clear all the entries from org-db if you want to start over.
;;
;; Advanced usage
;; you can build emacsql queries on org-db to do lots of things.


;;; Code:
(require 'cl-lib)
(require 's)    ; for s-trim
(require 'org)
(use-package emacsql-sqlite)
(require 'ivy)


(defcustom org-db-root "~/org-db/"
  "Root directory for db files."
  :group 'org-db)

(defcustom org-db-name "org-db.sqlite"
  "Name of the sqlite database file."
  :group 'org-db)

(defcustom org-db-index-content nil
  "Controls if the content of headlines is saved."
  :group 'org-db)


(unless (file-directory-p org-db-root)
  (make-directory org-db-root t))

(defvar org-db (emacsql-sqlite (expand-file-name org-db-name org-db-root))
  "Variable for the ‘org-db’ connection.")

(defvar org-db-queue '()
  "A list of files that need to be updated.")

(defvar org-db-log-file (expand-file-name "org-db.log" org-db-root)
  "Path to the log file.")

(defvar org-db-ignore-file-regexps '(".*.gpg$" "\\.dropbox")
  "A list of regexps of files (including their path) to ignore.")

(defvar org-db-ignore-tags '()
  "A list of tags to exclude from the database.")

(defvar org-db-ignore-properties '("RESULT")
  "A list of properties to exclude from the database.")

(defvar org-db-ignore-keywords '( )
  "A list of keywords to exclude from the database.")

(defvar org-db-debug nil
  "If non-nil log messages.")

(setq org-db-debug t)

(defvar hashtag-rx (rx (:
			;; hashtags begin at beginning of line, a blank, or
			;; after these punctuations. I am not sure what the
			;; "punctuation" group contains, so I am explicit here
			;; to also include the brackets.
			(or bol blank (in ",.;:?!(){}[]<>")) (= 1 "#")
			(group-n 1
				 ;; hashtags do not start with numbers
				 ;; or # + punctuation or a space
				 (not (in digit punctuation "#+' "))
				 ;; The rest of the chars cannot be a space or punctuation
				 (one-or-more
				  (one-or-more (not (in space punctuation)))
				  ;; the words can be joined by - or _
				  (zero-or-one (in "-_"))))
			;; hashtags end at blanks, end of line or punctuation
			(or blank eol punct)))
  "Regular expression to match hashtags.")

(defun org-db-log (format-string &rest args)
  "Insert the FORMAT-STRING formatted with ARGS into log buffer."
  (when org-db-debug
    (with-current-buffer (get-buffer-create "*org-db-log*")
      (goto-char (point-max))
      (insert (format "%s:\n" (current-time-string)))
      (let* ((msg (with-temp-buffer
		    (insert (format "%s\n"
				    (apply 'format format-string args)))
		    (fill-region (point-min) (point-max))
		    (indent-region (point-min) (point-max) 4)
		    (buffer-string))))
	(insert msg)
	(message (s-trim msg))))))

(org-db-log "Started org-db")

;; create the tables if we need to.
(progn
  (emacsql org-db [:PRAGMA (= foreign_keys 1)])

  (emacsql org-db [:create-table :if :not :exists files
				 ([(rowid integer :primary-key)
				   (filename text :unique)
				   (md5 text)
				   (last-updated text)])])

  (emacsql org-db [:create-table :if :not :exists tags
				 ([(rowid integer :primary-key)
				   (tag text :unique)])])

  (emacsql org-db [:create-table :if :not :exists properties
				 ([(rowid integer :primary-key)
				   (property text :unique)])])

  (emacsql org-db [:create-table :if :not :exists keywords
				 ([(rowid integer :primary-key)
				   (keyword text :unique)])])

  (emacsql org-db [:create-table :if :not :exists headlines
				 ([(rowid integer :primary-key)
				   (filename-id integer :not :null)
				   (title text :not :null)
				   (level integer :not :null)
				   (todo-keyword text)
				   (todo-type text)
				   archivedp
				   commentedp
				   footnote-section-p
				   (begin integer :not :null)
				   (tags text)
				   (priority text)]
				  (:foreign-key [filename-id] :references files [rowid]
						:on-delete :cascade))])

  (emacsql org-db [:create-table :if :not :exists headline-tags
				 ([(rowid integer :primary-key)
				   (headline-id integer)
				   (tag-id integer)]
				  (:foreign-key [headline-id] :references headlines [rowid]
						:on-delete :cascade)
				  (:foreign-key [tag-id] :references tags [rowid] :on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists headline-properties
				 ([(rowid integer :primary-key)
				   (headline-id integer)
				   (property-id integer)
				   (value text)]
				  (:foreign-key [headline-id] :references headlines [rowid]
						:on-delete :cascade)
				  (:foreign-key [property-id] :references properties [rowid]
						:on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists file-keywords
				 ([(rowid integer :primary-key)
				   (filename-id integer)
				   (keyword-id integer)
				   (value text)
				   (begin integer)]
				  (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
				  (:foreign-key [keyword-id] :references keywords [rowid]
						:on-delete :cascade))])


  (emacsql org-db [:create-table :if :not :exists links
				 ([(rowid integer :primary-key)
				   (filename-id integer)
				   (type text)
				   (path text)
				   (raw-link text)
				   (description text)
				   (search-option text)
				   (begin integer)]
				  (:foreign-key [filename-id] :references files [rowid]
						:on-delete :cascade))])


  ;; hashtags
  (emacsql org-db [:create-table :if :not :exists hashtags
				 ([(rowid integer :primary-key)
				   (hashtag text :unique)])])

  (emacsql org-db [:create-table :if :not :exists file-hashtags
				 ([(rowid integer :primary-key)
				   (filename-id integer)
				   (hashtag-id integer)
				   (begin integer)]
				  (:foreign-key [filename-id] :references files [rowid] :on-delete :cascade)
				  (:foreign-key [hashtag-id] :references hashtags [rowid]
						:on-delete :cascade))]))


(defun org-db-connect ()
  "Make sure we are connected."
  (unless (and org-db (emacsql-live-p org-db))
    (setq org-db (emacsql-sqlite (expand-file-name org-db-name org-db-root)))))


(defun org-db-get-filename-id (fname)
  "Return the rowid corresponding to FNAME.
Adds FNAME to the database if it doesn't exist."
  (org-db-connect)
  (or
   ;; this is a file in the database
   (caar (emacsql org-db [:select rowid :from files
				  :where (= filename $s1)]
		  fname))
   ;; no file found, we add one and get the id.
   (prog2
       (emacsql org-db [:insert :into files :values [nil $s1 $s2 $s3]]
		(buffer-file-name)
		(md5 (buffer-string))
		nil)
       (caar (emacsql org-db [:select (funcall last-insert-rowid)])))))


(defun org-db-remove-buffer ()
  "Remove the current buffer from the database."
  (interactive)
  (let* ((filename-id (org-db-get-filename-id (buffer-file-name))))
    ;; delete links
    (emacsql org-db [:delete :from links :where (= links:filename-id $s1)] filename-id)

    ;; keywords
    (emacsql org-db [:delete :from file-keywords
			     :where (= file-keywords:filename-id $s1)]
	     filename-id)

    ;; headlines
    (emacsql org-db [:delete :from headlines :where (= headlines:filename-id $s1)]
	     filename-id)

    ;; and the file
    (emacsql org-db [:delete :from files :where (= rowid $s2)] filename-id))
  (org-db-log "Removed %s from the database." (buffer-file-name)))


(defun org-db-update-buffer (&optional force)
  "Update the entries in the database for the currently visited buffer.
Optional argument FORCE. if non-nil force the buffer to be added."
  (interactive "P")
  (org-db-connect)
  (save-buffer)
  (org-db-log "Updating in buffer: %s" (buffer-file-name))
  (org-with-wide-buffer
   (when (or force
	     (and
	      ;; file does not match an ignore pattern
	      (and org-db-ignore-file-regexps
		   (not (string-match (regexp-opt org-db-ignore-file-regexps)
				      (buffer-file-name))))
	      ;; file is not in database
	      (null (caar (emacsql org-db [:select [md5] :from files
						   :where (= filename $s1)]
				   (buffer-file-name))))
	      (org-db-log "%s is a new file" (buffer-file-name)))
	     (and
	      ;; file does not match an ignore pattern
	      (and org-db-ignore-file-regexps
		   (not (string-match (regexp-opt org-db-ignore-file-regexps)
				      (buffer-file-name))))
	      ;; file is in database and it has changed
	      (not (string= (md5 (buffer-string))
			    (caar (emacsql org-db [:select [md5] :from files
							   :where (= filename $s1)]
					   (buffer-file-name)))))
	      (org-db-log "%s has changed." (buffer-file-name))))
     (let* ((filename-id (org-db-get-filename-id (buffer-file-name)))
	    (parse-tree (org-element-parse-buffer))
	    (links (org-element-map parse-tree 'link
		     (lambda (link)
		       (vector
			nil
			filename-id
			(org-element-property :type link)
			(org-element-property :path link)
			(org-element-property :raw-link link)
			(if (org-element-property :contents-begin link)
			    (buffer-substring-no-properties
			     (org-element-property :contents-begin link)
			     (org-element-property :contents-end link))
			  "")
			(org-element-property :search-option link)
			(org-element-property :begin link)))))
	    (ba (-split-at 400 links))
	    (handle (nth 0 ba))
	    (next (nth 1 ba))

	    (keywords (org-element-map parse-tree 'keyword
			(lambda (kw) (list
				      (upcase (org-element-property :key kw))
				      (org-element-property :value kw)
				      (org-element-property :begin kw)))))
	    keyword-id
	    (headlines (org-element-map parse-tree 'headline
			 'identity))

	    ;; "#\\([^0-9!$%^&*+.]\\(\\w+[-_]?\\)+\\)"
	    (hashtags (save-excursion
			(goto-char (point-min))
			(let ((hashtags '()))
			  (while (re-search-forward hashtag-rx nil t)
			    ;; there are many scenarios we do not want to count these.
			    ;; no src-blocks as these are often false matches for comments
			    ;; These are not always reliable as it relies on font-lock
			    (when (and (not (org-in-src-block-p))
				       (not (save-match-data (equal 'src-block (car (org-element-context))))))
			      (push (cons (match-string 1) (match-beginning 0)) hashtags)))
			  hashtags)))
	    hashtag-id
	    hlv 			;; headline level
	    headline-id
	    tags
	    properties
	    tag-id)

       ;; update the md5
       (emacsql org-db [:update files :set (= md5 $s1) :where (= rowid $s2)]
		(md5 (buffer-string)) filename-id)

       ;; * delete old links
       (emacsql org-db [:delete :from links :where (= links:filename-id $s1)] filename-id)

       ;;  ** add new links
       (while handle
	 (emacsql org-db [:insert :into links :values $v1] handle)
	 (setq ba (-split-at 400 next)
	       handle (nth 0 ba)
	       next (nth 1 ba)))

       ;; * File keywords. For each keyword, get the id or add to the keywords
       ;; * table and get the id.
       (emacsql org-db [:delete :from file-keywords
				:where (= file-keywords:filename-id $s1)]
		filename-id)

       (cl-loop for (keyword value begin) in keywords
		if (not (member keyword org-db-ignore-keywords))
		do
		(setq keyword-id (or (caar (emacsql org-db [:select rowid :from keywords
								    :where (= keyword $s1)]
						    keyword))
				     (emacsql org-db [:insert :into keywords :values [nil $s1]]
					      keyword)
				     (caar (emacsql org-db
						    [:select (funcall last-insert-rowid)]))))
		;; Now add to the file-keywords
		(emacsql org-db [:insert :into file-keywords :values [nil $s1 $s2 $s3 $s4]]
			 filename-id keyword-id value begin))

       ;; hashtags
       ;; first delete existing data on hashtags because it has probably changed.
       (emacsql org-db [:delete :from file-hashtags
				:where (= file-hashtags:filename-id $s1)]
		filename-id)


       (cl-loop for (hashtag . pos) in hashtags do
		;; get hashtag id
		(setq hashtag-id (or (caar (emacsql org-db [:select rowid :from hashtags
								    :where (= hashtag $s1)]
						    hashtag))
				     (emacsql org-db [:insert :into hashtags :values [nil $s1]]
					      hashtag)
				     (caar (emacsql org-db
						    [:select (funcall last-insert-rowid)]))))

		(emacsql org-db [:insert :into file-hashtags :values [nil $s1 $s2 $s3]]
			 filename-id hashtag-id pos))

       ;; * Headlines delete the headlines from this file. Should cascade delete
       ;; tags, properties and keywords.
       (emacsql org-db [:delete :from headlines :where (= headlines:filename-id $s1)]
		filename-id)

       (cl-loop for hl in headlines do
		(save-excursion
		  (goto-char (org-element-property :begin hl))
		  (setq tags (mapcar 'org-no-properties (org-get-tags-at))
			properties (org-entry-properties (org-element-property :begin hl) 'all)))

		(setq hlv (vector
			   nil
			   filename-id
			   (org-element-property :raw-value hl)
			   (org-element-property :level hl)
			   (when (org-element-property :todo-keyword hl)
			     (substring-no-properties
			      (org-element-property :todo-keyword hl)))
			   (org-element-property :todo-type hl)
			   (org-element-property :archivedp hl)
			   (org-element-property :commentedp hl)
			   (org-element-property :footnote-section-p hl)
			   (org-element-property :begin hl)
			   ;; this is really a tag string for easy searching in
			   ;; helm/ivy because it seems tricky to build this from a
			   ;; query
			   (when tags
			     (concat ":" (mapconcat
					  'substring-no-properties
					  tags ":") ":"))
			   (if (org-element-property :priority hl)
			       (char-to-string (org-element-property :priority hl))
			     nil)))

		;; insert headline row and get headline-id
		(emacsql org-db [:insert :into headlines :values $v1] hlv)
		(setq headline-id (caar (emacsql org-db
						 [:select (funcall last-insert-rowid)])))

		;; remove old tag data
		(emacsql org-db [:delete :from headline-tags
					 :where (= headline-tags:headline-id $s1)]
			 headline-id)

		(cl-loop for tag in tags
			 if (not (member tag org-db-ignore-tags))
			 do
			 (setq tag-id
			       (or (caar (emacsql org-db [:select rowid :from tags
								  :where (= tag $s1)]
						  tag))
				   (emacsql org-db [:insert :into tags :values [nil $s1]]
					    tag)
				   (caar (emacsql org-db [:select (funcall last-insert-rowid)]))))
			 (emacsql org-db [:insert :into headline-tags :values $v1]
				  (vector nil headline-id tag-id)))

		;; properties
		(emacsql org-db [:delete :from headline-properties
					 :where (= headline-properties:headline-id $s1)]
			 headline-id)

		(setq properties (save-excursion
				   (goto-char (org-element-property :begin hl))
				   (org-entry-properties)))

		(cl-loop for (property . value) in properties
			 if (not (member property org-db-ignore-properties))
			 do
			 (setq property-id
			       (or
				(caar (emacsql org-db [:select rowid :from properties
							       :where (= property $s1)]
					       property))
				(emacsql org-db [:insert :into properties
							 :values [nil $s1]]
					 property)
				(caar (emacsql org-db [:select (funcall last-insert-rowid)]))))

			 ;; and the values
			 (emacsql org-db [:insert :into headline-properties
						  :values [nil $s1 $s2 $s3]]
				  headline-id
				  property-id
				  (org-no-properties value))))

       (emacsql org-db [:update files :set (= last-updated $s1) :where (= rowid $s2)]
		(format-time-string "%Y-%m-%d %H:%M:%S") filename-id)))))


;; * the hooks
(defun org-db-hook-function ()
  "Function to run after starting ‘org-mode’."
  ;; Run when we open in case it changed from some external program. Only for
  ;; org and org_archive files, and not just when we enter org-mode for some
  ;; reason.
  (when (and (buffer-file-name)
	     (or (f-ext? (buffer-file-name) "org")
		 (f-ext? (buffer-file-name) "org_archive")))

    (add-to-list 'org-db-queue (buffer-file-name) t)
    (org-db-log "added %s to the queue." (buffer-file-name))

    ;; add local after save hook in case this is a new file.
    (add-hook 'after-save-hook 'org-db-hook-function t t)))


(add-hook 'org-mode-hook 'org-db-hook-function)


;; * Idle timer to update

(defun org-db-process-queue (&optional now)
  "Update all the files in `org-db-queue'.
Use a prefix ARG to process NOW."
  (interactive "P")
  (org-db-connect)
  (catch 'done
    (while org-db-queue
      (unless (or now (current-idle-time))
	(throw 'done nil))
      (org-db-log "Updating org-db for files %s." org-db-queue)
      (let* ((filename (pop org-db-queue))
	     (org-mode-hook '())
	     (enable-local-variables nil)
	     (already-open (find-buffer-visiting filename))
	     (buf (find-file-noselect filename)))
	(org-db-log "Updating %s" filename)
	(with-current-buffer buf
	  (org-db-update-buffer))
	(unless already-open (kill-buffer buf)))))
  (org-db-log "Done processing org-db queue."))

;; if we are idle for 5 minutes, process the queue.
(setq org-db-timer (run-with-idle-timer (* 60 5) t 'org-db-process-queue))

(defun org-db-status ()
  "Print a message of files scheduled for update."
  (interactive)
  (org-db-log "Files in queue for update: %s" org-db-queue)
  (switch-to-buffer "*org-db-log*"))



;; * Update the whole database

(defun org-db-refresh (&optional force)
  "Update all the files in the database.

Use a prefix arg to FORCE updates."
  (interactive "P")
  (let* ((files (emacsql org-db [:select [filename] :from files :order-by rowid :asc]))
	 (N (length files))
	 (enable-local-variables nil)
	 (org-mode-hook '())
	 buf already-open)
    (cl-loop for (fname) in files for i from 0 to N
	     if (and fname (file-exists-p fname))
	     do
	     (org-db-log "Refreshing %s of %s (%s)" i N fname)
	     (setq already-open (find-buffer-visiting fname))
	     (setq buf (find-file-noselect fname))
	     (with-current-buffer buf
	       (condition-case err
		   (org-db-update-buffer force)
		 (org-db-log "Error updating %s: %s" fname err)))
	     (unless already-open (kill-buffer buf))
	     else
	     do
	     (org-db-log "deleting %s from the database." fname)
	     (emacsql org-db [:delete :from files :where (= filename $s1)] fname))))


(defun org-db-index (path  &optional recursive)
  "Index all the org-files in PATH.
Optional RECURSIVE is non-nil find files recursively."
  (interactive (list (read-directory-name "Path: ")
		     current-prefix-arg))
  (let* ((enable-local-variables nil)
	 (org-mode-hook '())
	 already-open buf
	 (files (f-files path (lambda (f)
				(and (or (f-ext? f "org")
					 ;; I am not sure we should index these.
					 ;; (f-ext? f "org_archive")
					 )
				     (and org-db-ignore-file-regexps
					  (not (string-match (regexp-opt org-db-ignore-file-regexps)
							     f)))))
			 recursive))
	 (N (length files)))
    (cl-loop for fname in files
	     for i from 1
	     do
	     (message "%s of %s - %s" i N fname)
	     (setq already-open (find-buffer-visiting fname))
	     (with-current-buffer (or already-open (setq buf (find-file-noselect fname)))
	       (condition-case err
		   (org-db-update-buffer)
		 (message "Error updating %s: %s" fname err)))
	     (unless already-open (kill-buffer buf)))))


(defun org-db-clean-db ()
  "Remove entries from the database where the file does not exist."
  (cl-loop for (fname) in (emacsql org-db [:select :distinct [filename] :from files])
	   unless (file-exists-p fname)
	   do
	   (org-db-log "%s was not found. Removing it." fname)
	   (emacsql org-db [:delete :from files :where (= filename $s1)] fname)))


(defun org-db-reset-db ()
  "Clear all entries from all tables."
  (interactive)
  (emacsql org-db [:delete :from files])
  (emacsql org-db [:delete :from tags])
  (emacsql org-db [:delete :from properties])
  (emacsql org-db [:delete :from keywords])
  (emacsql org-db [:delete :from headlines])
  (emacsql org-db [:delete :from headline-tags])
  (emacsql org-db [:delete :from headline-properties])
  (emacsql org-db [:delete :from file-keywords])
  (emacsql org-db [:delete :from links])
  (emacsql org-db [:delete :from hashtags])
  (emacsql org-db [:delete :from file-hashtags])
  (message "everything should be reset."))



;; * org-db contacts
(defun org-db-contacts-candidates ()
  "List of headings with EMAIL properties."
  (let ((contacts (emacsql org-db
			   [:select [headlines:title
				     headline-properties:value
				     headlines:tags files:filename files:last-updated headlines:begin]
				    :from headlines
				    :inner :join headline-properties
				    :on (=  headlines:rowid headline-properties:headline-id)
				    :inner :join properties
				    :on (= properties:rowid headline-properties:property-id)
				    :inner :join files :on (= files:rowid headlines:filename-id)
				    :where (= properties:property "EMAIL")])))
    (cl-loop for (title email tags fname last-updated begin) in contacts
	     collect
	     (list (format "%30s | %40s | %s"
			   (s-pad-right 30 " " (s-trim title))
			   (s-pad-right 40 " " email)
			   (or tags ""))
		   :filename fname :last-updated last-updated :begin begin :email email :title (s-trim title)))))




(defun org-db-insert-contact-link (x)
  "Insert the contact associated with X."
  (let ((link)
	(candidate (cdr x)))
    ;; check if the file is up-to-date
    (let ((actual-mod-time (float-time (file-attribute-modification-time (file-attributes (plist-get candidate :filename))))))
      (when (org-time<= (plist-get candidate :last-updated) actual-mod-time)
	(error q"%s is not up to date in org-db.")))

    (with-current-buffer
	(find-file-noselect
	 (plist-get candidate :filename))
      (goto-char (plist-get candidate :begin))

      ;; Check we are looking at the right place
      (unless (and (looking-at org-heading-regexp)
		   (string= (plist-get candidate :email) (org-entry-get (point) "EMAIL")))
	(error "It does not appear we are looking at the right place:\n%s"))

      (setq link (format
		  "[[contact:%s][%s]]"
		  (org-id-get-create)
		  (nth 4 (org-heading-components))))
      (save-buffer)
      (org-db-update-buffer t))
    (when (looking-back "]" 1)
      (insert ", "))
    (insert link)))


(defun org-db-contacts ()
  "Ivy command to select an `org-db' contact."
  (interactive)
  (let ((candidates (org-db-contacts-candidates)))
    (ivy-read "Contact: " candidates :action '(1
					       ("i" (lambda (x)
						      (unless (looking-back " " 1)
							(insert ","))
						      (insert
						       (format "%s <%s>"
							       (plist-get (cdr x) :title)
							       (plist-get (cdr x) :email))))
						"insert")
					       ("o" (lambda (x)
						      (find-file (plist-get (cdr x) :filename))
						      (goto-char (plist-get (cdr x) :begin))
						      (show-entry))
						"open")
					       ("l" org-db-insert-contact-link "Insert link")))))


;; * org-db-locations

(defun org-db-locations-candidates ()
  "Return a list of headings with an ADDRESS property."
  (let ((locations (emacsql org-db [:select [headlines:title headline-properties:value headlines:tags files:filename headlines:begin]
					    :from headlines
					    :inner :join headline-properties :on (=  headlines:rowid headline-properties:headline-id)
					    :inner :join properties :on (= properties:rowid headline-properties:property-id)
					    :inner :join files :on (= files:rowid headlines:filename-id)
					    :where (= properties:property "ADDRESS")])))
    (cl-loop for (title address tags fname begin) in locations
	     collect
	     (list (format "%60s | %70s | %s"
			   (s-trim title)
			   address
			   (or tags ""))
		   :filename fname :begin begin))))



(defun org-db-locations ()
  "Open a location in `org-db'."
  (let ((candidates (org-db-locations-candidates)))
    (ivy-read "Location: " candidates :action '(1
						("o" (lambda (x)
						       (find-file (plist-get (cdr x) :filename))
						       (goto-char (plist-get (cdr x) :begin))))
						("l" (lambda (x)
						       (let ((link)
							     (candidate (cdr x)))
							 (with-current-buffer
							     (find-file-noselect
							      (plist-get candidate :filename))
							   (goto-char (plist-get candidate :begin))
							   (setq link (format
								       "[[location:%s][%s]]" (org-id-get-create)
								       (nth 4 (org-heading-components)))))
							 (insert link))))))))

;; ** geo link
;; eg. geo:40.442403,-79.943838
;; [[geo:40°26'32.6"N 79°56'37.8"W]]
(org-link-set-parameters
 "geo"
 :follow (lambda (path)
	   (browse-url (format "http://maps.google.com/maps?q=%s" path))))




;; * org-db headlines
(defun org-db-heading-candidates ()
  "Return candidates for ivy or helm selection."
  (let* ((headings (emacsql org-db [:select [headlines:level headlines:title headlines:tags
							     files:filename headlines:begin
							     files:last-updated]
					    :from headlines
					    :inner :join files
					    :on (= files:rowid headlines:filename-id)
					    :order :by files:last-updated :desc]))
	 (candidates (cl-loop for (level title tags filename begin last-updated) in headings
			      collect
			      (cons
			       (format "%100s|%20s|%s|%s"
				       (s-pad-right 100 " " (concat  (make-string level (string-to-char "*")) " " title))
				       (s-pad-right 20 " " (or tags ""))
				       filename last-updated)
			       (list
				:file filename
				:last-updated last-updated
				:begin begin)))))
    candidates))


;;;###autoload
(defun org-db-headings ()
  "Use ivy to open a heading with completion."
  (interactive)
  (let* ((candidates (org-db-heading-candidates)))
    (ivy-read "heading: " candidates
	      :action
	      '(1
		("o" (lambda (candidate)
		       (find-file (plist-get (cdr candidate) :file))
		       (goto-char (plist-get (cdr candidate) :begin))
		       (org-show-context))
		 "Open to heading.")
		("l" (lambda (candidate)
		       "Store link"
		       (find-file (plist-get (cdr candidate) :file))
		       (goto-char (plist-get (cdr candidate) :begin))
		       (org-store-link))
		 "Store link to heading.")))))


;; * org-db files

(defun org-db-files ()
  "Open a file in ‘org-db’ with completion."
  (interactive)
  (find-file (completing-read "File: " (mapcar 'car (emacsql org-db [:select [filename]
									     :from files
									     :order :by filename])))))


(defun org-db-recentf ()
  "Open a recent file in ‘org-db’ with completion."
  (interactive)
  (let ((candidates (mapcar (lambda (x)
			      (cons (format "%s %s" (cdr x) (car x)) (car x)))
			    (emacsql org-db [:select [filename last-updated]
						     :from files
						     :order :by last-updated :desc]))))
    (find-file (cdr (assoc (ivy-read "File: " candidates) candidates)))))


;; * org-db-links

(defun org-db-links ()
  "Open a link."
  (interactive)
  (let ((candidates (cl-loop for (rl fn bg) in (emacsql org-db [:select [raw-link filename begin ]
									:from links
									:left :join files :on (= links:filename-id files:rowid)
									:order :by filename])
			     collect
			     ;; (candidate :filename :begin)
			     (list (format "%s | %s" rl fn) :filename fn :begin bg))))
    (ivy-read "link: " candidates :action '(1
					    ("o" (lambda (x)
						   (find-file (plist-get (cdr x) :filename))
						   (goto-char (plist-get (cdr x) :begin))
						   (org-show-entry))
					     "Open to link")))))

;; * org-db-hashtags

;; The idea here is to use #hashtags in org files so we can search them later.
;; These are hard, there are many reasons for #something, including comments in
;; python blocks, #'in-elisp, etc. I sort of like the idea though, because it
;; provides a way to jump to paragraphs. Maybe it would be better to use radio
;; targets for this though.

(defun org-db-delete-hashtag (hashtag)
  "Delete HASHTAG from the db."
  (let* ((hashtag-id (caar (emacsql org-db [:select rowid :from hashtags
						    :where (= hashtag $s1)]
				    hashtag))))
    (if (null hashtag-id)
	(message "Weird. no hashtag id found for %s" hashtag)
      (message "removing %s entries: %s" hashtag hashtag-id)

      (emacsql org-db [:delete :from file-hashtags
			       :where (= file-hashtags:hashtag-id $s1)]
	       hashtag-id)
      (emacsql org-db [:delete :from hashtags
			       :where (= hashtags:rowid $s1)]
	       hashtag-id))))


(defun org-db-delete-hashtags ()
  "Delete all the hashtags in the db."
  (interactive)
  (emacsql org-db [:delete :from hashtags])
  (emacsql org-db [:delete :from file-hashtags]))


(defun looking-at-hashtag ()
  "Return hashtag if looking at one, else nil.
This is kind of tricky. We look backwards to the beginning the
line and only return a match if it is around the current point."
  (interactive)
  ;; I thought this would work too, but it also isn't reliable
  ;; (save-excursion
  ;;   (let ((p (point)))
  ;;     (when (re-search-backward hashtag-rx (line-beginning-position) t 1)
  ;; 	;; We have to do this little trick to get the whole match. the previous
  ;; 	;; line stops at the current point.
  ;; 	(looking-at hashtag-rx)
  ;; 	;; then we make sure the match brackets our point.
  ;; 	(when (and (>= p (match-beginning 0))
  ;; 		   (<= p (match-end 0)))
  ;; 	  (message "%s" (match-string 0))))))

  ;; I thought this should work, but it doesn't appear to
  ;; (looking-back hashtag-rx (line-beginning-position) t)

  ;; This is clunky to me, but it does work.

  ;; There are some other subtle points.
  ;; 1. should we allow hashtags in src-blocks? This leads to many false hashtags in Python for example.
  ;; [2020-07-28 Tue] I am eliminating src-blocks for hash-tags.
  (let ((p (point))
  	(lbp (line-beginning-position)))
    (save-excursion
      (while (and
	      (not (org-in-src-block-p))
	      (not (looking-at hashtag-rx))
  	      (>= (point) lbp))
  	(backward-char))
      (when (and (>= p (match-beginning 0))
  		 (<= p (match-end 0)))
  	(match-string 1)))))


(defun org-db-hashtags ()
  "Open a #hashtag.
If the cursor is on a hashtag, it uses that as the initial input.
I am not sure how to do multiple hashtag matches right now."
  (interactive)
  (let* ((tip (looking-at-hashtag))
	 (hashtag-data (emacsql org-db [:select [hashtag file-hashtags:begin files:filename]
						:from hashtags
						:left :join file-hashtags :on (= hashtags:rowid file-hashtags:hashtag-id)
						:inner :join files
						:on (= files:rowid file-hashtags:filename-id)]))
	 (candidates (cl-loop for (hashtag begin fname) in hashtag-data
			      collect (list (format "%20s%8s  %s" hashtag begin fname)
					    begin fname hashtag))))

    (ivy-read "#hashtag: " candidates :initial-input tip
	      :action
	      '(1
		("o" (lambda (candidate)
		       (find-file (third candidate))
		       (goto-char (second candidate))
		       (show-entry))
		 "Open file at hashtag")
		("i" (lambda (candidate)
		       (insert (format "#%s" (fourth candidate))))
		 "Insert hashtag at point.")
		("D" (lambda (candidate)
		       (let* ((hashtag (fourth candidate)))
			 (org-db-delete-hashtag hashtag)))
		 "Delete this hashtag")))))



;; org-id integration
;; org-id-goto is very slow for me. This function can replace it.
(defun org-db-goto-id (id)
  "Open an org file at ID."
  (let* ((result (emacsql org-db
			  [:select [files:filename headlines:begin]
				   :from headlines
				   :inner :join headline-properties
				   :on (=  headlines:rowid headline-properties:headline-id)
				   :inner :join properties
				   :on (= properties:rowid headline-properties:property-id)
				   :inner :join files :on (= files:rowid headlines:filename-id)
				   :where (and (= properties:property "ID")
					       (= headline-properties:value $s1))]
			  id))
	 match fname p)
    (cond
     ((and result
	   (= 1 (length result)))
      (setq match (first result)
	    fname (first match)
	    p (second match))
      (find-file fname)
      (goto-char p))

     ((> (length result) 1)
      ;; TODO maybe use ivy to select?
      (message "(%s) matches: %s result" (length result) result))

     (t
      (message "No match found")))))


(defun org-db-toggle-org-id ()
  "Toggle using `org-db' to follow org-id links."
  (interactive)
  (if (not (get 'org-db-goto-id 'enabled))
      (progn
	(advice-add 'org-id-goto :override #'org-db-goto-id)
	(put 'org-db-goto-id 'enabled t)
	(message "org-db-goto-id advice enabled."))
    (advice-remove 'org-id-goto #'org-db-goto-id)
    (put 'org-db-goto-id 'enabled nil)
    (message "org-db-goto-id advice disabled.")))


;; * property search

(defun org-db-property-search (property pattern)
  "Search org-db for entries where PROPERTY matches PATTERN.
PATTERN follows sql patterns, so % is a wildcard."
  (interactive (list (completing-read "Property: "
				      (-flatten (emacsql org-db [:select properties:property :from properties])))
		     (read-string "Pattern: ")))
  (let* ((results (emacsql org-db
			   [:select [headlines:title
				     headline-properties:value
				     headlines:tags files:filename files:last-updated headlines:begin]
				    :from headlines
				    :inner :join headline-properties
				    :on (=  headlines:rowid headline-properties:headline-id)
				    :inner :join properties
				    :on (= properties:rowid headline-properties:property-id)
				    :inner :join files :on (= files:rowid headlines:filename-id)
				    :where (and (= properties:property $s1)
						(like headline-properties:value $s2))]
			   property pattern))
	 (candidates (cl-loop for (title value tags fname last-updated begin) in results
			      collect
			      (list (format "%s | %s" fname value) :filename fname :begin begin))))

    (ivy-read "Choose: " candidates)))


;; * org-db-macro

(defmacro org-db-table (select where)
  "Experiment to see if we can make the syntax nicer."
  `[:select ,select
	    :from headlines
	    :inner :join headline-properties
	    :on (=  headlines:rowid headline-properties:headline-id)
	    :inner :join properties
	    :on (= properties:rowid headline-properties:property-id)
	    :inner :join files :on (= files:rowid headlines:filename-id)
	    :where ,where])

;; example usage
;; (emacsql org-db (org-db-table [headlines:title
;; 			       headline-properties:value
;; 			       headlines:tags files:filename files:last-updated headlines:begin]
;; 			      (and (= properties:property $s1)
;; 				   (like headline-properties:value $s2)))
;; 	 "EMAIL" "%kitchin%")

;; * End
(provide 'org-db)

;;; org-db.el ends here
