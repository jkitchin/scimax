;;; org-db ---  -*- lexical-binding: t -*-
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
;; `org-db-@' actions for @-labels
;; `org-db-properties' searches properties, although I don't find it that useful
;; `org-db-editmarks' searches your editmarks
;; `org-db-email-addresses' finds email addresses in your files.
;; `org-db-src-blocks' search src-blocks

;; `org-db-toggle-org-id' If you want to use org-db to jump to an org-id instead
;; of `org-id-goto'. I find the built-in function to slow for me.
;;
;; Utilities
;; `org-db-index' will prompt you for a directory and index the org-files in that directory. Use a prefix arg to make it recursive.
;; `org-db-clean-db' will prune entries where the file no longer exists.
;; `org-db-reset-db' will clear all the entries from org-db if you want to start over.
;;
;;; org-db.el --- An org database

;;; Code:
(require 'cl-lib)
(require 's)    ; for s-trim
(require 'org)

(require 'ivy)


(defcustom org-db-root "~/org-db/"
  "Root directory for db files."
  :type 'directory
  :group 'org-db)


(defcustom org-db-name "org-db-v2.sqlite"
  "Name of the sqlite database file."
  :type 'string
  :group 'org-db)


(defcustom org-db-update-functions
  '(org-db-update-headlines
    org-db-update-links
    org-db-update-keywords
    org-db-update-src-blocks
    org-db-update-hashtags
    org-db-update-@-labels
    org-db-update-email-addresses
    org-db-update-editmarks
    org-db-update-targets
    org-db-update-timestamps)
  "List of functions to run when updating a file in `org-db'.
Each function takes arguments that are the filename-id, and the
parse tree from `org-element-parse-buffer'. Your function should
delete old data if needed, and insert or update data as needed."
  :type '(list function)
  :group 'org-db)

;; Make sure we have a directory to store the root in
(unless (file-directory-p org-db-root)
  (make-directory org-db-root t))


(defvar org-db-log-file (expand-file-name "org-db.log" org-db-root)
  "Path to the log file.")


(defvar org-db-ignore-file-regexps '(".*.gpg$" "\\.dropbox" "*~undo-tree~*")
  "A list of regexps of files (including their path) to ignore.")


(defvar org-db-ignore-tags '()
  "A list of tags to exclude from the database.")


(defvar org-db-ignore-properties '("RESULT")
  "A list of properties to exclude from the database.")


(defvar org-db-ignore-keywords '( )
  "A list of keywords to exclude from the database.")


(defvar org-db-debug t
  "If non-nil log messages.")


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


(defvar @-rx (rx (:
		  ;; @label begin at beginning of line, a blank, or
		  ;; after these punctuations. I am not sure what the
		  ;; "punctuation" group contains, so I am explicit here
		  ;; to also include the brackets.
		  (or bol blank (in ",.;:?!(){}[]<>")) (= 1 "@")
		  (group-n 1
		    ;; The rest of the chars cannot be a space or punctuation
		    (one-or-more
		     (one-or-more (not (in space punctuation)))
		     ;; the words can be joined by - or _
		     (zero-or-one (in "-_"))))
		  ;; @labels end at blanks, end of line or punctuation
		  (or blank eol punct)))
  "Regular expression to match @labels.
Maybe not surprisingly, there are a lot of false positives.
Sometimes, this seems to match emails, it matches things like
decorators in Python, etc.")


(defvar email-rx "<?\\([-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+\\)>?"
  "Regular expression for email addresses.
Adapted from `thing-at-point-email-regexp'.")


(defun org-db-log (format-string &rest args)
  "Insert the FORMAT-STRING formatted with ARGS into log file."
  (f-append-text
   (apply 'format (append (list (concat "%s " format-string "\n\n"))
			  (list (current-time-string)) args))
   'utf-8
   org-db-log-file))


(defmacro with-org-db (&rest body)
  "Run BODY commands in an org-db context.
Opens the database if needed. BODY should be regular commands.
The commands can use the variable `org-db' in them. Closes the db
when done. Returns the last sexp of BODY."
  `(lexical-let ((org-db (sqlite-open (expand-file-name org-db-name org-db-root))))
     (prog1
	 (progn ,@body)
       (sqlite-close org-db))))

(org-db-log "Starting org-db")

(defun org-db-setup ()
  "Setup the db tables."
  ;; create the tables if we need to.
  (let ((org-db (sqlite-open (expand-file-name org-db-name org-db-root))))
    (sqlite-pragma org-db "foreign_keys=ON")
    
    (sqlite-execute org-db "create table if not exists files(
       rowid integer primary key,
       filename text unique,
       md5 text,
       last_updated text);")
    
    (sqlite-execute org-db "create table if not exists tags(
       rowid integer primary key,
       tag text unique);")
    
    (sqlite-execute org-db "create table if not exists properties(
       rowid integer primary key,
       property text unique);")
    
    (sqlite-execute org-db "create table if not exists keywords(
       rowid integer primary key,
       keyword text unique);")
    
    (sqlite-execute org-db "create table if not exists headlines(
       rowid integer primary key,
       filename_id integer not null,
       title text not null,
       level integer not null,
       todo_keyword text,
       todo_type text,
       archivedp,
       commentedp,
       footnote_section_p,
       begin integer not null,
       tags text,
       priority text,
       scheduled text,
       deadline text,
       foreign key(filename_id) references files(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists headline_tags(
       rowid integer primary key,
       headline_id integer,
       tag_id integer,
       foreign key(headline_id) references headlines(rowid) on delete cascade,
       foreign key(tag_id) references tags(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists headline_properties(
       rowid integer primary key,
       headline_id integer,
       property_id integer,
       value text,
       foreign key(headline_id) references headlines(rowid) on delete cascade,
       foreign key(property_id) references properties(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists file_keywords(
       rowid integer primary key,
       filename_id integer,
       keyword_id integer,
       value text,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade,
       foreign key(keyword_id) references keywords(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists links(
       rowid integer primary key,
       filename_id integer,
       type text,
       path text,
       raw_link text,
       description text,
       search_option text,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists targets(
       rowid integer primary key,
       target text);")
    
    (sqlite-execute org-db "create table if not exists file_targets(
       rowid integer primary key,
       filename_id integer,
       target_id integer,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade,
       foreign key(target_id) references targets(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists hashtags(
       rowid integer primary key,
       hashtag text unique);")
    
    (sqlite-execute org-db "create table if not exists file_hashtags(
       rowid integer primary key,
       filename_id integer,
       hashtag_id integer,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade,
       foreign key(hashtag_id) references hashtags(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists atlabels(
       rowid integer primary key,
       atlabel text unique);")
    
    (sqlite-execute org-db "create table if not exists file_atlabels(
       rowid integer primary key,
       filename_id integer,
       atlabel_id integer,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade,
       foreign key(atlabel_id) references atlabels(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists email_addresses(
       rowid integer primary key,
       email_address text unique);")
    
    (sqlite-execute org-db "create table if not exists file_email_addresses(
       rowid integer primary key,
       filename_id integer,
       email_address_id integer,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade,
       foreign key(email_address_id) references email_addresses(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists file_editmarks(
       rowid integer primary key,
       filename_id integer,
       type text,
       content text,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade);")
    
    (sqlite-execute org-db "create table if not exists src_blocks(
       rowid integer primary key,
       filename_id integer,
       language text,
       contents text,
       begin integer,
       foreign key(filename_id) references files(rowid) on delete cascade);")

    (sqlite-execute org-db "create table if not exists timestamps(
rowid integer primary key,
filename_id integer,
ts text,
type text,
context text,
begin integer,
foreign key(filename_id) references files(rowid) on delete cascade);")

    ;; this is the persistent queue that we process during idle time.
    (sqlite-execute org-db "create table if not exists queue(filename text)")
    
    (sqlite-close org-db)))


(defun org-db-reset ()
  "Delete and recreate the db.
Note this does not recreate tables from plugins.
"
  (interactive)
  (delete-file (expand-file-name org-db-name org-db-root))
  (org-db-setup))


(defun org-db-md5 (fname)
  "Return an md5sum for the contents of FNAME."
  (with-temp-buffer
    (insert-file-contents-literally fname)
    (md5 (buffer-string))))


(defun org-db-get-filename-id (fname)
  "Return the rowid corresponding to FNAME.
Adds FNAME to the database if it doesn't exist."
  (let ((org-db (sqlite-open (expand-file-name org-db-name org-db-root))))
    (or
     ;; this is a file in the database
     (caar (sqlite-select org-db "select rowid from files where filename = ?" (list fname)))
     
     ;; no file found, we add one and get the id.
     (prog2
	 (sqlite-execute org-db "insert into files(filename, md5, last_updated) values(?, ?, ?)"
			 (list fname
			       (org-db-md5 fname)
			       (format-time-string "%Y-%m-%d %H:%M:%S")))
	 
	 (caar (sqlite-execute org-db "select last_insert_rowid()"))
       (sqlite-close org-db)))))


(defun org-db-remove-buffer ()
  "Remove the current buffer from the database."
  (interactive)
  (org-db-remove-file (buffer-file-name)))


(defun org-db-remove-file (fname)
  "Remove FNAME from the database."
  (let* ((filename-id (with-org-db (caar (sqlite-select org-db "select rowid from files where filename = ?" (list fname))))))
    (when filename-id
      (with-org-db
       ;; delete links
       (sqlite-execute org-db "delete from links where filename_id = ?" (list filename-id))

       ;; keywords
       (sqlite-execute org-db "delete from file_keywords where filename_id = ?" (list filename-id))

       ;; headlines
       (sqlite-execute org-db "delete from headlines where filename_id = ?" (list filename-id))
       
       ;; and the file
       (sqlite-execute org-db "delete from files where rowid = ?" (list filename-id))
       
       (org-db-log "Removed %s from the database." (buffer-file-name))))))


;; * Update the database for a buffer
;;

(defun org-db-update-timestamps (filename-id parse-tree org-db)
  "Update timestamps in the buffer."
  (org-db-log "Updating timestamps")
  (sqlite-execute org-db "delete from timestamps where filename_id = ?"
		  (list filename-id))
  (let ((timestamps (org-element-map parse-tree 'timestamp
		      (lambda (ts)
			(unless (save-excursion
				  (org-element-property :begin ts)
				  (re-search-backward "DEADLINE:\\|SCHEDULED:" (line-beginning-position) t))
			  (list
			   nil
			   filename-id
			   ;; type
			   (symbol-name (org-element-property :type ts))
			   ;; ts (date-string)
			   (format "%d-%02d-%02d %02d:%02d:00"
				   (org-element-property :year-start ts)
				   (org-element-property :month-start ts)
				   (org-element-property :day-start ts)
				   (or (org-element-property :hour-start ts) 0)
				   (or (org-element-property :minute-start ts) 0))
			   ;; context
			   (buffer-substring
			    (save-excursion (goto-char (org-element-property :begin ts))
					    (line-beginning-position))
			    (save-excursion (goto-char (org-element-property :begin ts))
					    (line-end-position)))
			   ;; begin
			   (org-element-property :begin ts)))))))
    (cl-loop for row in timestamps do
	     (sqlite-execute org-db "insert into timestamps values (?, ?, ?, ?, ?, ?)"
			     row))))


(defun org-db-update-src-blocks (filename-id parse-tree org-db)
  "Update the src blocks in the buffer."
  (org-db-log "Updating src blocks")

  ;; delete old entries
  (sqlite-execute org-db "delete from src_blocks where filename_id = ?"
		  (list filename-id))
  ;; add new blocks
  (org-babel-map-src-blocks nil
    (sqlite-execute org-db "insert into src_blocks values(?, ?, ?, ?, ?)"
		    (list nil filename-id lang body beg-block))))


(defun org-db-update-keywords (filename-id parse-tree org-db)
  "Updates the keyword table for the org-buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating keywords")
  (let ((keywords (org-element-map parse-tree 'keyword
		    (lambda (kw) (list
				  (upcase (org-element-property :key kw))
				  (org-element-property :value kw)
				  (org-element-property :begin kw)))))
	keyword-id)

    ;; * File keywords.
    (sqlite-execute org-db "delete from file_keywords where filename_id = ?" (list filename-id))
    
    ;; For each keyword, get the id or add to the keywords table and get the id.
    (cl-loop for (keyword value begin) in keywords
	     if (not (member keyword org-db-ignore-keywords))
	     do
	     (setq keyword-id (or
			       (caar (sqlite-select org-db "select rowid from keywords where keyword = ?"
						    (list keyword)))
			       (sqlite-execute org-db "insert into keywords values (?, ?)"
					       (list nil keyword))
			       (caar (sqlite-select org-db "select last_insert_rowid()"))))
	     
	     ;; Now add to the file-keywords
	     (sqlite-execute org-db "insert into file_keywords values (?, ?, ?, ?, ?)"
			     (list nil filename-id keyword-id value begin)))))


(defun org-db-update-hashtags (filename-id parse-tree org-db)
  "Update hashtags in the buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating hashtags")
  ;; "#\\([^0-9!$%^&*+.]\\(\\w+[-_]?\\)+\\)"
  (let ((hashtags (save-excursion
		    (goto-char (point-min))
		    (let ((hashtags '()))
		      (while (re-search-forward hashtag-rx nil t)
			;; there are many scenarios we do not want to count these.
			;; no src-blocks as these are often false matches for comments
			;; These are not always reliable as it relies on font-lock
			(when (and (not (org-in-src-block-p))
				   ;; these are formulas in org lines sometimes
				   (not (eq 'org-meta-line (face-at-point)))
				   (not (save-match-data (equal 'src-block (car (org-element-context))))))
			  (push (cons (match-string-no-properties 1) (match-beginning 0)) hashtags)))
		      hashtags)))
	hashtag-id)

    ;; hashtags
    ;; first delete existing data on hashtags because it has probably changed.
    (sqlite-execute org-db "delete from file_hashtags where filename_id = ?"
		    (list filename-id))

    (cl-loop for (hashtag . pos) in hashtags do
	     ;; get hashtag id
	     (setq hashtag-id (or
			       (caar (sqlite-select org-db "select rowid from hashtags where hashtag = ?"
						    (list hashtag)))
			       (sqlite-execute org-db "insert into hashtags values (?, ?)"
					       (list nil hashtag))
			       (caar (sqlite-select org-db "select last_insert_rowid()"))))
	     (sqlite-execute org-db "insert into file_hashtags values (?, ?, ? ,?)"
			     (list nil filename-id hashtag-id pos)))))


(defun org-db-update-targets (filename-id parse-tree org-db)
  "Update the targets table."
  (org-db-log "Updating targets")

  (sqlite-execute org-db "delete from file_targets where filename_id = ?" (list filename-id))

  (org-element-map parse-tree 'target
    (lambda (target)
      (let ((target-id (or
			(caar (sqlite-select org-db "select rowid from targets where target = ?"
					     (list (org-element-property :value target))))
			
			(sqlite-execute org-db "insert into targets values (?, ?)"
					(list nil (org-element-property :value target)))
			(caar (sqlite-select org-db "select last_insert_rowid()")))))
	(sqlite-execute org-db "insert into file_targets values (?, ?, ?, ?)"
			(list nil filename-id target-id (org-element-property :begin target)))))))


(defun org-db-update-editmarks (filename-id parse-tree org-db)
  "Update the editmarks table in a buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating editmarks")
  (when (fboundp 'sem-get-editmarks)
    (sqlite-execute org-db "delete from file_editmarks where filename_id = ?" (list filename-id))

    (cl-loop for (em-type buffer (start . end) em-content) in (sem-get-editmarks)
	     do
	     (sqlite-execute org-db "insert into file_editmarks values (?, ?, ?, ?, ?)"
			     (list nil
				   filename-id
				   (symbol-name em-type)
				   em-content
				   start)))))


(defun org-db-update-email-addresses (filename-id parse-tree org-db)
  "Update emails table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating email addresses")
  (let ((emailaddresses (save-excursion
			  (goto-char (point-min))
			  (let ((emailaddresses '()))
			    (while (re-search-forward email-rx nil t)
			      ;; there are many scenarios we do not want to count these.
			      ;; no src-blocks as these are often false matches for comments
			      ;; These are not always reliable as it relies on font-lock
			      (when (and (not (org-in-src-block-p))
					 (not (save-match-data (equal 'src-block (car (org-element-context))))))
				(push (cons (match-string-no-properties 1) (match-beginning 0)) emailaddresses)))
			    emailaddresses)))
	email-address-id)

    (sqlite-execute org-db "delete from file_email_addresses where filename_id = ?" (list filename-id))

    (cl-loop for (email-address . pos) in emailaddresses do
	     ;; get email-address-id
	     (setq email-address-id (or
				     (caar (sqlite-select org-db "select rowid from email_addresses where email_address = ?"
							  (list email-address)))
				     (sqlite-execute org-db "insert into email_addresses values (?, ?)"
						     (list nil email-address))
				     (caar (sqlite-select org-db "select last_insert_rowid()"))))
	     (sqlite-execute org-db "insert into file_email_addresses values (?, ?, ?, ?)"
			     (list nil filename-id email-address-id pos)))))


(defun org-db-update-@-labels (filename-id parse-tree org-db)
  "Update @labels.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating @-labels")
  (let ((atlabels (save-excursion
		    (goto-char (point-min))
		    (let ((atlabels '()))
		      (while (re-search-forward @-rx nil t)
			;; there are many scenarios we do not want to count these.
			;; no src-blocks as these are often false matches for comments
			;; These are not always reliable as it relies on font-lock
			(when (and (not (org-in-src-block-p))
				   ;; these are formulas in org lines sometimes
				   (not (eq 'org-meta-line (face-at-point)))
				   (not (save-match-data (equal 'src-block (car (org-element-context))))))
			  (push (cons (match-string-no-properties 1) (match-beginning 0)) atlabels)))
		      atlabels)))
	atlabel-id)

    (sqlite-execute org-db "delete from file_atlabels where filename_id = ?" (list filename-id))
    
    (cl-loop for (atlabel . pos) in atlabels do
	     ;; get atlabel id
	     (setq atlabel-id (or
			       (caar (sqlite-select org-db "select rowid from atlabels where atlabel = ?"
						    (list atlabel)))
			       (sqlite-execute org-db "insert into atlabels values (?, ?)"
					       (list nil atlabel))
			       (caar (sqlite-select org-db "select last_insert_rowid()"))))

	     (sqlite-execute org-db "insert into file_atlabels values (?, ?, ?, ?)"
			     (list nil filename-id atlabel-id pos)))))


(defun org-db-update-links (filename-id parse-tree org-db)
  "Update links table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating links")
  (let* ((links (org-element-map parse-tree 'link
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
		     (org-element-property :begin link))))))

    ;; * delete old links
    (sqlite-execute org-db "delete from links where filename_id = ?" (list filename-id))

    ;;  ** add new links
    (cl-loop for link in links do
	     (sqlite-execute org-db "insert into links values (?, ?, ?, ?, ?, ?, ?, ?)"
			     link))))


(defun org-db-update-headlines (filename-id parse-tree org-db)
  "Update headlines table.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-log "Updating headlines")
  (let* ((headlines (org-element-map parse-tree 'headline 'identity))
	 hlv headline-id
	 tags tag-id
	 properties property-id
	 scheduled
	 deadline)

    ;; delete old headline data
    (sqlite-execute org-db "delete from headlines where filename_id = ?"
		    (list filename-id))

    (cl-loop for hl in headlines do
	     (save-excursion
	       (goto-char (org-element-property :begin hl))
	       (setq tags (mapcar 'org-no-properties (org-get-tags))
		     properties (org-entry-properties (org-element-property :begin hl) 'all)))

	     (setq
	      scheduled (when-let (ts (org-element-property :scheduled hl))
			  (org-timestamp-format ts "%Y-%m-%d %H:%M:%S"))

	      deadline (when-let (ts (org-element-property :deadline hl))
			 (org-timestamp-format ts "%Y-%m-%d %H:%M:%S")))

	     ;; headline vector to insert in table.
	     (setq hlv (vector
			nil
			filename-id
			(org-element-property :raw-value hl)
			(org-element-property :level hl)
			(when (org-element-property :todo-keyword hl)
			  (substring-no-properties
			   (org-element-property :todo-keyword hl)))
			;; todo-type is a symbol.
			(symbol-name (org-element-property :todo-type hl))
			(mapconcat 'org-no-properties
				   (org-element-property :archivedp hl)
				   " ")
			(org-element-property :commentedp hl)
			(org-element-property :footnote-section-p hl)
			(org-element-property :begin hl)
			;; this is really a tag string for easy searching in
			;; ivy because it seems tricky to build this from a
			;; query
			(when tags
			  (concat ":" (mapconcat
				       'substring-no-properties
				       tags ":")
				  ":"))
			;; priority
			(if (org-element-property :priority hl)
			    (char-to-string (org-element-property :priority hl))
			  nil)
			;; scheduled and deadline
			scheduled
			deadline))

	     ;; insert headline row and get headline-id
	     (org-db-log "Working on headline: %s" (org-element-property :raw-value hl))

	     (sqlite-execute org-db "insert into headlines values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
			     hlv)
	     (setq headline-id (caar (sqlite-select org-db "select last_insert_rowid()")))
	     
	     ;; remove old tag data
	     (sqlite-execute org-db "delete from headline_tags where headline_id = ?"
			     (list headline-id))
	     
	     (cl-loop for tag in tags
		      if (not (member tag org-db-ignore-tags))
		      do
		      (setq tag-id
			    (or
			     (caar (sqlite-select org-db "select rowid from tags where tag = ?" (list tag)))
			     (and
			      (sqlite-execute org-db "insert into tags values (?, ?)"
					      (list nil tag))
			      (caar (sqlite-select org-db "select last_insert_rowid()")))))
		      (sqlite-execute org-db "insert into headline_tags values (?, ?, ?)"
				      (list nil headline-id tag-id)))

	     ;; properties
	     (sqlite-execute org-db "delete from headline_properties where headline_id = ?"
			     (list headline-id))
	     
	     
	     (setq properties (save-excursion
				(goto-char (org-element-property :begin hl))
				(org-entry-properties)))

	     (cl-loop for (property . value) in properties
		      if (not (member property org-db-ignore-properties))
		      do
		      ;; get the property-id
		      (setq property-id
			    (or
			     ;; we have a property already
			     (caar (sqlite-select org-db "select rowid from properties where property = ?"
						  (list property)))
			     
			     ;; no property, so insert one, and get the rowid
			     (and
			      (sqlite-execute org-db "insert into properties values (?, ?)"
					      (list nil property))
			      (caar (sqlite-select org-db "select last_insert_rowid()")))))

		      ;; and the values
		      (sqlite-execute org-db "insert into headline_properties values (?, ?, ?, ?)"
				      (list nil
					    headline-id
					    property-id
					    (org-no-properties value)))))))


;; ;; ** update a buffer

(defun org-db-update-buffer (&optional force)
  "Update the entries in the database for the currently visited buffer.
Optional argument FORCE. if non-nil force the buffer to be added."
  (interactive "P")
  (save-buffer)
  (org-with-wide-buffer
   (let ((org-db (sqlite-open (expand-file-name org-db-name org-db-root))))
     (if (or force
	     (and
	      ;; file does not match an ignore pattern
	      (and org-db-ignore-file-regexps
		   (not (string-match (regexp-opt org-db-ignore-file-regexps)
				      (buffer-file-name))))
	      ;; file is not in database
	      (null
	       (caar (sqlite-select org-db "select rowid from files where filename = ?"
				    (list (buffer-file-name)))))
	      (org-db-log "update %s is a new file" (buffer-file-name)))
	     (and
	      ;; file does not match an ignore pattern
	      (and org-db-ignore-file-regexps
		   (not (string-match (regexp-opt org-db-ignore-file-regexps)
				      (buffer-file-name))))
	      ;; file is in database and it has changed, i.e. the md5 is not the same as on record
	      (not (string= (md5 (buffer-string))
			    (caar (sqlite-select org-db "select md5 from files where filename = ?"
						 (list (buffer-file-name))))))))
	 ;; We are updating the db here
	 (progn
	   (org-db-log "%s has changed." (buffer-file-name))
	   (let* ((filename-id (org-db-get-filename-id (buffer-file-name)))
		  (parse-tree (org-element-parse-buffer)))

	     ;; update the md5 for the file so we can tell later if it has changed.
	     (sqlite-execute org-db "update files set md5 = ? where rowid = ?"
			     (list (md5 (buffer-string)) filename-id))

	     (cl-loop for update-func in org-db-update-functions do
		      (funcall update-func filename-id parse-tree org-db))

	     (sqlite-execute org-db "update files set last_updated = ? where rowid = ?"
			     (list (format-time-string "%Y-%m-%d %H:%M:%S")
				   filename-id))))
       ;; No update required
       (org-db-log "no change detected"))
     (sqlite-close org-db))))



;; * the hooks


(defun org-db-hook-function ()
  "Function to run after starting ‘org-mode’."
  ;; Run when we open in case it changed from some external program. Only for
  ;; org and org_archive files, and not just when we enter org-mode for some
  ;; reason.
  (when (and (buffer-file-name)
	     (or (f-ext? (buffer-file-name) "org")
		 (f-ext? (buffer-file-name) "org_archive")))

    (with-org-db
     (unless (sqlite-select org-db "select filename from queue where filename = ?"
			    (list (buffer-file-name)))
       (sqlite-execute org-db "insert into queue values (?)"
		       (list (buffer-file-name)))))

    (org-db-log "added %s to the queue." (buffer-file-name))

    ;; add local after save hook in case this is a new file.
    (add-hook 'after-save-hook 'org-db-hook-function t t)))


(add-hook 'org-mode-hook 'org-db-hook-function)

;; * Idle timer to update

(defun org-db-process-queue (&optional force)
  "Update all the files in the queue (stored in the db).
Use a prefix ARG to FORCE the process instead of waiting for idle time."
  (interactive "P")
  (let ((filenames (mapcar 'car (with-org-db  (sqlite-select org-db "select filename from queue")))))
    (catch 'done
      (while filenames
	(unless (or force (current-idle-time))
	  (throw 'done nil))
	
	(let* ((filename (pop filenames))
	       (already-open (find-buffer-visiting filename))
	       (buf))
	  (if (not (file-exists-p filename))
	      (progn
		(org-db-remove-file filename)
		(with-org-db (sqlite-execute org-db "delete from queue where filename = ?"
					     (list filename))))
	    (setq buf (find-file-noselect filename))
	    (org-db-log "Updating %s" filename)
	    (with-current-buffer buf
	      (org-db-update-buffer force)
	      (with-org-db (sqlite-execute org-db "delete from queue where filename = ?"
					   (list filename))))
	    (unless already-open (kill-buffer buf)))))
      (org-db-log "Done processing org-db queue."))))


;; if we are idle for 5 minutes, process the queue.
(setq org-db-timer (run-with-idle-timer (* 60 5) t 'org-db-process-queue))


(defun org-db-status ()
  "Print a message of files scheduled for update."
  (interactive)
  (org-db-log "Files in queue for update: %s" org-db-queue)
  (switch-to-buffer "*org-db-log*"))



;; * Update the whole database

(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (cl-psetf (elt LIST el2) (elt LIST el1)
            (elt LIST el1) (elt LIST el2)))


(defun shuffle (LIST)
  "Shuffle the elements in LIST.
Used to randomize the order. Shuffling is done in place."
  (cl-loop for i in (reverse (number-sequence 1 (1- (length LIST))))
           do (let ((j (random (+ i 1))))
		(swap LIST i j)))
  LIST)


(defun org-db-refresh (&optional force)
  "Update all the files in the database.
Updates are done by `org-db-update-buffer'.

Use a prefix arg to FORCE updates."
  (interactive "P")

  (let* ((files)
	 (N)
	 buf already-open
	 md5)
    (with-org-db 
     (setq files (sqlite-select org-db "select filename from files order by date(last_updated)") 
	   N (length files)))

    (org-db-log "refresh %s Refreshing %s files" (current-time-string) N)

    ;; Shuffle the files so they are randomly updated. In case an early file
    ;; fails a lot, this increases the likely hood of getting past it.
    (cl-loop for (fname) in (shuffle files) for i from 0 to N
	     if (and fname (file-exists-p fname))
	     do
	     (org-db-log "refresh Refreshing %s of %s (%s)" i N fname)
	     (unless (string=
		      (setq md5 (s-trim (shell-command-to-string
					 (format "md5sum %s | cut -c -32" fname))))

		      (caar (with-org-db (sqlite-select org-db "select md5 from files where filename = ?"
							(list fname)))))
	       (with-timeout (60 (org-db-log "refresh ERROR: Timed out refreshing  %s." fname))
		 (setq already-open (find-buffer-visiting fname))
		 (setq buf (find-file-noselect fname))

		 (with-current-buffer buf
		   (condition-case err
		       (let ((enable-local-variables nil)
			     (org-mode-hook '())
			     (org-ref-activate-cite-links nil)
			     (org-ref-activate-ref-links nil)
			     (org-ref-activate-glossary-links nil))
			 (org-db-update-buffer force))
		     (org-db-log "refresh ERROR updating %s: %s" fname err)))

		 (unless already-open (kill-buffer buf))))
	     else
	     do
	     (org-db-log "refresh %s not found. Deleting from the database." fname)
	     (with-org-db
	      (sqlite-execute org-db "delete from files where filename = ?" (list fname))))))


(defun org-db-index (path  &optional recursive)
  "Index all the org-files in PATH.
Optional RECURSIVE is non-nil find files recursively."
  (interactive (list (read-directory-name "Path: ")
		     current-prefix-arg))
  (let* (already-open
	 buf
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
	     (org-db-log "index %s of %s - %s" i N fname)
	     (setq already-open (find-buffer-visiting fname))
	     (let* ((enable-local-variables nil)
		    (org-mode-hook '())
		    (org-ref-activate-cite-links nil)
		    (org-ref-activate-ref-links nil)
		    (org-ref-activate-glossary-links nil))
	       (with-current-buffer (or already-open (setq buf (find-file-noselect fname)))
		 (condition-case err
		     (org-db-update-buffer)
		   (org-db-log "index ERROR updating %s: %s" fname err))))
	     (unless already-open (kill-buffer buf)))))


(defun org-db-index-projects ()
  "Loop over known projects and index them."
  (interactive)
  (cl-loop for project in projectile-known-projects do
	   (ignore-errors
	     (org-db-index project t))))


(defun org-db-clean-db ()
  "Remove entries from the database where the file does not exist."
  (interactive)
  (with-org-db
   (cl-loop for (fname) in (sqlite-select org-db "select distinct filename from files") 
	    unless (and fname
			;; tramp filenames are a problem
			(not (s-starts-with? "/ssh:" fname))
			(not (s-starts-with? "/kubectl:" fname))
			(file-exists-p fname))
	    do
	    (org-db-log "clean %s was not found. Removing it." fname)
	    (sqlite-execute org-db "delete from files where filename = ?" (list fname)))))


(defun org-db-reset-db ()
  "Clear all entries from all tables."
  (interactive)
  (with-org-db
   (sqlite-execute org-db "delete from files")
   (sqlite-execute org-db "delete from tags")
   (sqlite-execute org-db "delete from properties")
   (sqlite-execute org-db "delete from keywords")
   (sqlite-execute org-db "delete from headlines")
   (sqlite-execute org-db "delete from headline_tags")
   (sqlite-execute org-db "delete from headline_properties")
   (sqlite-execute org-db "delete from file_keywords")
   (sqlite-execute org-db "delete from links")
   (sqlite-execute org-db "delete from hashtags")
   (sqlite-execute org-db "delete from file_hashtags")
   (sqlite-execute org-db "delete from atlabels")
   (sqlite-execute org-db "delete from file_atlabels")
   (sqlite-execute org-db "delete from email_addresses")
   (sqlite-execute org-db "delete from file_email_addresses")
   (sqlite-execute org-db "delete from src_blocks")
   (sqlite-execute org-db "delete from file_editmarks")
   (sqlite-execute org-db "delete from file_targets")
   (sqlite-execute org-db "delete from targets")
   (sqlite-execute org-db "delete from timestamps")
   (sqlite-execute org-db "delete from queue"))
  (org-db-log "reset Everything should be reset."))



;; * org-db contacts

(defun org-db-contacts-candidates ()
  "List of headings with EMAIL properties."
  (let ((contacts (with-org-db
		   (sqlite-select org-db "select
headlines.title, headline_properties.value,
headlines.tags, files.filename, files.last_updated, headlines.begin
from headlines
inner join headline_properties
on headlines.rowid = headline_properties.headline_id
inner join properties
on properties.rowid = headline_properties.property_id
inner join files
on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\""))))
    (cl-loop for (title email tags fname last-updated begin) in contacts
	     collect
	     (list (format "%30s | %40s | %s"
			   (s-pad-right 30 " " (s-trim title))
			   (s-pad-right 40 " " email)
			   (or tags ""))
		   :filename fname
		   :last-updated last-updated
		   :begin begin
		   :email email
		   :title (s-trim title)))))


(defun org-db--insert-contact-link (x)
  "Insert the contact associated with X."
  (let ((link)
	(candidate (cdr x)))
    ;; check if the file is up-to-date
    ;; (let ((actual-mod-time (float-time (file-attribute-modification-time
    ;; 					(file-attributes (plist-get candidate :filename))))))
    ;;   (when (org-time<= (plist-get candidate :last-updated) actual-mod-time)
    ;; 	(warn "%s is not up to date in org-db." (plist-get candidate :filename))
    ;; 	(with-current-buffer (find-file-noselect (plist-get candidate :filename))
    ;; 	  (save-buffer)
    ;; 	  (org-db-update-buffer t))))
    ;; (save-excursion
    ;;   (with-current-buffer
    ;; 	  (find-file-noselect
    ;; 	   (plist-get candidate :filename))
    ;; 	(goto-char (plist-get candidate :begin))

    ;; 	;; Check we are looking at the right place
    ;; 	(unless (and (looking-at org-heading-regexp)
    ;; 		     (string= (plist-get candidate :email) (org-entry-get (point) "EMAIL")))
    ;; 	  (error "It does not appear we are looking at the right place here:\n%s" (plist-get candidate :filename)))

    ;; 	(setq link (format
    ;; 		    "[[contact:%s][%s]]"
    ;; 		    (org-entry-get (point) "EMAIL")
    ;; 		    (nth 4 (org-heading-components))))))
    ;; (when (looking-back "]" 1)
    ;;   (insert ", "))
    ;; (insert link)
    (insert (format "[[contact:%s][%s]]"
		    (plist-get candidate :email)
		    (plist-get candidate :title)))))


(defun org-db--assign-contact (x)
  "Assign current heading to contact X.
Sets heading TODO state and prompts for deadline if there is not one."
  (interactive)
  (let ((emails (org-entry-get-multivalued-property (point) "ASSIGNEDTO")))
    (setq emails (append emails (list (plist-get (cdr x) :email))))
    (apply 'org-entry-put-multivalued-property (point)
	   "ASSIGNEDTO" emails))

  (unless (string= "TODO" (org-get-todo-state))
    (org-todo "TODO"))

  (unless (org-get-deadline-time (point))
    (org-deadline nil)))


(defun org-db--open-contact (x)
  "Open contact X.
This is a little flexible. Sometimes :begin is out of date so
instead we use search."
  (find-file (plist-get (cdr x) :filename))
  (goto-char (car (org-ql-query :select #'point
				:from (current-buffer)
				:where `(property "EMAIL" ,(plist-get (cdr x) :email)))))
  (outline-show-entry))


(defun org-db--insert-contact (x)
  "Insert \"name\" <email> for X at point.
If point is not looking back on a space insert a comma separator."
  (unless (and (looking-back " " 1)
	       (not (bolp)))
    (insert ","))
  (insert
   (format "\"%s\" <%s>"
	   (plist-get (cdr x) :title)
	   (plist-get (cdr x) :email))))


(defun org-db--email-contact (x)
  "Open an email to the contact"
  (interactive)
  (compose-mail)
  (message-goto-to)
  (insert (plist-get (cdr x) :email))
  (message-goto-subject))


(defun org-db--insert-@-label (x)
  "Insert an @ label"
  (interactive)
  (insert (format "@%s" (s-join "-" (s-split " " (plist-get (cdr x) :title))))))


(defun org-db--multi-contact (candidates)
  "Act on CANDIDATES with choice of actions"
  (let ((action (ivy-read "Action: " '(("insert")
				       ("links")
				       ("email"))
			  :initial-input "^")))
    (cond
     ((string= action "insert")
      (unless (or (bolp)
		  (looking-back " " 1))
	(insert ","))
      (insert (mapconcat (lambda (x)
			   (format "\"%s\" <%s>"
				   (plist-get (cdr x) :title)
				   (plist-get (cdr x) :email)))
			 candidates
			 ",")))
     ((string= action "email")
      (compose-mail)
      (message-goto-to)
      (insert (mapconcat (lambda (x)
			   (plist-get (cdr x) :email))
			 candidates
			 ","))
      (message-goto-subject))
     ((string= action "links")
      (mapcar 'org-db--insert-contact-link candidates)))))


(defvar org-db-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-h") 'org-db--contacts-help)
    map))


(defun org-db--contacts-help ()
  (interactive)
  (org-link-open-from-string
   (format
    "[[%s::*Contacts]]"
    (expand-file-name "scimax.org" scimax-dir))))


(defun org-db-contacts ()
  "Ivy command to select an `org-db' contact."
  (interactive)
  (let* ((org-db-contacts-marked-candidates '())
	 (candidates (org-db-contacts-candidates)))
    (ivy-read "Contact: " candidates
	      :keymap org-db-contacts-keymap
	      :caller 'org-db-contacts
	      :multi-action 'org-db--multi-contact
	      :action '(1
			("i" org-db--insert-contact "insert")
			("o" org-db--open-contact "open")
			("l" org-db--insert-contact-link "Insert link")
			("a" org-db--assign-contact "Assign to heading")
			("e" org-db--email-contact "Email contact")
			("2" org-db--insert-@-label "Insert @label")
			("?" org-db--contacts-help "Help")))))


(defun org-db-contact-transformer (s)
  "Make marked candidates look red."
  (if (s-starts-with? ">" s)
      (propertize s 'face 'font-lock-warning-face)
    s))

(ivy-configure 'org-db-contacts
  :display-transformer-fn 'org-db-contact-transformer)


;; * org-db-locations
(defun org-db-locations-candidates ()
  "Return a list of headings with an ADDRESS property."
  (with-org-db
   (let ((locations (with-org-db  (sqlite-select org-db "select
headlines.title, headline_properties.value, headlines.tags, files.filename, headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"ADDRESS\""))))
     (cl-loop for (title address tags fname begin) in locations
	      collect
	      (list (format "%60s | %70s | %s"
			    (s-trim title)
			    address
			    (or tags ""))
		    :filename fname :begin begin)))))



(defun org-db-locations ()
  "Open a location in `org-db'."
  (interactive)
  (let ((candidates (org-db-locations-candidates)))
    (ivy-read "Location: " candidates :action '(1
						("o" (lambda (x)
						       (find-file (plist-get (cdr x) :filename))
						       (goto-char (plist-get (cdr x) :begin)))
						 "open")
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
							 (insert link)))
						 "insert link")))))


;; * org-db src-blocks 
(defun org-db-src-blocks (&optional project)
  "Search src blocks.
Optional PROJECT prefix arg to limit to current project."
  (interactive "P")

  (let* ((src-blocks (with-org-db
		      (sqlite-select
		       org-db
		       (format "select src_blocks.language, src_blocks.contents,
src_blocks.begin, files.filename from src_blocks inner join files on files.rowid = src_blocks.filename_id%s"
			       (if project
				   (format " where files.filename like \"%s%%\"" (projectile-project-root))
				 "")))))
	 (candidates (cl-loop for (language contents begin filename) in src-blocks collect
			      (list (format "%s: %s" language contents)
				    :filename filename :begin begin))))

    (ivy-read "query: " candidates
	      :action (lambda (candidate)
			(find-file (plist-get (cdr candidate) :filename))
			(goto-char (plist-get (cdr candidate) :begin))))))


;; * org-db headings
(defun org-db-heading-candidates (&optional project)
  "Return heading candidates completion."
  (let* ((headings (with-org-db
		    (sqlite-select
		     org-db
		     (format "
select headlines.level, headlines.todo_keyword, headlines.title, headlines.tags,
files.filename, headlines.begin, files.last_updated from headlines
inner join files 
on files.rowid = headlines.filename_id
%s
order by files.last_updated desc"
			     (if project
				 (format "where files.filename like \"%s%%\"" (projectile-project-root))
			       "")))))
	 (candidates (cl-loop for (level todo title tags filename begin last-updated) in headings
			      collect
			      (cons
			       (format "%100s|%20s|%s|%s"
				       (s-pad-right 100 " " (concat  (make-string level (string-to-char "*")) " "
								     (if todo
									 (concat todo " ")
								       "")
								     title))
				       (s-pad-right 20 " " (or tags ""))
				       filename last-updated)
			       (list
				:file filename
				:last-updated last-updated
				:begin begin
				:title title)))))
    candidates))


(defun org-db-headings--open (x)
  "Open the heading X."
  (interactive)
  (find-file (plist-get (cdr x) :file))
  (goto-char (plist-get (cdr x) :begin))
  (org-show-context))


(defun org-db-headings--store-link (x)
  "Store a link to X."
  (interactive)
  (find-file (plist-get (cdr x) :file))
  (goto-char (plist-get (cdr x) :begin))
  (org-store-link nil))


(defun org-db-headings--insert-link (x)
  "Insert a link to X"
  (interactive)
  (insert (format "[[file:%s::*%s][%s]]"
		  (plist-get (cdr x) :file)
		  (plist-get (cdr x) :title)
		  (plist-get (cdr x) :title))))


;;;###autoload
(defun org-db-headings (&optional project)
  "Use ivy to open a heading with completion."
  (interactive "P")
  (let* ((candidates (org-db-heading-candidates project)))
    (ivy-read "heading: " candidates
	      :action
	      '(1
		("o" org-db-headings--open "Open to heading.")
		("l" org-db-headings--insert-link "Insert link to heading")
		("s" org-db-headings--store-link "Store link to heading.")))))


;; ;; * org-db files

(defun org-db-files ()
  "Open a file in ‘org-db’ with completion."
  (interactive)
  (let ((candidates (mapcar 'car (with-org-db
				  (sqlite-select org-db "select filename from files order by filename")))))
    (ivy-read "File: " candidates
	      :action
	      '(1
		("o" (lambda (candidate)
		       (find-file candidate))
		 "Open file")
		("r" (lambda (candidate)
		       (with-org-db
			(sqlite-execute org-db "delete from files where filename = ?" (list candidate))))
		 "Remove file from org-db")
		("u" (lambda (candidate)
		       (cl-loop for (filename) in (with-org-db (sqlite-select org-db "select filename from files"))
				do
				(unless (file-exists-p filename)
				  (org-db-remove-file filename))))
		 "Update the file list")))))


(defun org-db-recentf ()
  "Open a recent file in ‘org-db’ with completion.
Recent is sorted by last-updated in the database."
  (interactive)
  (let ((candidates (cl-loop for entry in
			     (with-org-db
			      (sqlite-select org-db "select filename, last_updated from files order by date(last_updated) desc"))
			     collect
			     (cons (format "%s %s" (cdr entry) (car entry)) (car entry)))))
    (ivy-read "File: " candidates
	      :action
	      '(1
		("o" (lambda (candidate)
		       (find-file (cdr candidate)))
		 "Open file")
		("u" (lambda (candidate)
		       (cl-loop for (filename) in (with-org-db (sqlite-select org-db "select filename from files"))
				do
				(unless (file-exists-p filename)
				  (org-db-remove-file filename))))
		 "Update the file list")))))


;; * org-db-links

(defun org-db-links--open (x)
  "Open the link X."
  (interactive)
  (find-file (plist-get (cdr x) :filename))
  (goto-char (plist-get (cdr x) :begin))
  (org-show-entry))


(defun org-db-links ()
  "Open a link."
  (interactive)
  (let ((candidates (cl-loop
		     for (rl fn bg) in
		     (with-org-db
		      (sqlite-select org-db "select raw_link, files.filename, begin
from links
inner join files on links.filename_id = files.rowid order by filename"))
		     collect
		     ;; (candidate :filename :begin)
		     (list (format "%-80s%s" rl fn) :filename fn :begin bg))))
    (ivy-read "link: " candidates :action '(1
					    ("o" org-db-links--open "Open to link")))))


(defun org-db-bookmark ()
  "Open a bookmark.
A bookmark is any heading with a url property. If the url starts
with http it opens in a browser, otherwise we assume it is an
org-link."
  (interactive)
  (let ((candidates (cl-loop for (title tags value) in
			     (with-org-db
			      (sqlite-select org-db "select
headlines.title, headlines.tags, headline_properties.value
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"URL\"
"))
			     collect (cons (format "%s %s" title tags)
					   value))))
    (ivy-read "bookmark: " candidates :action '(1
						("o"
						 (lambda (cand)
						   (cond
						    ;; A regular url to open in a browser
						    ((string-prefix-p "http" (string-trim (cdr cand)))
						     (browse-url (cdr cand)))
						    ;; an org-link
						    (t
						     (org-link-open-from-string (cdr cand)))))
						 "Open to bookmark")))))


;; * org-db-targets
(defun org-db-target ()
  "Search for targets in org-files."
  (interactive)
  (let* ((results (with-org-db
		   (sqlite-select org-db "select target, file_targets.begin, files.filename
from targets
left join file_targets on targets.rowid = file_targets.target_id
inner join files
on files.rowid = file_targets.filename_id")))
	 (candidates (cl-loop for (target begin fname) in results collect
			      (list
			       (format "%-50s%s" (s-pad-right 30 " " target) fname)
			       fname
			       begin))))
    (ivy-read "Target: " candidates :action (lambda (x)
					      (find-file (cl-second x))
					      (goto-char (cl-third x))))))

;; * org-db-hashtags
;; The idea here is to use #hashtags in org files so we can search them later.
;; These are hard, there are many reasons for #something, including comments in
;; python blocks, #'in-elisp, etc. I sort of like the idea though, because it
;; provides a way to jump to paragraphs. Maybe it would be better to use radio
;; targets for this though.

(defun org-db-delete-hashtag (hashtag)
  "Delete HASHTAG from the db."
  (let* ((hashtag-id (caar (with-org-db
			    (sqlite-select org-db "select rowid from hashtags where hashtag = ?"
					   (list hashtag))))))
    (if (null hashtag-id)
	(org-db-log "Weird. no hashtag id found for %s" hashtag)
      (org-db-log "removing %s entries: %s" hashtag hashtag-id)

      (with-org-db
       (sqlite-execute org-db "delete from file_hashtags where hashtag_id = ?"
		       (list hashtag-id))
       
       (sqlite-execute org-db "delete from hashtags where rowid = ?"
		       (list hashtag-id))))))


(defun org-db-delete-hashtags ()
  "Delete all the hashtags in the db."
  (interactive)
  (with-org-db
   (sqlite-execute org-db "delete from hashtags")
   (sqlite-execute org-db "delete from file_hashtags")))


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
  ;; 1. should we allow hashtags in src-blocks? This leads to many false hashtags
  ;;    in Python for example for comments..
  ;; [2020-07-28 Tue] I am eliminating src-blocks for hash-tags.
  (let ((p (point))
	(lbp (line-beginning-position)))
    (if (string= major-mode "org-mode")
	(save-excursion
	  (while (and
		  (not (org-in-src-block-p))
		  (not (looking-at hashtag-rx))
  		  (>= (point) lbp))
	    (backward-char))
	  (when (and (>= p (match-beginning 0))
  		     (<= p (match-end 0)))
	    (match-string-no-properties 1)))
      (word-at-point))))


(defun org-db-hashtags--open (x)
  "Open the hashtag entry X."
  (interactive)
  (find-file (plist-get (cdr x) :filename))
  (goto-char (plist-get (cdr x) :begin))
  (outline-show-entry))


(defun org-db-hashtags--insert (x)
  "Insert the hashtag X."
  (interactive)
  (insert (concat "#" (plist-get (cdr x) :hashtag))))


(defun org-db-hashtags--other-files (x)
  "Open list of other files containing hashtag X."
  (let* ((hashtag (org-no-properties (plist-get (cdr x) :hashtag)))
	 (results (with-org-db
		   (sqlite-select org-db "select hashtag, file_hashtags.begin, files.filename
from hashtags
left join file_hashtags on hashtags.rowid = file_hashtags.hashtag_id
inner join files
on files.rowid = file_hashtags.filename_id
where hashtag = ?" (list hashtag))))
	 (candidates (cl-loop for (hashtag begin fname) in results
			      collect (cons fname begin)))
	 (choice (ivy-read "File: " candidates)))
    (find-file choice)
    (goto-char (cdr (assoc choice candidates)))))


(defun org-db-hashtags--delete (x)
  "Delete entries for hashtag."
  (let* ((hashtag (plist-get (cdr x) :hashtag)))
    (org-db-delete-hashtag hashtag)))


(defun org-db-hashtags ()
  "Open a #hashtag.
If the cursor is on a hashtag, it uses that as the initial input.
I am not sure how to do multiple hashtag matches right now, that
needs a fancier query."
  (interactive)
  (let* ((tip (looking-at-hashtag))
	 (hashtag-data (with-org-db
			(sqlite-select org-db "select hashtag, file_hashtags.begin, files.filename
from hashtags
left join file_hashtags on hashtags.rowid = file_hashtags.hashtag_id
inner join files
on files.rowid = file_hashtags.filename_id")))
	 (candidates (cl-loop for (hashtag begin fname) in hashtag-data
			      collect (list (format "#%40s  %s" (s-pad-right 40 " " hashtag) fname)
					    :hashtag hashtag :begin begin :filename fname))))

    (ivy-read "#hashtag: " candidates :initial-input tip
	      :action
	      '(1
		("o" org-db-hashtags--open "Open file at hashtag")
		("i" org-db-hashtags--insert "Insert hashtag at point.")
		("O" org-db-hashtags--other-files "Other files with hashtag")
		("D" org-db-hashtags--delete "Delete this hashtag")))))


;; * org-db-@
(defun org-db-@ ()
  "Jump to an @label."
  (interactive)
  (let* ((@-data (with-org-db
		  (sqlite-select org-db "select atlabel, file_atlabels.begin, files.filename
from atlabels
left join file_atlabels on atlabels.rowid = file_atlabels.atlabel_id
inner join files
on files.rowid = file_atlabels.filename_id")))
	 (candidates (cl-loop for (atlabel begin fname) in @-data
			      collect (list (format "@%-40s  %s" atlabel fname)
					    :@-label atlabel :begin begin :filename fname))))

    (ivy-read "@label: " candidates
	      :action
	      '(1
		("o" (lambda (x)
		       "Open the @label entry X."
		       (interactive)
		       (find-file (plist-get (cdr x) :filename))
		       (goto-char (plist-get (cdr x) :begin))
		       (unless (org-before-first-heading-p)
			 (outline-show-entry)))
		 "Open file at @label")))))

;; ;; * org-id integration
;; ;; org-id-goto is very slow for me. This function can replace it.
(defun org-db-goto-id (id)
  "Open an org file at ID."
  (let* ((result (with-org-db
		  (sqlite-select org-db "select files.filename,headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"ID\" and headline_properties.value = ?"
				 (list id))))
	 match fname p)
    (cond
     ((and result
	   (= 1 (length result)))
      (setq match (cl-first result)
	    fname (cl-first match)
	    p (cl-second match))
      (find-file fname)
      (goto-char p))

     ((> (length result) 1)
      ;; TODO maybe use ivy to select?
      (org-db-log "(%s) matches: %s result" (length result) result))

     (t
      (org-db-log "No match found")))))


(defun org-db-toggle-org-id ()
  "Toggle using `org-db' to follow org-id links."
  (interactive)
  (if (not (get 'org-db-goto-id 'enabled))
      (progn
	(advice-add 'org-id-goto :override #'org-db-goto-id)
	(put 'org-db-goto-id 'enabled t)
	(org-db-log "org-db-goto-id advice enabled."))
    (advice-remove 'org-id-goto #'org-db-goto-id)
    (put 'org-db-goto-id 'enabled nil)
    (org-db-log "org-db-goto-id advice disabled.")))


;; * tag search
;; We don't really need a special tag search. You can do it with
;; `org-db-headings' by adding : into your search. I am leaving this note here
;; in case I ever think of trying to write this again!


;; * property search

(defun org-db-properties (property pattern)
  "Search org-db for entries where PROPERTY matches PATTERN.
PATTERN follows sql patterns, so % is a wildcard.
It is not currently possible to do multiple property searches."
  (interactive (list (completing-read "Property: "
				      (-flatten (with-org-db
						 (sqlite-select org-db "select property from properties"))))
		     (read-string "Pattern: ")))
  (let* ((results (with-org-db
		   (sqlite-select org-db "select
headlines.title,
properties.property,
headline_properties.value,
files.filename, files.last_updated, headlines.begin
from headlines
inner join headline_properties
on headlines.rowid = headline_properties.headline_id
inner join properties
on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = ? and headline_properties.value like ?"
				  (list property pattern))))
	 (candidates (cl-loop for (title property value fname last-updated begin) in results
			      collect
			      (list (format "%s | %s=%s | %s" title property value fname)
				    :filename fname :begin begin))))

    (ivy-read "Choose: " candidates
	      :action (lambda (x)
			(let ((candidate (cdr x)))
			  (find-file (plist-get candidate :filename))
			  (goto-char (plist-get candidate :begin)))))))


;; * search editmarks
(defun org-db-editmarks ()
  "Search the editmarks table in org-db."
  (interactive)

  (let* ((results (with-org-db
		   (sqlite-select org-db "select
files.filename, file_editmarks.begin, file_editmarks.content, file_editmarks.type
from file_editmarks
inner join files on files.rowid = file_editmarks.filename_id")))
	 (candidates (cl-loop for (fname begin content type) in results
			      collect
			      (list
			       (format "%s | %s | %s" type fname (s-trim content))
			       :filename fname
			       :begin begin))))

    (ivy-read "Choose: " candidates :action (lambda (x)
					      (find-file (plist-get (cdr x) :filename))
					      (goto-char (plist-get (cdr x) :begin))))))


;; * search for email addresses

(defun org-db-email-addresses ()
  "Search for email addresses."
  (interactive)
  (let* ((results (with-org-db
		   (sqlite-select org-db "select files.filename, file_email_addresses.begin,
email_addresses.email_address from file_email_addresses
inner join files on files.rowid = file_email_addresses.filename_id
inner join email_addresses on email_addresses.rowid = file_email_addresses.email_address_id")))
	 (candidates (cl-loop for (fname begin email-address) in results
			      collect
			      (list
			       (format "%s | %s " email-address fname)
			       :filename fname
			       :begin begin))))

    (ivy-read "Choose: " candidates :action (lambda (x)
					      (find-file (plist-get (cdr x) :filename))
					      (goto-char (plist-get (cdr x) :begin))))))


;; * backlinks
(defun org-db-backlink-candidates ()
  (let* ((buffer-name (buffer-file-name))
	 (fname (file-name-nondirectory buffer-name))
	 ;; I am assuming we only want to match on file links
	 (potential-matches (with-org-db
			     (sqlite-select org-db "select filename, path, begin from links
left join files on links.filename_id = files.rowid
where links.type = 'file' and path like ?"
					    (list (concat "%" fname "%")))))
	 (matches (cl-loop for (src-filename path begin) in potential-matches collect
			   (cond
			    ;; path is absolute and points to file
			    ((and (file-name-absolute-p path)
				  (string= path buffer-name))
			     (list src-filename begin))
			    ;; path is relative but expands to buffer-name relative to src-filename
			    ((string= (expand-file-name path (file-name-directory src-filename)) buffer-name)
			     (list src-filename begin))
			    (t
			     nil)))))
    (cl-loop for match in matches
	     if (not (null match))
	     collect
	     (list
	      (format "%s | %s"
		      (first match)
		      (with-temp-buffer
			(insert-file-contents (first match))
			(goto-char (second match))
			(buffer-substring (line-beginning-position) (line-end-position))))
	      (first match)
	      (second match)))))


(defun org-db-backlinks ()
  "Find backlinks to the current file.
This finds other files with links in them to this file."
  (interactive)
  (ivy-read "Backlink: " (org-db-backlink-candidates) :action (lambda (match)
								(find-file (second match))
								(goto-char (third match)))))

;; * timestamp search
(defun org-db-timestamps ()
  (interactive)
  (let ((candidates (with-org-db (sqlite-select org-db "select context, files.filename, begin from timestamps
inner join files on timestamps.filename_id = files.rowid"))))
    (ivy-read "Timestamp: " candidates
	      :action (lambda (cand)
			(find-file (nth 1 cand))
			(goto-char (nth 2 cand))
			(org-show-context)))))



;; * org-db hydra
;; this is just for convenience, so I don't have to remember everything.

(defhydra org-db (:color blue :hint nil)
  "org-db search
"
  ("h" org-db-headings "headlines" :column "org element")
  ("p" org-db-properties "properties" :column "org element")
  ("s" org-db-src-blocks "src-blocks" :column "org element")
  ("k" org-db-links "links" :column "org element")
  ("b" org-db-backlinks "backlinks" :column "org element")
  ("i" org-db-images "images" :column "org element")
  ("t" org-db-fulltext-search "full text" :column "org element")
  ("T" org-db-timestamps "timestamp" :column "org element")

  ("c" org-db-contacts "contacts" :column "derived")
  ("l" org-db-locations "locations" :column "derived")
  ("u" org-db-bookmark "bookmark" :column "derived")

  ("e" org-db-email-addresses "email" :column "pattern")
  ("m" org-db-editmarks "editmarks"  :column "pattern")
  ("3" org-db-hashtags "hashtags"  :column "pattern")
  ("2" org-db-@ "@-labels"  :column "pattern")
  ("g" org-db-target "target" :column "pattern")

  ("f" org-db-files "files" :column "pattern")
  ("r" org-db-recentf "recent files" :column "pattern")
  ("U" (org-db-update-buffer t) "update buffer" :column "misc"))


;; * debugging
(defun org-db-tables ()
  "Get a list of tables in `org-db'."
  (interactive)
  (split-string
   (shell-command-to-string (format "sqlite3 %s \".tables\"" (expand-file-name org-db-name org-db-root)))))


(defun org-db-table-columns (tblname)
  "List the columns in TBLNAME."
  (interactive (list (ivy-read "Table: " (org-db-tables))))
  (message "%s"
	   (shell-command-to-string (format "sqlite3 %s \"PRAGMA table_info(%s)\""
					    (expand-file-name org-db-name org-db-root)
					    tblname))))

(defun org-db-report ()
  "Create a summary report of the db and its status."
  (interactive)
  (let ((buf (get-buffer-create "*org-db-report*"))
	(tables (split-string
		 (shell-command-to-string (format "sqlite3 %s \".tables\""
						  (expand-file-name org-db-name org-db-root))))))
    (with-current-buffer buf
      (erase-buffer)
      (org-mode)
      (insert "#+title: org-db report\n\n")

      (insert "* Queue\n\n")
      (cl-loop for (fname) in (with-org-db org-db
					   (sqlite-select org-db "select * from queue"))
	       do
	       (insert (format "- %s\n" fname)))
      (insert "\n")
      (insert "[[elisp:org-db-report]]\n\n")
      (insert "[[elisp:(progn (org-db-process-queue t) (org-db-report))][Process queue]]\n\n")
      
      (insert "* Files\n\n")
      (insert (format "org-db is at %s (%1.1f MB)\n\n"
		      (expand-file-name org-db-name org-db-root)
		      (/  (float (file-attribute-size (file-attributes (expand-file-name org-db-name org-db-root)))) 1024 1024)))

      (insert (format "org-db contains:\n- %d files\n"
		      (with-org-db
		       (caar (sqlite-select org-db "select count() from files")))))
      (insert (format "- %d headlines\n"
		      (with-org-db
		       (caar (sqlite-select org-db "select count() from headlines")))))

      (insert "\n* tables\n\n")

      (cl-loop for table in tables do
	       (insert (format "** %s\n\n" table))
	       (let ((columns (split-string
			       (string-trim (shell-command-to-string (format "sqlite3 %s \"PRAGMA table_info(%s)\""
									     (expand-file-name org-db-name org-db-root)
									     table)))
			       "\n"))
		     (statement (format "select count () from %s" (make-symbol table))))
		 (cl-loop for line in columns do
			  (when line
			    (insert (concat "|" line "|\n"))))
		 (insert "\n\n")
		 (org-entry-put (point) "ROWCOUNT" (format "%s" (caar (with-org-db (sqlite-select org-db statement)))))))

      (org-table-map-tables (lambda () (org-table-align))))
    (pop-to-buffer buf)
    (beginning-of-buffer)))


;; * End
(provide 'org-db)

;;; org-db.el ends here
