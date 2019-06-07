;;; org-db.el --- An org database

;;; Commentary:
;;
(require 'cl)
(require 's)    ; for s-trim
(require 'org)
(require 'emacsql-sqlite)

;;; Code:

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
	    hlv
	    healine-id
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

       (loop for (keyword value begin) in keywords
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

       ;; * Headlines delete the headlines from this file. Should cascade delete
       ;; tags, properties and keywords.
       (emacsql org-db [:delete :from headlines :where (= headlines:filename-id $s1)]
		filename-id)

       (loop for hl in headlines do
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

	     (loop for tag in tags
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

	     (loop for (property . value) in properties
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


;; * the hooks

(add-hook 'org-mode-hook 'org-db-hook-function)


;; * Idle timer to update

(defun org-db-process-queue (&optional now)
  "Update all the files in `org-db-queue'.
Use a prefix ARG to process now."
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
    (loop for (fname) in files for i from 0 to N
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
	  do (emacsql org-db [:delete :from files :where (= filename $s1)] fname))))


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
					 (f-ext? f "org_archive"))
				     (and org-db-ignore-file-regexps
					  (not (string-match (regexp-opt org-db-ignore-file-regexps)
							     f)))))
			 recursive))
	 (N (length files)))
    (loop for fname in files
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
  (loop for (fname) in (emacsql org-db [:select :distinct [filename] :from files])
	unless (file-exists-p fname)
	do
	(org-db-log "%s was not found. Removing it." fname)
	(emacsql org-db [:delete :from files :where (= filename $s1)] fname)))



;; * org-db contacts
(defun org-db-contacts-candidates ()
  "List of headings with EMAIL properties."
  (let ((contacts (emacsql org-db
			   [:select [headlines:title
				     headline-properties:value
				     headlines:tags files:filename headlines:begin]
				    :from headlines
				    :inner :join headline-properties
				    :on (=  headlines:rowid headline-properties:headline-id)
				    :inner :join properties
				    :on (= properties:rowid headline-properties:property-id)
				    :inner :join files :on (= files:rowid headlines:filename-id)
				    :where (= properties:property "EMAIL")])))
    (loop for (title email tags fname begin) in contacts
	  collect
	  (list (format "%30s | %40s | %s"
			(s-pad-right 30 " " (s-trim title))
			(s-pad-right 40 " " email)
			(or tags ""))
		:filename fname :begin begin))))


(defun org-db-contacts ()
  "Helm selector for an org-db-contact."
  (interactive)
  (helm :sources `(((name . "contacts")
		    (candidates . ,(org-db-contacts-candidates))
		    (action . (("Open location" . (lambda (candidate)
						    (find-file (plist-get candidate :filename))
						    (goto-char (plist-get candidate :begin))))
			       ("Insert link" . (lambda (candidate)
						  (let ((link))
						    (with-current-buffer
							(find-file-noselect
							 (plist-get candidate :filename))
						      (goto-char (plist-get candidate :begin))
						      (setq link (format
								  "[[location:%s][%s]]" (org-id-get-create)
								  (nth 4 (org-heading-components)))))
						    (insert link))))))))))

;; (ivy-read "Contact: " (org-db-contacts-candidates))

;; * org-db-locations

(defun org-db-locations-candidates ()
  "Return a list of headings with an ADDRESS property."
  (let ((locations (emacsql org-db [:select [headlines:title headline-properties:value headlines:tags files:filename headlines:begin]
					    :from headlines
					    :inner :join headline-properties :on (=  headlines:rowid headline-properties:headline-id)
					    :inner :join properties :on (= properties:rowid headline-properties:property-id)
					    :inner :join files :on (= files:rowid headlines:filename-id)
					    :where (= properties:property "ADDRESS")])))
    (loop for (title address tags fname begin) in locations
	  collect
	  (list (format "%60s | %70s | %s"
			(s-trim title)
			address
			(or tags ""))
		:filename fname :begin begin))))


(defun org-db-locations ()
  "Helm selector for org-locations."
  (interactive)
  (helm :sources (helm-build-sync-source "locations"
		   :candidates (org-db-locations-candidates)
		   :fuzzy-match t
		   :action '(("Open location" . (lambda (candidate)
						  (find-file (plist-get candidate :filename))
						  (goto-char (plist-get candidate :begin))))
			     ("Insert link" . (lambda (candidate)
						(let ((link))
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
							     files:filename headlines:begin]
					    :from headlines
					    :inner :join files
					    :on (= files:rowid headlines:filename-id)]))
	 (candidates (loop for (level title tags filename begin) in headings
			   collect
			   (cons
			    (format "%100s|%20s|%s"
				    (s-pad-right 100 " " (concat  (make-string level (string-to-char "*")) " " title))
				    (s-pad-right 20 " " (or tags ""))
				    filename)
			    (list
			     :file filename
			     :begin begin)))))
    candidates))

(defun org-db-open-heading ()
  "Use helm to select and open a heading."
  (interactive)
  (helm :sources (helm-build-sync-source "org-db-headlines"
		   :candidates (org-db-heading-candidates)
		   :fuzzy-match nil
		   :action '(("Open" . (lambda (x)
					 (find-file (plist-get x :file))
					 (goto-char (plist-get x :begin))
					 (org-show-entry)))
			     ("Collect in buffer" . (lambda (_)
						      (switch-to-buffer (get-buffer-create "*org-db*"))
						      (org-mode)
						      (loop for hl in (helm-marked-candidates) do
							    (save-excursion
							      (with-current-buffer (find-file-noselect (plist-get hl :file))
								(goto-char (plist-get hl :begin))
								(org-copy-subtree)))
							    (org-yank)
							    (insert "\n"))))
			     ("Save to org-db-marked-candidates" . (lambda (_)
								     "This saves the marked candidates so you could use them in another code."
								     (setq org-db-marked-candidates (helm-marked-candidates))))))))


;; * org-db files

(defun org-db-open-file ()
  "Open a file in ‘org-db’ with completion."
  (interactive)
  (find-file (completing-read "File: " (mapcar 'car (emacsql org-db [:select [filename]
									     :from files
									     :order :by filename])))))

(defun org-db-open-recent-file ()
  "Open a recent file in ‘org-db’ with completion."
  (interactive)
  (let ((candidates (mapcar (lambda (x)
			      (cons (format "%s %s" (cdr x) (car x)) (car x)))
			    (emacsql org-db [:select [filename last-updated]
						     :from files
						     :order :by last-updated :desc]))))
    (find-file (cdr (assoc (ivy-read "File: " candidates) candidates)))))

;; * org-db-links
(defun org-db-open-link-in-file ()
  "Search the link database."
  (interactive)
  (helm :sources
	(helm-build-sync-source "links"
	  :candidates (loop for (rl fn bg) in  (emacsql org-db [:select [raw-link filename begin ]
									:from links
									:left :join files :on (= links:filename-id files:rowid)
									:order :by filename])
			    collect
			    (list (format "%s | %s" rl fn) :filename fn :begin bg))
	  :fuzzy-match t
	  :action '(("Open" . (lambda (x)
				(find-file (plist-get x :filename))
				(goto-char (plist-get x :begin))
				(org-show-entry)))))))


;; * End
(provide 'org-db)

;;; org-db.el ends here
