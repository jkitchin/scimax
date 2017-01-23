;;; mongo.el --- An emacs-lisp interface to MongDB

;;; Commentary:
;; Adapted from  https://www.masteringemacs.org/article/comint-writing-command-interpreter

;; * Customizable variables
(defcustom mongo-program "/usr/local/bin/mongo"
  "Path to the program used by `mongo'")

(defcustom mongo-program-arguments '("--host=127.0.0.1" "--port=27017")
  "Commandline arguments to pass to `mongo'")


;; * mongo-mode
(defvar mongo-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    (define-key map (kbd "<up>") 'comint-previous-input)
    (define-key map (kbd "<down>") 'comint-next-input)
    map)
  "Basic mode map for `mongo'")

(defvar mongo-prompt-regexp "^\\(^> \\)"
  "Prompt for `mongo'.")

(defun mongo--initialize ()
  "Helper function to initialize Mongo"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode mongo-mode comint-mode "Mongo"
  "Major mode for `mongo'.

\\<mongo-mode-map>"
  nil "Mongo"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp mongo-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(mongo-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) mongo-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'mongo-mode-hook 'mongo--initialize)

;; * Font-lock 
(set (make-local-variable 'font-lock-defaults) '(mongo-font-lock-keywords t))

;; https://docs.mongodb.com/manual/crud/
(defconst mongo-keywords
  '("db" "use" "show"
    "insert" "insertOne" "insertMany" "save"
    "find" "limit" "sort"
    "update" "updateOne" "updateMany" "replaceOne"
    "remove" "deleteOne" "deleteMany"
    "aggregate"
    "bulkWrite"
    "drop" "dropDatabse"
    "ensureIndex"
    "_id" "ObjectId"
    "$lt" "$gt" "$text" "$search" "$regex" "$options"))

(defvar mongo-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt mongo-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `mongo-mode'.")


;; * mongo command

;; (defun mongo (&optional bury)
;;   "Run an inferior instance of `mongo' inside Emacs."
;;   (interactive "P")
;;   (let* ((mongo-program mongo-cli-file-path)
;;          (buffer (comint-check-proc "Mongo")))
;;     ;; pop to the "*Mongo*" buffer if the process is dead, the
;;     ;; buffer is missing or it's got the wrong mode.
;;     (pop-to-buffer-same-window
;;      (if (or buffer (not (derived-mode-p 'mongo-mode))
;;              (comint-check-proc (current-buffer)))
;;          (get-buffer-create (or buffer "*Mongo*"))
;;        (current-buffer)))
;;     ;; create the comint process if there is no buffer.
;;     (unless buffer
;;       (apply 'make-comint-in-buffer "Mongo" buffer
;;              mongo-program mongo-cli-arguments)
;;       (mongo-mode))))

(defun mongo (&optional bury)
  "Run an inferior instance of `mongo' inside Emacs."
  (interactive "P")
  (let* ((proc (comint-check-proc "*Mongo*")))
    (cond
     ((null proc)
      ;; start it up
      (with-current-buffer (get-buffer-create "*Mongo*")
	(apply 'make-comint-in-buffer "Mongo" (get-buffer-create "*Mongo*") 
	       mongo-program mongo-program-arguments)
	(mongo-mode)))
     
     (t
      (message "Mongo seems to be running already."))))
  (unless bury
    (pop-to-buffer-same-window "*Mongo*")))


(defun mongo-close ()
  "Close the connection to mongo."
  (when (get-buffer "*Mongo*")
    (let ((kill-buffer-query-functions '()))
      (kill-buffer "*Mongo*"))))


;; * Programmatic use of mongo with comint
(defun mongo-connect ()
  "Make a mongo connection."
  (mongo)
  (bury-buffer "*Mongo*"))


(defvar mongo-output '()
  "A variable for storing the output of a command.
This will normally be empty, or a list of strings while
collecting output, or a string when the output is done
collecting.")


(defun mongo-output-filter (output)
  "Hook function for handling process output."
  (message "Handling: %s" output)
  (with-current-buffer "*Mongo*"
    (goto-char (point-max))
    (forward-line 0)
    (setq mongo-output (append mongo-output (list output)))
    (when (looking-at mongo-prompt-regexp) 
      (setq mongo-output
	    (replace-regexp-in-string
	     mongo-prompt-regexp ""
	     (mapconcat 'identity mongo-output ""))))
    ;; corner case for when a single chunk is returned and it containes the last line..
    ;; (when (s-ends-with? "\n>" mongo-output)
    ;;   (setq mongo-output (substring mongo-output 0 -2)))
    ))


(add-to-list 'comint-output-filter-functions 'mongo-output-filter)


(defun mongo-cmd (cmd &optional read-json)
  "Send CMD to `mongo' and return the output.
CMD should be a string, as if you had typed it into the mongo
shell. For short commands, they are sent directly to mongo, so
you can use interactive commands there. For long commands they
are written to a javascript file which is then loaded by mongo."
  (setq mongo-output '())
  (if (> (length cmd) 255)
      (let ((tf (make-temp-file "mongo-" nil ".js")))
	(with-temp-file tf
	  (insert cmd))
	(comint-simple-send (get-buffer-process "*Mongo*") (format "load(\"%s\")" tf)))
    (comint-simple-send (get-buffer-process "*Mongo*") cmd))
  (while (listp mongo-output)
    (sleep-for 0.05))
  (prog1
      mongo-output
    (setq mongo-output '())))

;; ** CRUD operations

;; *** Inserting entries

(defun mongo-insert (collection document)
  "Insert into COLLECTION the DOCUMENT.
COLLECTION is a string and must be in the current database.
DOCUMENT is a lisp data structure that will be json-encoded."
  (let* ((json (json-encode document))
	 (cmd (format "var result = db.%s.insert(%s); printjsononeline(result);"
		      collection
		      json))
	 (output (mongo-cmd cmd)))
    (json-read-from-string output)))

;; *** Finding documents

(defun mongo--unquote-query (query)
  "Json encodes QUERY, and unquotes any ObjectId calls.

We don't have syntax for the ObjectId call that mongo wants in
 lisp, so a query has to look like this:
'((_id .  \"ObjectId(\"587babfaef131d0d4603b3ad\")\"))

Mongo can't have the quotes around the call, so this function
removes them.
"
  (replace-regexp-in-string "\"\\(ObjectID(\\\\\"\\(.*?\\)\\\\\")\\)\""
			    "ObjectId(\"\\2\")"
			    (json-encode query)))

(defun mongo--requote-output (output)
  "Adds quotes around ObjectId in OUTPUT.
When mongo outputs json, it has unquoted ObjectIds in it that
emacs cannot interpret as json. This function adds quotes around
ObjectID() so that we can read it as json."
  (replace-regexp-in-string
   "ObjectId(\"\\(.*?\\)\")"
   "\"ObjectId(\\\\\"\\1\\\\\")\""
   output))

(defun mongo-find (collection query &optional projection limit sort)
  "Find documents in COLLECTION that match QUERY,
If PROJECTION is non-nil return those fields instead of the whole
documents. If LIMIT is non-nil use it to limit the number of responses. If SORT is non-nil, use it to sort the documents by they kes"
  (let* ((query-json (mongo--unquote-query query))
	 (projection-json
	  (and projection (json-encode projection)))
	 (cmd (format "var results = db.%s.find(%s)%s.toArray(); printjsononeline(results);"
		      collection
		      (if projection
			  (format "%s, %s" query-json projection-json)
			query-json)
		      (if limit
			  (format ".limit(%s)" limit)
			"")))
	 (output))
    (message cmd)
    (setq output (mongo--requote-output 
		  (mongo-cmd cmd))) 
    (json-read-from-string output)))

;; ** Updating an entry

(defun mongo-update (collection query document)
  "In COLLECTION update documents matching QUERY with DOCUMENT."
  (let* ((query-json (json-encode query))
	 (document-json (json-encode document))
	 (cmd (format "var result = db.%s.update(%s, %s); printjsononeline(result);" 
		      collection
		      query-json document-json))
	 (output (mongo-cmd cmd)))
    (json-read-from-string output)))

;; ** Deleting
(defun mongo-deleteMany (collection filter)
  "Delete records in COLLECTION matched by FILTER.
TODO: add write concern."
  (let* ((filter-json (mongo--unquote-query filter)) 
	 (cmd (format "var result = db.%s.deleteMany(%s); printjsononeline(result)"
		      collection
		      filter-json))
	 (output (mongo-cmd cmd)))
    (json-read-from-string output)))

(defun mongo-remove (collection filter &optional justOne)
  "From COLLECTION remove documents matching FILTER.
If justOne is non-nil, use it in the command. It should usually be set to 1."
  (let* ((filter-json (mongo--unquote-query filter)) 
	 (cmd (format "var result = db.%s.remove(%s%s); printjsononeline(result)"
		      collection
		      (if justOne
			  (format ", %s" justOne)
			"")
		      filter-json))
	 (output (mongo-cmd cmd)))
    (json-read-from-string output)))

;; ** Miscellaneous commands

(defun mongo-count (collection &optional query options)
  "Return the number of documents in COLLECTION using OPTIONS."
  (mongo-cmd (format "var result = db.%s.count(%s%s); printjsononeline(result);"
		     collection
		     (if query
			 (mongo--unquote-query query)
		       "null")
		     (if options
			 (format ", %s" (json-encode options))
		       ""))))

(defun mongo-drop (collection)
  "Drop COLLECTION."
  (mongo-cmd (format "var result = db.%s.drop(); printjsononeline(result);" collection)))

(defun mongo-dropDatabase ()
  "Drop the selected database."
  (mongo-cmd (format "var result = db.dropDatabase(); printjsononeline(result);")))

(defun mongo-use (database)
  "Switch to DATABASE."
  (mongo-cmd (format "use %s" database)))

(defun mongo-ensureIndex (collection fields)
  "Create an index for fields.
FIELDS should be an alist of (key . int). 1 for ascending, -1 for
descending."
  (mongo-cmd (format "var result = db.%s.ensureIndex(%s);  printjsononeline(result);"
		     (json-encode fields))))

(defun mongo-getIndexes (collection)
  (mongo-cmd "var result = db.%s.getIndexes(); printjsononeline(result);" collection))

(defun mongo-dropIndex (collection index)
  (mongo-cmd "var result = db.%s.dropIndexe(\"%s\"); printjsononeline(result);"
	     collection
	     index))

(defun mongo-createCollection (name &optional options)
  "Create the collection NAME with OPTIONS."
  (let ((cmd (format "var result = db.createCollection(%s%s); printjsononeline(result);"
		     name
		     (if options
			 (format ", %s" (json-encode options))
		       ""))))
    (mongo-cmd cmd)))

;; * Utilities


(defun mongo-help (query)
  "Search the online manual for QUERY."
  (interactive "sQuery: ")
  (browse-url (concat
	       "https://docs.mongodb.com/manual/"
	       (format "search/?query=%s" (url-hexify-string query)))))

;; * The end
(provide 'mongo)

;;; mongo.el ends here
