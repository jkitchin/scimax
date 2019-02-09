;;; contacts.el --- Contact manager for org-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a replacement for org-contacts. I find it is too slow for large
;; numbers of contacts across many files. This library uses a cache system to
;; speed up loading, and persistent files that are only updated when a change is
;; detected.

;; There is an ivy function: `ivy-contacts'.
;; And a `helm-contacts' function.

;; A Contact is any org-heading with an EMAIL property. The contact database is
;; a list of (display-string property-alist). The display string is what is
;; shown in helm or ivy for selection. The property-alist contains all the
;; properties of the contact headline, as well as the FILE and POSITION of the
;; contact.

;;; Code:

(defvar contacts-files
  '()
  "A list of org-files to retrieve contacts from.")


(defvar contacts-cache-file "~/.contacts-cache"
  "File to store cached contacts in.")


(defvar contacts-cache-data
  nil
  "Cache data as an a-list.
'hashes is a list of cons cells (contact-file . hash)
'contacts are a list of cons cells (contact-file . contacts)
These are stored persistently in `contacts-cache-file'.")


(defvar contacts '()
  "List of contacts. (display-string property-a-list).")


;; * load and clear cache functions

(defun contacts-load-cache-file ()
  "Load the cache file to set `contacts-cache-data'."
  (when (file-exists-p contacts-cache-file)
    (with-temp-buffer
      (insert-file-contents contacts-cache-file)
      (setq contacts-cache-data (read (current-buffer))))))


;;;###autoload
(defun contacts-clear-cache ()
  "Reset `contacts-cache-data' and delete cache file."
  (interactive)
  (setq contacts '()
	contacts-cache-data '((hashes . nil)
			      (contacts . nil)))
  (when (file-exists-p contacts-cache-file)
    (delete-file contacts-cache-file)))


(defun contacts-known-hash (fname)
  "Return the known hash for FNAME."
  (cdr (assoc fname (cdr (assoc 'hashes contacts-cache-data)))))


(defun contacts-known-contacts (fname)
  "Return the known hash for FNAME."
  (cdr (assoc fname (cdr (assoc 'contacts contacts-cache-data)))))


(defun contacts-update-cache ()
  "Update cache for each file in `contacts-files' if needed."
  (when (null contacts-cache-data)
    (contacts-load-cache-file))
  (setq contacts
	(cl-loop
	 for contacts-file in contacts-files
	 if (file-exists-p contacts-file)
	 append (with-temp-buffer
		  (insert-file-contents contacts-file)
		  ;; return the known results if hash matches
		  (let* ((hash (secure-hash 'sha256 (current-buffer)))
			 (results))
		    (setq
		     results
		     (if (string=
			  hash
			  (or (contacts-known-hash contacts-file) ""))
			 (contacts-known-contacts contacts-file)
		       ;; we need to get new results
		       (org-mode)
		       (org-map-entries
			(lambda ()
			  (append
			   (list
			    ;; display string
			    (format "|%s|%s|%s|%45s | %40s | %30s | %s"
				    (if (org-entry-get (point) "PHONE") "P" " ")
				    (if (org-entry-get (point) "URL") "W" " ")
				    (if (org-entry-get (point) "SCOPUSID") "S" " ")
				    (format "%s%s"
					    (org-get-heading t t)
					    (if (org-entry-get (point) "ALIASES")
						(format " (%s)" (org-entry-get (point) "ALIASES"))
					      ""))
				    (org-entry-get (point) "EMAIL")
				    (let ((tags (org-get-tags-at)))
				      (if tags
					  (concat ":" (s-join ":" tags) ":")
					""))
				    contacts-file))
			   ;; since we use a temp buffer we have
			   ;; to set the file here.
			   (let ((properties (org-entry-properties)))
			     (setf (cdr (assoc "FILE" properties))
				   contacts-file)
			     properties)

			   (list
			    (cons "NAME" (org-no-properties
					  (org-get-heading t t)))
			    (cons "POSITION" (point)))))
			"EMAIL<>\"\"-ARCHIVE")))
		    (when (not (string=
				hash
				(or (contacts-known-hash contacts-file) "")))
		      (message "Updating contacts cache for %s" contacts-file)
		      (let ((place (cdr (assoc contacts-file
					       (cdr (assoc 'hashes contacts-cache-data))))))
			(if place
			    (setf (cdr (assoc contacts-file
					      (cdr (assoc 'hashes contacts-cache-data))))
				  hash)
			  (cl-pushnew (cons contacts-file hash)
				      (cdr (assoc 'hashes contacts-cache-data)))))
		      (let ((place (cdr (assoc contacts-file
					       (cdr (assoc 'contacts contacts-cache-data))))))
			(if place
			    (setf (cdr (assoc contacts-file
					      (cdr (assoc 'contacts contacts-cache-data))))
				  results)
			  (cl-pushnew (cons contacts-file results)
				      (cdr (assoc 'contacts contacts-cache-data)))))
		      (with-temp-file contacts-cache-file
			(let ((print-length nil))
			  (print contacts-cache-data (current-buffer)))))
		    results)))))


;; * Tag filter

(defun contacts-match-tag-expression-p (tag-expression contact)
  "Return if the TAG-EXPRESSION matches the CONTACT."
  (let* ((lexical-binding t)
	 (todo-only nil)
	 (tags-list (let ((tags (cdr (assoc "ALLTAGS" contact))))
		      (when tags
			(split-string (substring tags 1 -1) ":")))))
    (funcall (cdr (org-make-tags-matcher tag-expression)) todo-only tags-list nil)))


(defun contacts-filter (tag-expression)
  "Return list of contacts matching TAG-EXPRESSION."
  (loop for contact in contacts

	if (with-current-buffer (find-file-noselect (cdr (assoc "FILE" contact)))
	     (goto-char (cdr (assoc "POSITION" contact)))
	     (let* ((lexical-binding t)
		    (todo-only nil)
		    (tags-list (let ((tags (cdr (assoc "ALLTAGS" contact))))
				 (when tags
				   (split-string (substring tags 1 -1) ":")))))
	       (funcall (cdr (org-make-tags-matcher tag-expression)) todo-only tags-list nil)))
	collect contact))


(defun contacts-search ()
  "Search for word at point or selection in `contacts-files'.
e.g. on a person name, email, etc..."
  (interactive)
  (let ((word (if (region-active-p)
		  (buffer-substring (region-beginning) (region-end))
		(or (thing-at-point 'word)
		    (read-string "Search for: ")))))

    (multi-occur
     (mapcar 'find-file-noselect contacts-files)
     word)
    (switch-to-buffer-other-window (or (get-buffer "*Occur*") (current-buffer)))))


(defun contacts-property (property contact)
  "Get PROPERTY from CONTACT."
  (cdr (assoc property (cdr contact))))


(defun contacts-map-property (property contacts)
  "Get PROPERTY for each contact in CONTACTS."
  (mapcar (lambda (c) (contacts-property property c)) contacts))



;; * Ivy-contacts

(defvar contacts-marked-candidates '()
  "Holds marked candidates.")


(defun ivy-mark-candidate ()
  "Add current candidate to `contacts-marked-candidates'.
If candidate is already in, remove it."
  (interactive)
  (let ((cand (or (assoc (org-ref-ivy-current) (ivy-state-collection ivy-last))
		  (org-ref-ivy-current))))
    (if (-contains? contacts-marked-candidates cand)
        ;; remove it from the marked list
        (setq contacts-marked-candidates
              (-remove-item cand contacts-marked-candidates))

      ;; add to list
      (setq contacts-marked-candidates
            (append contacts-marked-candidates (list cand)))))

  ;; move to the next line
  (ivy-next-line))


(defun ivy-show-marked-candidates ()
  "Show marked candidates."
  (interactive)
  (when contacts-marked-candidates
    (setf (ivy-state-collection ivy-last) contacts-marked-candidates)
    (ivy--reset-state ivy-last)))


(defun ivy-contacts-show-all ()
  "Show all the candidates."
  (interactive)
  (setf (ivy-state-collection ivy-last)
	(ivy-contacts-candidates))
  (ivy--reset-state ivy-last))


(defun ivy-contacts-candidates ()
  "Return list of contacts to choose from.
Loads cache file."
  (unless contacts-cache-data
    (contacts-load-cache-file))

  (contacts-update-cache)

  ;; Add mu4e contacts if we have them
  (when (and (featurep 'mu4e) (not (boundp 'mu4e~contacts)))
    (mu4e t))

  (if (featurep 'mu4e)
      (append contacts
	      (loop for entry in
		    (split-string (shell-command-to-string "mu cfind --format=mutt-ab") "\n" t)
		    collect
		    (let ((tup (split-string  entry "\t")))
		      (list
		       (format "\"%s\" <%s>" (nth 1 tup) (nth 0 tup))
		       (cons "EMAIL" (nth 0 tup))))))
    contacts))


(defvar ivy-contacts-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-<SPC>") 'ivy-mark-candidate)
    (define-key map (kbd "C-,") 'ivy-show-marked-candidates)
    (define-key map (kbd "C-.") 'ivy-contacts-show-all)
    (define-key map (kbd "C-<return>")
      (lambda ()
        "Apply action and move to next/previous candidate."
        (interactive)
        (ivy-call)
        (ivy-next-line)))
    map))


(defun ivy-marked-transformer (s)
  "Make S entry red if it is marked."
  (if (-contains?
       (if (listp (car contacts-marked-candidates))
	   (mapcar 'car contacts-marked-candidates)
	 ;; we have a list of strings
	 contacts-marked-candidates)
       s)
      (propertize s 'face 'font-lock-warning-face)
    (propertize s 'face nil)))


(ivy-set-display-transformer
 'ivy-contacts
 'ivy-marked-transformer)


(defun ivy-contact-insert (contact)
  (with-ivy-window
    ;; Make sure we are at the end of a word or line
    (unless (or (eolp)
		(looking-at "\\>"))
      (re-search-forward "\\>"))
    ;; put in a comma unless we are looking back at a
    ;; space or comma
    (when (not (looking-back " \\|,")) (insert ","))
    (if contacts-marked-candidates
	(insert (mapconcat (lambda (contact)
			     (if (listp contact)
				 (cdr (assoc "EMAIL" contact))
			       contact))
			   contacts-marked-candidates
			   ","))
      (insert (if (listp contact)
		  (cdr (assoc "EMAIL" contact))
		contact)))))
;;;###autoload
(defun ivy-contacts ()
  "Select contacts using ivy."
  (interactive)
  (setq contacts-marked-candidates '())
  (ivy-read "Contact: " (ivy-contacts-candidates)
	    :keymap ivy-contacts-keymap
	    :caller 'ivy-contacts
	    :action '(1
		      ("i" ivy-contact-insert
		       "Insert email")
		      ("x" (lambda (contact)
			     (with-ivy-window
			       (insert (cdr (assoc "EMAIL" contact))))))
		      ("n" (lambda (contact)
			     (with-ivy-window
			       (insert
				(format "\"%s\" <%s>"
					(cdr (assoc "NAME" contact))
					(cdr (assoc "EMAIL" contact))))))
		       "Insert \"name\" <email>")
		      ("c" (lambda (contact)
			     (kill-new (format "\"%s\" <%s>"
					       (cdr (assoc "NAME" contact))
					       (cdr (assoc "EMAIL" contact)))))
		       "Copy \"name\" <email> to clipboard")
		      ("o" (lambda (contact)
			     (find-file (cdr (assoc "FILE" contact)))
			     (goto-char (cdr (assoc "POSITION" contact)))
			     (outline-show-entry))
		       "Open contact")
		      ("m" (lambda (contact)
			     "Send imessage if there is cellphone number."
			     (when-let ((cellphone-number (cdr (assoc "CELLPHONE" contact))))
			       (setq cellphone-number (replace-regexp-in-string "-\\|(\\|)" "" cellphone-number))
			       (do-applescript
				(format
				 "tell application \"Messages\"
	set targetService to 1st service whose service type = iMessage
	set targetBuddy to buddy \"%s\" of targetService
	send \"%s\" to targetBuddy
end tell"
				 cellphone-number
				 (read-input "SMS: "))))
			     ))
		      ("e" (lambda (contact)
			     (compose-mail)
			     (message-goto-to)
			     (insert (cdr (assoc "EMAIL" contact)))
			     (message-goto-subject))
		       "Email contact")
		      ("t" (lambda (contact)
			     (org-open-link-from-string
			      (format "[[mu4e:query:to:%s]]"
				      (cdr (assoc "EMAIL" contact)))))
		       "Emails to contact")
		      ("f" (lambda (contact)
			     (org-open-link-from-string
			      (format "[[mu4e:query:from:%s]]"
				      (cdr (assoc "EMAIL" contact)))))
		       "Emails from contact")

		      ("w" (lambda (contact)
			     (let ((url (cdr (assoc "URL" contact))))
			       (if url
				   (browse-url url)
				 (message "No URL found for %s." (cdr (assoc "NAME" contact))))))
		       "Open url")
		      ("s" (lambda (contact)
			     (let ((scopusid (cdr (assoc "SCOPUSID" contact))))
			       (if scopusid
				   (browse-url
				    (format
				     "http://www.scopus.com/authid/detail.url?origin=AuthorProfile&authorId=%s"
				     scopusid))
				 (let* ((name (cdr (assoc "NAME" contact)))
					(fields (s-split " " name))
					;; This is a really naive split of name
					(firstname (car fields))
					(lastname (car (last fields))))
				   (browse-url
				    (format
				     "https://www.scopus.com/results/authorNamesList.uri?origin=searchauthorlookup&src=al&&st1=%s&st2=%s"
				     lastname (substring firstname 0 1)))))))
		       "Open in scopus")
		      ("p" (lambda (contact)
			     (do-applescript
			      (format "tell application \"Cisco Jabber\"
	activate
	tell application \"System Events\" to keystroke \"n\" using {shift down, command down}
	tell application \"System Events\" to keystroke \"%s\"
	tell application \"System Events\" to key code 36 #return
end tell" (cdr (assoc "PHONE" contact)))))
		       "Call")
		      ("g" (lambda (contact)
			     "Add tag to contact."
			     (let ((tags (split-string
					  (read-string "Tag (space separated): ")
					  " " t)))
			       (if contacts-marked-candidates
				   (loop for contact in contacts-marked-candidates
					 do
					 (message "%s" contact)
					 ;; this edits the buffer, so points can get messed up.
					 (save-window-excursion
					   (find-file (cdr (assoc "FILE" contact)))
					   (goto-char (point-min))
					   (re-search-forward (regexp-quote
							       (cdr (assoc "ITEM" contact))))
					   (org-set-tags-to
					    (-uniq (append (org-get-tags-at) tags))))
					 (save-buffer))

				 (save-window-excursion
				   (find-file (cdr (assoc "FILE" contact)))
				   (goto-char (cdr (assoc "POSITION" contact)))
				   (org-set-tags-to (-uniq (append (org-get-tags-at) tags))))
				 (save-buffer))))
		       "Add tag to contact")
		      ("l" (lambda (contact)
			     (insert (format "[[%s][%s]]"
					     (concat "contact:"
						     (cdr (assoc "NAME" contact)))
					     (cdr (assoc "NAME" contact)))))
		       "Insert link")

		      ("q" nil "Quit"))))

(when (featurep 'mu4e)
  (define-key mu4e-compose-mode-map "\C-c]" 'ivy-contacts))

(define-key message-mode-map "\C-c]" 'ivy-contacts)

(defalias 'ic 'ivy-contacts)



;; * contact link

(defhydra contact (:color blue :hint nil)
  "contact:"
  ("o" (lambda ()
	 (interactive)
	 (org-id-goto (org-element-property
		       :path (org-element-context))))
   "Open contact")
  ("l" (lambda ()
	 (interactive)
	 (kill-ring-save
	  (buffer-substring (org-element-property
			     :begin (org-element-context))
			    (org-element-property
			     :end (org-element-context)))))
   "Copy link"))


(defun contact-store-link ()
  "Store a contact link."
  (when (org-entry-get (point) "EMAIL")
    (push (list (format "contact:%s" (org-id-get-create))
		(or (org-entry-get (point) "NAME")
		    (nth 4 (org-heading-components))))
	  org-stored-links)))


(org-link-set-parameters
 "contact"
 :follow (lambda (path) (contact/body))
 :complete (lambda (&optional arg)
	     (format "contact:%s" (org-link-unescape
				   (completing-read
				    "Name: "
				    (mapcar (lambda (contact)
					      (cdr (assoc "NAME" (cdr contact))))
					    contacts)))))
 :face '(:foreground "OrangeRed1")
 :help-echo "An org-contact."
 ;; (lambda (window object position)
 ;;   (save-excursion
 ;; 	(goto-char position)
 ;; 	(let ((id (org-element-property
 ;; 		   :path (org-element-context))))
 ;; 	  (save-window-excursion
 ;; 	    (org-id-goto id)
 ;; 	    (save-restriction
 ;; 	      (org-narrow-to-subtree)
 ;; 	      (buffer-string))))))
 :store #'contact-store-link)


;; * contact agenda

(defun contact-agenda ()
  "Open `org-agenda' using `contacts-files' in TAG/PROP/TODO query mode."
  (interactive)
  (let ((org-agenda-files contacts-files))
    (org-agenda nil "m")))


;; * helm contacts
;; This is slow.

(defun helm-contacts-candidates ()
  "Return list of strings for the helm source."
  (mapcar 'car (ivy-contacts-candidates)))


(defun helm-contacts-get-contact (candidate)
  "Given a helm CANDIDATE, return the corresponding contact."
  (cdr (assoc candidate (ivy-contacts-candidates))))


(defun contact-insert-email (_)
  "Insert email addresses in `helm-marked-candidates."
  (let ((emails (cl-loop for cand in (helm-marked-candidates)
			 with contact = (helm-contacts-get-contact cand)
			 do
			 (setq contact (helm-contacts-get-contact cand))
			 collect (if (listp contact)
				     (cdr (assoc "EMAIL" contact))
				   contact))))
    ;; Make sure we are at the end of a word or line
    (unless (or (eolp)
		(looking-at "\\>"))
      (re-search-forward "\\>"))
    ;; put in a comma unless we are looking back at a
    ;; space or comma
    (when (not (looking-back " \\|," 1)) (insert ","))
    (insert (mapconcat 'identity emails ","))))


(defun contact-insert-name-email (_)
  "Insert \"name\" <email> for selectected contacts, separated by ;."
  (insert
   (mapconcat 'identity (cl-loop for candidate in (helm-marked-candidates)
				 with contact
				 do
				 (setq contact (helm-contacts-get-contact candidate))
				 collect
				 (format "\"%s\" <%s>"
					 (cdr (assoc "NAME" contact))
					 (cdr (assoc "EMAIL" contact))))
	      "; ")))


(defun contact-copy-name-email (_)
  "Copy \"name\" <email> for selectected contacts, separated by ;."
  (kill-new
   (mapconcat 'identity (loop for candidate in (helm-marked-candidates)
			      with contact = nil
			      do
			      (setq contact (helm-contacts-get-contact candidate))
                              collect
                              (if helm-current-prefix-arg
                                  (cdr (assoc "EMAIL" contact))
                                (format "\"%s\" <%s>"
                                        (cdr (assoc "NAME" contact))
                                        (cdr (assoc "EMAIL" contact)))))
              "; ")))



(defvar helm-contacts-source
  (helm-build-sync-source "contacts"
    :candidates 'helm-contacts-candidates
    :fuzzy-match t
    :action '(("Insert email(s)" . (lambda (candidate)
				     (contact-insert-email (helm-contacts-get-contact candidate))))
	      ("Open contact" . (lambda (candidate)
				  (let ((contact (helm-contacts-get-contact candidate)))
				    (find-file (cdr (assoc "FILE" contact)))
				    (goto-char (cdr (assoc "POSITION" contact)))
				    (outline-show-entry))))
	      ("Insert \"name\" <email>" . contact-insert-name-email)
	      ("Copy \"name\" <email>" . contact-copy-name-email)
	      ("Send email" . (lambda (candidate)
				(let ((contact (helm-contacts-get-contact candidate)))
				  (compose-mail)
				  (message-goto-to)
				  (insert
				   (mapconcat
				    'identity
				    (cl-loop for candidate in (helm-marked-candidates)
					     with contact
					     do
					     (setq contact (helm-contacts-get-contact candidate))
					     collect
					     (cdr (assoc "EMAIL" contact)))
				    ","))
				  (message-goto-subject))))

	      ("Find emails to contact" . (lambda (candidate)
					    (let ((contact (helm-contacts-get-contact candidate)))
					      (org-open-link-from-string
					       (format "[[mu4e:query:to:%s]]"
						       (cdr (assoc "EMAIL" contact)))))))
	      ("Find emails from contact" . (lambda (candidate)
					      (let ((contact (helm-contacts-get-contact candidate)))
						(org-open-link-from-string
						 (format "[[mu4e:query:from:%s]]"
							 (cdr (assoc "EMAIL" contact)))))))
	      ("Call" . (lambda (candidate)
			  (let ((contact (helm-contacts-get-contact candidate)))
			    (do-applescript
			     (format "tell application \"Cisco Jabber\"
	activate
	tell application \"System Events\" to keystroke \"n\" using {shift down, command down}
	tell application \"System Events\" to keystroke \"%s\"
	tell application \"System Events\" to key code 36 #return
end tell" (cdr (assoc "PHONE" contact)))))))
	      ("Open URL" . (lambda (candidate)
			      (let* ((contact (helm-contacts-get-contact candidate))
				     (url (cdr (assoc "URL" contact))))
				(if url
				    (browse-url url)
				  (message "No URL found for %s." (cdr (assoc "NAME" contact)))))))
	      ("Insert link" . (lambda (candidate)
				 "Insert an org link for CONTACT or helm-marked-candidates."
				 (let ((contact (helm-contacts-get-contact candidate)))
				   (insert (mapconcat
					    'identity
					    (cl-loop for candidate in (helm-marked-candidates)
						     with contact = nil
						     do
						     (setq contact (helm-contacts-get-contact candidate))
						     collect
						     (format
						      "[[contact:%s][%s]]"
						      (or (cdr (assoc "ID" contact))
							  (with-current-buffer (find-file-noselect (cdr (assoc "FILE" contact)))
							    (save-excursion
							      (goto-char (cdr (assoc "POSITION" contact)))
							      (prog1
								  (org-id-get-create)
								(save-buffer)
								(contacts-update-cache)))))
						      (cdr (assoc "NAME" contact))))
					    ", ")))))
	      ("Add tag(s)" . (lambda (candidate)
				(let* ((contact (helm-contacts-get-contact candidate))
				       (tags (helm :sources `((name . "Tags")
							      (candidates . ,(org-global-tags-completion-table contacts-files))
							      (action . (lambda (tag)
									  (helm-marked-candidates)))))))
				  (cl-loop for candidate in (helm-marked-candidates)
					   with contact = nil
					   do
					   (save-window-excursion
					     (setq contact (helm-contacts-get-contact candidate))
					     (find-file (cdr (assoc "FILE" contact)))
					     (goto-char (cdr (assoc "POSITION" contact)))
					     (org-set-tags-to
					      (-uniq (append (org-get-tags-at) tags)))
					     (save-buffer)
					     (contacts-update-cache))))))
	      ("Find contact in open buffers" . (lambda (candidate)
						  (let ((contact (helm-contacts-get-contact candidate)))
						    (multi-occur
						     (buffer-list)
						     (concat
						      (or (cdr (assoc "ID" contact)) "")
						      "\\|"
						      (cdr (assoc "NAME" contact)))))))
	      ;; this needs some more work. you can use it to customize on the fly what is done to the selected
	      ;; Maybe it should just take property names instead of a sexp input. Or an s-format string.
	      ;; ("Apply and insert" . (lambda (candidate)
	      ;; 			  (let ((contact (helm-contacts-get-contact candidate)))
	      ;; 			    (eval
	      ;; 			     `(insert (mapconcat
	      ;; 				       (lambda (it)
	      ;; 					 ,(read (read-string "Body (it):")))
	      ;; 				       (helm-marked-candidates)
	      ;; 				       ,(if (> (length (helm-marked-candidates)) 1)
	      ;; 					    (read-string "Separator:" nil nil ", ")
	      ;; 					  "")))))))
	      ))
  "Helm contact source.")

(defun helm-contacts ()
  "A helm source for contacts."
  (interactive)
  (helm :sources helm-contacts-source))


;; * Speed keys

(defun org-speed-contacts (keys)
  "Find the command to run for KEYS."
  (when (or (and (bolp) (looking-at org-outline-regexp)
                 (not (null (org-entry-get (point) "EMAIL")))))
    (cdr (assoc keys org-speed-commands-contacts))))

(setq org-speed-commands-contacts
      '(("b" . (lambda ()
		 "If contact has a URL open it in a browser."
		 (when (org-entry-get (point) "URL")
		   (browse-url (org-entry-get (point) "URL")))))
	("c" . (lambda ()
		 "Copy the email address to the clipboard."
		 (message (kill-new (org-entry-get (point) "EMAIL")))))
	("e" . (lambda ()
		 "Send an email to the contact."
		 (let ((email (org-entry-get (point) "EMAIL")))
		   (compose-mail)
		   (message-goto-to)
		   (insert email)
		   (message-goto-subject))))
	("l" . (lambda ()
		 "Store and copy a link to the contact."
		 (message (kill-new (format "[[contact:%s][%s]]" (org-id-get-create) (nth 4 (org-heading-components)))))
		 (org-store-link nil)))
	("m" . (lambda ()
		 "Copy \"name <email>\""
		 (message (kill-new
			   (format "%s <%s>"
				   (nth 4 (org-heading-components))
				   (org-entry-get (point) "EMAIL"))))))
	("P" . (lambda ()
		 (when (org-entry-get (point) "PHONE")
		   (do-applescript
		    (format "tell application \"Cisco Jabber\"
	activate
	tell application \"System Events\" to keystroke \"n\" using {shift down, command down}
	tell application \"System Events\" to keystroke \"%s\"
	tell application \"System Events\" to key code 36 #return
end tell" (org-entry-get (point) "PHONE"))))))
	("?" . (lambda ()
		 "Print contacts speed key help."
		 (with-output-to-temp-buffer "*Help*"
		   (princ "Contacts Speed commands\n===========================\n")
		   (mapc #'org-print-speed-command org-speed-commands-contacts)
		   (princ "\n")
		   (princ "User-defined Speed commands\n===========================\n")
		   (mapc #'org-print-speed-command org-speed-commands-user)
		   (princ "Built-in Speed commands\n=======================\n")
		   (mapc #'org-print-speed-command org-speed-commands-default))
		 (with-current-buffer "*Help*"
		   (setq truncate-lines t))))))


(add-hook 'org-speed-command-hook 'org-speed-contacts)


;; * help

(defun contacts-help ()
  "Open the help file."
  (interactive)
  (find-file (expand-file-name "contacts.org" (file-name-directory
					       (locate-library "contacts")))))

;; * Locations
;; We can treat a headline like a location if they have an ADDRESS

(defun location-google-maps ()
  "Open the address in the current entry in google maps."
  (interactive)
  (google-maps (org-entry-get (point) "ADDRESS")))


(defun contact-store-location-link ()
  "Store a contact location link."
  (when (org-entry-get (point) "ADDRESS")
    (push (list (format "location:%s" (org-id-get-create))
		(or (org-entry-get (point) "NAME")
		    (nth 4 (org-heading-components))))
	  org-stored-links)))

(org-link-set-parameters
 "location"
 :follow (lambda (path) (contact/body))
 :face '(:foreground "BlueViolet")
 :help-echo "An org-location"
 :store #'contact-store-location-link)

;; * The end
(provide 'contacts)

;;; contacts.el ends here
