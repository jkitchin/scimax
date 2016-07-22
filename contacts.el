;;; contacts.el --- Contact manager for org-mode


;;; Commentary:
;; This is a replacement for org-contacts. I find it is too slow for large
;; numbers of contacts across many files. This library uses a cache system to
;; speed up loading, and persistent files that are only updated when a change is
;; detected. There is one main function: `ivy-contacts'.

;; A Contact is any org-heading with an EMAIL property. The contact database is
;; a list of (display-string property-alist). 

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
  "List of contacts. (display-string property-a-list)")

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
			    (format "|%s|%s|%s|%35s | %40s | %s"
				    (if (org-entry-get (point) "PHONE") "P" " ")
				    (if (org-entry-get (point) "URL") "W" " ")
				    (if (org-entry-get (point) "SCOPUSID") "S" " ")
				    (org-no-properties
				     (org-get-heading t t))
				    (org-entry-get (point) "EMAIL")
				    (let ((tags (org-get-tags-at)))
				      (if tags
					  (concat ":" (s-join ":" tags) ":")
					""))))
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
			"EMAIL<>\"\"")))
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
			(print contacts-cache-data (current-buffer))))
		    results)))))


;; * Tag filter
(defun contacts-match-tag-expression-p (tag-expression contact)
  "Return if the CONTACT matches the TAG-EXPRESSION."
  (let* ((lexical-binding nil)
	 (todo-only nil)
	 (tags-list (let ((tags (cdr (assoc "ALLTAGS" contact))))
		      (when tags
			(split-string (substring tags 1 -1) ":")))))
    (eval (cdr (org-make-tags-matcher tag-expression)))))

(defun contacts-filter (tag-expression)
  "Return list of contacts matching TAG-EXPRESSION"
  (loop for contact in contacts
	if (contacts-match-tag-expression-p tag-expression contact)
	collect contact))

(defun contacts-search ()
  "Search for word at point or selection in org-contacts.
e.g. on a person name, email, etc..."
  (interactive)
  (let ((word (if (region-active-p)
		  (buffer-substring (region-beginning) (region-end))
		(thing-at-point 'word))))

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

(defvar ivy-marked-candidates '()
  "Holds marked candidates")

(defun ivy-mark-candidate () 
  "Add current candidate to `ivy-marked-candidates'.
If candidate is already in, remove it."
  (interactive) 
  (let ((cand (or (assoc ivy--current (ivy-state-collection ivy-last))
		  ivy--current)))
    (if (-contains? ivy-marked-candidates cand)
	;; remove it from the marked list
	(setq ivy-marked-candidates
	      (-remove-item cand ivy-marked-candidates))
      
      ;; add to list
      (setq ivy-marked-candidates
	    (append ivy-marked-candidates (list cand))))) 
  (ivy-next-line))


(defun ivy-show-marked-candidates ()
  "Show marked candidates."
  (interactive)
  (when ivy-marked-candidates
    (setf (ivy-state-collection ivy-last) ivy-marked-candidates)
    (setf (ivy-state-preselect ivy-last) ivy--current)
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
  (unless (and (featurep 'mu4e) (boundp 'mu4e~contacts))
    (mu4e t))
  
  (append contacts
	  (loop for contact being the hash-key of mu4e~contacts
		collect (list contact (cons "EMAIL" contact)))))

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
  "Make entry red if it is marked."
  (if (-contains?
       (if (listp (car ivy-marked-candidates))
	   (mapcar 'car ivy-marked-candidates)
	 ;; we have a list of strings
	 ivy-marked-candidates)
       s)
      (propertize s 'face 'font-lock-warning-face)
    (propertize s 'face s)))

(ivy-set-display-transformer
 'ivy-contacts
 'ivy-marked-transformer)

;;;###autoload
(defun ivy-contacts (arg)
  "Select contacts using ivy."
  (interactive "P")
  (setq ivy-marked-candidates '())
  (ivy-read "Contact: " (ivy-contacts-candidates)
	    :keymap ivy-contacts-keymap
	    :caller 'ivy-contacts
	    :action '(1
		      ("i" (lambda (contact)
			     (with-ivy-window
			       ;; Make sure we are at the end of a word or line
			       (unless (or (eolp)
					   (looking-at "\\>"))
				 (re-search-forward "\\>"))
			       ;; put in a comma unless we are looking back at a
			       ;; space or comma
			       (when (not (looking-back " \\|,")) (insert ","))
			       (if ivy-marked-candidates
				   (insert (mapconcat (lambda (contact)
							(if (listp contact)
							    (cdr (assoc "EMAIL" contact))
							  contact))
						      ivy-marked-candidates
						      ","))
				 (insert (if (listp contact)
					     (cdr (assoc "EMAIL" contact))
					   contact)))))
		       "Insert email")
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
		      
		      ("q" nil "Quit"))))

(define-key mu4e-compose-mode-map "\C-c]" 'ivy-contacts)
(define-key message-mode-map "\C-c]" 'ivy-contacts)

(defalias 'ic 'ivy-contacts)

(provide 'contacts)

;;; contacts.el ends here

