;;; scimax-contacts.el --- org-mode contacts in scimax

;;; Commentary:
;;
;; Scimax contacts library - provides a link and integration with org-db to manage contacts.
;;
;; It relies on org-db to find contacts.
;;
;; [2024-03-21 Thu] There is some overlap with org-db-contacts here. I am not
;; sure what the best way to manage that, maybe move org-db-contacts here?

;;; Code:

(require 'org-db)

(defalias 'scimax-contacts 'org-db-contacts "An alias for inserting contacts.")

;; * contact link
;; I have struggled with what the link should use for a path. There are two options in my opinion:
;; 1. An org-id - this is the most unambiguous, and should refer to a single heading but least readable.
;; 2. An email address - this is the most readable, but there may be many headings with this property.
;;
;; 3. A third possibility is to use a more complex syntax like
;; [[contact:jkitchin@andrew.cmu :id some-uuid]], which might be an editmark.
;;
;; I favor readability over correctness, so I will use an email address. If
;; there is more than one heading with that address, you will just have to
;; choose which one to open.


(defun scimax-contact-store-link ()
  "Store a contact link.
If you are in a contact heading we store a link."
  (let* ((email (org-entry-get (point) "EMAIL"))
	 (link (concat "contact:" email)))
    (when email
      (org-link-store-props
       :type "contact"
       :link link
       :description (or (org-entry-get (point) "NAME")
			(nth 4 (org-heading-components)))
       :email email)
      link)))


(defun scimax-contact-open-link ()
  "Follow a contact link."
  (interactive)
  (let* ((email (org-element-property :path (org-element-context)))
	 (candidates (cl-loop for (title value tags fname lup begin) in
			      (with-org-db
			       (sqlite-select org-db "select headlines.title,headline_properties.value,headlines.tags,files.filename,files.last_updated,headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\" and headline_properties.value = ?"
					      (list email))) 
			      collect
			      (list (format "%40s | %s" title fname) :filename fname :begin begin :email email)))
	 candidate)
    (cond
     ((s-contains? "@" email)
      (cond
       ((= 1 (length candidates))
	(org-db--open-contact (first candidates)))
       ((> (length candidates) 1)
	(ivy-read "Contact: " candidates :action 'org-db--open-contact))
       (t
	(error "No matching candidates found for %s" email))))
     ;; assume it is an id
     (t
      (org-db-goto-id email)))))


(defun scimax-contact-complete (&optional arg)
  "Completion function for a scimax-contact.
Optional argument ARG is ignored."
  (let* ((contacts (org-db-contacts-candidates))
	 (contact (cdr (assoc (completing-read "Contact: " contacts) contacts))))
    (org-link-store-props
     :type "contact"
     :link (format "contact:%s" (plist-get contact :email))
     :description (plist-get contact :title)
     :email (plist-get contact :email))
    (format "contact:%s" (plist-get contact :email))))


(setq org-link-make-description-function
      (lambda (link desc)
	(plist-get org-store-link-plist :description)))


(defun scimax-contact-help-echo (window object position)
  "Help-echo for scimax-contact links.
Argument WINDOW is ignored.
Argument OBJECT is ignored.
Argument POSITION is where the mouse cursor is."
  (with-current-buffer object
    (save-excursion
      (goto-char position)
      (let* ((email (org-element-property :path (org-element-context))))
	(cl-loop for (title value tags fname lup begin) in
		 (with-org-db
		  (sqlite-select org-db "select headlines.title,headline_properties.value,headlines.tags,files.filename,files.last_updated,headlines.begin
from headlines inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\" and headline_properties.value = ?"
				 (list email)))
		 concat
		 (format "%40s | %s | %s\n" email title fname))))))


(defun scimax-contact-email ()
  "Send email to contact at point."
  (interactive)
  (let ((email (org-element-property :path (org-element-context))))
    (compose-mail)
    (message-goto-to)
    (insert email)
    (message-goto-subject)))


(defun scimax-contact-add-tag ()
  "Add a tag to a contact."
  (interactive)
  (save-window-excursion
    (scimax-contact-open-link)
    (org-set-tags
     (-uniq
      (append
       (org-get-tags)
       (list (ivy-read "Tag: "
		       (flatten-tree (with-org-db (sqlite-select org-db "select tag from tags"))))))))
    (save-buffer)))


(defun scimax-contact-copy-email (&optional name-email)
  "Copy email to the clipboard.
With NAME-MAIL copy name <email> instead."
  (interactive)
  (save-window-excursion
    (scimax-contact-open-link)
    (kill-new
     (if name-email
	 (format "\"%s\" <%s>"
		 (nth 4 (org-heading-components))
		 (org-entry-get (point) "EMAIL"))
       (org-entry-get (point) "EMAIL")))))


(defun scimax-contact-to-from (&optional FROM)
  "Open mu4e with emails to contact at point.
If FROM is non-nil, emails from the contact."
  (interactive)
  (unless (fboundp 'mu4e)
    (error "mu4e does not seem to be available."))
  (org-link-open-from-string
   (if FROM
       (format "[[mu4e:query:from:%s]]"
	       (org-element-property :path (org-element-context)))
     (format "[[mu4e:query:to:%s]]"
	     (org-element-property :path (org-element-context))))))


(defun scimax-contact-related ()
  "Show documents that have the contact linked in it."
  (interactive)
  (let* ((email (org-element-property :path (org-element-context)))
	 (link-candidates (cl-loop
			   for (rl fn bg) in
			   (with-org-db
			    (sqlite-select org-db "select raw_link,filename,begin from links
left join files on links.filename_id = files.rowid
where links.type = \"contact\" and links.path = ?
order by filename" (list email))) 
			   collect
			   ;; (candidate :filename :begin)
			   (list (format "%s | %s" rl fn) :filename fn :begin bg)))

	 (results (with-org-db
		   (sqlite-select org-db "select headlines.title,properties.property,headline_properties.value, files.filename, files.last_updated,headlines.begin
from headlines
inner join headline_properties
on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"ASSIGNEDTO\" and headline_properties.value like ?" (list email))))

	 (assigned-candidates (cl-loop for (title property value fname last-updated begin) in results
				       collect
				       (list (format "%s | %s=%s | %s" title property value fname)
					     :filename fname :begin begin)))
	 (results (with-org-db
		   (sqlite-select org-db "select headlines.title,properties.property, headline_properties.value,files.filename, files.last_updated,headlines.begin
from headlines
inner join headline_properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\" and headline_properties.value like ?" (list email))))
	 (email-candidates (cl-loop for (title property value fname last-updated begin) in results
				    collect
				    (list (format "%s | %s=%s | %s" title property value fname)
					  :filename fname :begin begin))))
    (ivy-read "Choose: " (append assigned-candidates email-candidates link-candidates)
	      :action (lambda (x)
			(let ((candidate (cdr x)))
			  (find-file (plist-get candidate :filename))
			  (goto-char (plist-get candidate :begin)))))))


(use-package pretty-hydra)

(pretty-hydra-define scimax-contact
  (:title "contacts" :quit-key "q" :color blue)
  ("actions"
   (("o" scimax-contact-open-link "Open contact")
    ("e" scimax-contact-email "Email contact")
    ("c" scimax-contact-copy-email  "Copy email address")
    ("C" (scimax-contact-copy-email t) "Copy \"name\" <email>"))
   "Edit"
   (("g" scimax-contact-add-tag "Add tags"))
   "Related"
   (("r" scimax-contact-related "Related items")
    ("f" (scimax-contact-to-from t) "Emails from contact")
    ("t" scimax-contact-to-from "Emails to contact"))))



(defun scimax-contact-follow-link (&optional path)
  "Menu for actions on a contact link.
Optional argument PATH is ignored."
  (interactive)
  (scimax-contact/body))


(org-link-set-parameters
 "contact"
 :follow #'scimax-contact-follow-link
 :complete #'scimax-contact-complete
 :face '(:foreground "OrangeRed1")
 :help-echo #'scimax-contact-help-echo
 :store #'scimax-contact-store-link)


;; * Speed keys for contact entries
;; These work when you at a contact heading

(defun scimax-contacts-speed-keys (keys)
  "Find the command to run for KEYS."
  (when (or (and (bolp) (looking-at org-outline-regexp)
                 (not (null (org-entry-get (point) "EMAIL")))))
    (cdr (assoc keys scimax-contacts-speed-commands))))


(defvar scimax-contacts-speed-commands
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
	     (message (kill-new (format "[[contact:%s][%s]]" (org-entry-get (point) "EMAIL") (nth 4 (org-heading-components)))))
	     (org-store-link nil)))
    ("m" . (lambda ()
	     "Copy \"name\ <email>\""
	     (message (kill-new
		       (format "\"%s\" <%s>"
			       (nth 4 (org-heading-components))
			       (org-entry-get (point) "EMAIL"))))))
    ("?" . (lambda ()
	     "Print contacts speed key help."
	     (with-output-to-temp-buffer "*Help*"
	       (princ "Contacts Speed commands\n===========================\n")
	       (mapc #'scimax-contacts-speed-keys scimax-contacts-speed-commands)
	       (princ "\n")
	       (princ "User-defined Speed commands\n===========================\n")
	       (mapc #'org-print-speed-command org-speed-commands-user)
	       (princ "Built-in Speed commands\n=======================\n")
	       (mapc #'org-print-speed-command org-speed-commands-default))
	     (with-current-buffer "*Help*"
	       (setq truncate-lines t)))))
  "Speed key definitions for scimax-contacts.")


(add-hook 'org-speed-command-hook 'scimax-contacts-speed-keys)

(defun scimax-contacts-exists-p (email)
  "Return non-nil if the EMAIL address is already in org-db."
  ;; this seems complicated. why cand I just check headline_properties.value?
  (not (null (with-org-db (sqlite-select org-db "select files.filename,headlines.begin,headlines.title
from headlines inner join headline-properties on headlines.rowid = headline_properties.headline_id
inner join properties on properties.rowid = headline_properties.property_id
inner join files on files.rowid = headlines.filename_id
where properties.property = \"EMAIL\" and headline_properties.value = ?" (list email))))))



(defvar scimax-message-org-contacts-file
  (expand-file-name (locate-user-emacs-file "message-contacts.org")) 
  "File name to store contacts captured from messages.")



;; * Capture contacts in messages
;; I use this with mu4e, but it should work in any message.

(defun scimax-message-get-emails ()
  "Captures emails in a message."
  (interactive)
  (let* ((captured-results (mapcar 's-trim
				   (append
				    (s-split "," (message-field-value "To"))
				    (s-split "," (message-field-value "From"))
				    (s-split "," (or (message-field-value "CC") "")))))
	 (emails (cl-loop for s in captured-results
			  if (string-match
			      ;; adapted from thing-at-point-email-regexp to add group
			      "<?\\([-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+\\)>?"
			      s)
			  collect
			  (list (s-trim (s-replace (match-string 0 s) "" s))
				(match-string 1 s)))))

    (with-current-buffer (find-file-noselect scimax-message-org-contacts-file)
      (goto-char (point-max))
      (when (not (bolp))
	(insert "\n"))
      (insert (cl-loop for (name email) in emails
		       unless (scimax-contacts-exists-p email)
		       concat
		       (format "* %s
   :PROPERTIES:
   :ID: %s
   :EMAIL: %s
   :END:\n\n" (if (string= "" name) email name) (org-id-new) email)))
      (save-buffer)
      (org-db-update-buffer t))))

;; * mu4e integration
(defvar with-mu4e (fboundp 'mu4e)
  "If non-nil it means we have mu4e available.")

;; I want to make sure if I reply, I have contacts.
(when with-mu4e
  (advice-add #'mu4e-compose-reply :before #'scimax-message-get-emails)

  (define-key mu4e-compose-mode-map "\C-c]" 'scimax-contacts)
  (define-key message-mode-map "\C-c]" 'scimax-contacts))

(provide 'scimax-contacts)

;;; scimax-contacts.el ends here
