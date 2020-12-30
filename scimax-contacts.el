;;; scimax-contacts.el --- org-mode contacts in scimax

;;; Commentary:
;;
;; Scimax contacts library - provides a link and integration with org-db to manage contacts.
;;
;; It relies on org-db to find contacts.
;;

;;; Code:

(require 'org-db)

;; * contact link
;; I have struggled with what the link should use for a path. There are two options in my opinion:
;; 1. An org-id - this is the most unambiguous, but least readable.
;; 2. An email address - this is the most readable, but there may be many headings with this property.
;;
;; 3. A third possibility is to use a more complex syntax like [[contact:jkitchin@andrew.cmu :id some-uuid]]
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
      (org-store-link-props
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
			      (emacsql org-db
				       [:select [headlines:title
						 headline-properties:value
						 headlines:tags files:filename files:last-updated headlines:begin]
						:from headlines
						:inner :join headline-properties
						:on (=  headlines:rowid headline-properties:headline-id)
						:inner :join properties
						:on (= properties:rowid headline-properties:property-id)
						:inner :join files :on (= files:rowid headlines:filename-id)
						:where (and  (= properties:property "EMAIL")
							     (= headline-properties:value $s1))]
				       email)
			      collect
			      (list (format "%40s | %s" title fname) :filename fname :begin begin))))
    (if (= 1 (length candidates))
	(org-db--open-contact (car candidates))
      (ivy-read "Contact: " candidates :action 'org-db--open-contact))))


(defun scimax-contact-complete (&optional arg)
  "Completion function for a scimax-contact.
Optional argument ARG is ingored.
TODO: I don't know what to do for the description yet"
  (let* ((contacts (org-db-contacts-candidates))
	 (contact (cdr (assoc (completing-read "Contact: " contacts) contacts)))
	 contact-id desc)
    (with-current-buffer (find-file-noselect (plist-get contact :filename))
      (goto-char (plist-get contact :begin))
      (setq contact-id (org-id-get-create)
	    desc (nth 4 (org-heading-components))))
    (format "contact:%s" (org-entry-get (point) "EMAIL"))))


(defun scimax-contact-help-echo (window object position)
  "Help-echo for scimax-contact links.
Argument WINDOW is ignored.
Argument OBJECT is ignored.
Argument POSITION is where the mouse cursor is."
  "A contact")


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
    (org-set-tags-to
     (-uniq
      (append
       (org-get-tags-at)
       (list (ivy-read "Tag: "
		       (-flatten (emacsql org-db [:select [tags:tag]
							  :from tags ])))))))
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
  (org-open-link-from-string
   (if FROM
       (format "[[mu4e:query:from:%s]]"
	       (org-element-property :path (org-element-context)))
     (format "[[mu4e:query:to:%s]]"
	     (org-element-property :path (org-element-context))))))


(defhydra scimax-contact (:color blue :hint nil)
  "contact:"
  ("o" scimax-contact-open-link "Open contact")
  ("e" scimax-contact-email "Email contact")
  ("c" scimax-contact-copy-email  "Copy email address")
  ("C" (scimax-contact-copy-email t) "Copy \"name\" <email>")
  ("r" nil "Related items")
  ("t" scimax-contact-add-tag "Add tags")
  ("F" (scimax-contact-to-from t) "Emails from contact")
  ("T" scimax-contact-to-from "Emails to contact"))


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
  (not (null (emacsql org-db
		      [:select [files:filename headlines:begin headlines:title]
			       :from headlines
			       :inner :join headline-properties
			       :on (=  headlines:rowid headline-properties:headline-id)
			       :inner :join properties
			       :on (= properties:rowid headline-properties:property-id)
			       :inner :join files :on (= files:rowid headlines:filename-id)
			       :where (and (= properties:property "EMAIL")
					   (= headline-properties:value $s1))]
		      email))))



(defvar scimax-message-org-contacts-file
  (expand-file-name "message-contacts.org" scimax-user-dir)
  "File name to store contacts captured from messages.")



;; * Capture contacts in messages
;; I use this this mu4e, but it should work in any message.

(defun scimax-message-get-emails ()
  "Captures emails in a message."
  (interactive)
  (let* ((captured-results (mapcar 's-trim (append
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
  (advice-add #'mu4e-compose-reply :before #'scimax-mu4e-get-emails))

(provide 'scimax-contacts)

;;; scimax-contacts.el ends here
