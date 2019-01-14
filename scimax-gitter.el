;;; scimax-gitter.el --- org-mode + gitter integration

;;; Commentary:
;; This library provides some integration of Emacs/org-mode and gitter.
;;
;; You need a like like this in your ~/.authinfo
;; machine gitter.im password token
;;
;; where token comes from going to https://developer.gitter.im/apps

;;; Code:
(require 'ox-md)

;; Customized markdown exporter to handle some cases different than the regular
;; md exporter.
(org-export-define-derived-backend 'sg-md 'md
  :translate-alist '((strike-through . sg-org-md-strikethrough)
		     (underline . sg-org-md-underline)
		     (latex-fragment . sg-org-md-latex-fragment))
  :options-alist '((:with-toc nil "toc" org-export-with-toc)
		   (:with-tags nil "tags" org-export-with-tags)))

;; strikethrough is not rendered in the markdown format needed for gitter. I
;; define an export function here, and set it in the exporter.
(defun sg-org-md-strikethrough (_strikethrough contents _info)
  "Transcode STRIKETHROUGH object into Markdown format.
CONTENTS is the text within strikethrough markup.  INFO is a plist used as
a communication channel."
  (format "~~%s~~" contents))


;; also, underline is exported as html, so I quench that here.
(defun sg-org-md-underline (_underline contents _info)
  "Transcode UNDERLINE object into Markdown format.
CONTENTS is the text within strikethrough markup.  INFO is a plist used as
a communication channel."
  (format "_%s_" contents))


;; We need latex-fragments to go verbatim with $$
(defun sg-org-md-latex-fragment (latex-fragment _contents info)
  "Transcode a LATEX-FRAGMENT object from Org to MD.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let ((latex-frag (org-element-property :value latex-fragment))
	(processing-type (plist-get info :with-latex)))
    latex-frag))


(defun sg-get-token ()
  "Get the gitter token from auth-sources."
  (let* ((plist (car (auth-source-search :max 1 :host "gitter.im")))
	 (k (plist-get plist :secret)))
    (if (functionp k)
	(funcall k))))


(defun sg-get-rooms ()
  "Get an alist of (name . id) for your rooms."
  (let* ((url-request-method "GET")
	 (token (sg-get-token))
	 (url-request-extra-headers  `(("Authorization" . ,(concat "BEARER "  token))))
	 (url "https://api.gitter.im/v1/rooms")
	 (json-object-type 'plist)
	 (room-data (with-current-buffer (url-retrieve-synchronously url)
		      (json-read-from-string
		       (buffer-substring url-http-end-of-headers (point-max))))))
    (loop for room across room-data collect
	  (cons (plist-get room :name)
		(plist-get room :id)))))


(defun sg-send-region-to-room (r1 r2 room-id)
  "Send the region (R1 to R2) to a gitter room with ROOM-ID."
  (interactive (list (region-beginning)
		     (region-end)
		     (let ((rooms (sg-get-rooms)))
		       (cdr (assoc (completing-read "Room: " rooms) rooms)))))
  (let* ((token (sg-get-token))
	 (org-export-with-toc nil)
	 (org-export-with-tags nil)
	 (text (cond
		((eq major-mode 'org-mode)
		 (org-export-string-as
		  (buffer-substring r1 r2)
		  'sg-md t))
		(t
		 (buffer-substring-no-properties r1 r2))))
  	 (url-request-method "POST")
  	 (url-mime-accept-string "application/json")
  	 (url-mime-encoding-string "application/json")
  	 (url-request-extra-headers  `(("Content-Type" . "application/json")
  				       ("Authorization" . ,(concat "BEARER "  token))))
  	 (url-request-data (json-encode `(("text" . ,text))))
  	 (url (format "https://api.gitter.im/v1/rooms/%s/chatMessages" room-id))
  	 (json-object-type 'plist))
    (with-current-buffer (url-retrieve-synchronously url)
      (json-read-from-string
       (buffer-substring url-http-end-of-headers (point-max))))))


(defun sg-send-heading-to-room ()
  "Send an org-heading to a gitter room."
  (interactive)
  (let* ((rooms (sg-get-rooms))
	 (room (or (org-entry-get (point) "gitter-room")
		   (completing-read "Room: " rooms)))
	 (room-id (cdr (assoc room rooms)))
	 (result))
    (save-excursion
      (unless (looking-at org-heading-regexp)
	(org-previous-visible-heading 1))
      (org-mark-element)
      (setq result (sg-send-region-to-room (region-beginning)
					   (region-end)
					   room-id)))
    (org-entry-put (point) "gitter-room" room)
    (org-entry-put (point) "gitter-room-id" room-id)
    (org-entry-put (point) "gitter-message-id" (plist-get result :id))
    (org-entry-put (point) "gitter-sent" (plist-get result :sent))))


(defun sg-update-heading-to-room ()
  "Update the message from the current heading.

PUT /v1/rooms/:roomId/chatMessages/:chatMessageId"
  (interactive)
  (let* ((token (sg-get-token))
	 (room-id (org-entry-get (point) "gitter-room-id"))
	 (message-id (org-entry-get (point) "gitter-message-id"))
	 (text (save-excursion
		 (unless (looking-at org-heading-regexp)
		   (org-previous-visible-heading 1))
		 (org-mark-element)
		 (org-export-string-as
		  (buffer-substring (region-beginning)(region-end))
		  'sg-md t '(:with-toc nil :with-tags nil))))
  	 (url-request-method "PUT")
  	 (url-mime-accept-string "application/json")
  	 (url-mime-encoding-string "application/json")
  	 (url-request-extra-headers  `(("Content-Type" . "application/json")
  				       ("Authorization" . ,(concat "BEARER "  token))))
  	 (url-request-data (json-encode `(("text" . ,text))))
  	 (url (format "https://api.gitter.im/v1/rooms/%s/chatMessages/%s"
		      room-id message-id))
  	 (json-object-type 'plist)
	 (result (with-current-buffer (url-retrieve-synchronously url)
		   (json-read-from-string
		    (buffer-substring url-http-end-of-headers (point-max))))))
    (org-entry-put (point) "gitter-editedAt" (plist-get result :editedAt))))


(defun sg-delete-heading-message ()
  "Delete the message in the current heading."
  (interactive)
  (let* ((token (sg-get-token))
	 (room-id (org-entry-get (point) "gitter-room-id"))
	 (message-id (org-entry-get (point) "gitter-message-id"))

  	 (url-request-method "DELETE")
  	 (url-mime-accept-string "application/json")
  	 (url-mime-encoding-string "application/json")
  	 (url-request-extra-headers  `(("Content-Type" . "application/json")
  				       ("Authorization" . ,(concat "BEARER "  token))))
  	 ;; (url-request-data (json-encode `(("text" . ,text))))
  	 (url (format "https://api.gitter.im/v1/rooms/%s/chatMessages/%s"
		      room-id message-id))
  	 (json-object-type 'plist))
    (url-retrieve-synchronously url)
    (org-entry-delete (point) "gitter-message-id")
    (org-entry-delete (point) "gitter-sent")
    (org-entry-delete (point) "gitter-editedAt")))


(defun sg-insert-room ()
  "Insert room and id properties with completion."
  (interactive)
  (let* ((rooms (sg-get-rooms))
	 (room (completing-read "Room: " rooms))
	 (room-id (cdr (assoc room rooms))))
    (org-entry-put (point) "gitter-room" room)
    (org-entry-put (point) "gitter-room-id" room-id)))


(defun sg-get-users-in-room ()
  "Return list of users in the room."
  (let* ((token (sg-get-token))
	 (room-id (or (org-entry-get (point) "gitter-room-id" t)
		      (let ((rooms (sg-get-rooms)))
			(cdr (assoc (completing-read "Room: " rooms) rooms)))))
  	 (url-request-method "GET")
  	 (url-mime-accept-string "application/json")
  	 (url-mime-encoding-string "application/json")
  	 (url-request-extra-headers  `(("Content-Type" . "application/json")
  				       ("Authorization" . ,(concat "BEARER "  token))))

  	 (url (format "https://api.gitter.im/v1/rooms/%s/users"
		      room-id))
  	 (json-object-type 'plist)
	 (result (with-current-buffer (url-retrieve-synchronously url)
		   (json-read-from-string
		    (buffer-substring url-http-end-of-headers (point-max)))))
	 (users (loop for user across result collect
		      (cons (plist-get user :displayName)
			    (plist-get user :username)))))
    users))

(defun sg-insert-user-in-room ()
  "Insert a user handle in the room for the current heading.

GET /v1/rooms/:roomId/users"
  (interactive)
  (let* ((users (sg-get-users-in-room)))
    (insert (format "@%s" (cdr (assoc (completing-read "User: " users) users))))))


(defun scimax-gitter-erc ()
  "Open the gitter channel in `erc'.
Go to https://irc.gitter.im/ to get your password. Save it in ~/.authinfo like this:
machine irc.gitter.im password your-token.
This token is not the same as the developer token."
  (interactive)
  (erc-ssl :server "irc.gitter.im"
	   :port 6667
	   :full-name (user-full-name)
	   :nick (user-login-name)
	   :password (let* ((plist (car (auth-source-search :max 1 :host "irc.gitter.im")))
			    (k (plist-get plist :secret)))
		       (if (functionp k)
			   (funcall k))))
  (erc-join-channel "#scimax-community"))


(provide 'scimax-gitter)

;;; scimax-gitter.el ends here
