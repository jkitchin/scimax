;;; scimax-gitter.el --- org-mode + gitter integration

;;; Commentary:
;; This library provides some integration of Emacs/org-mode and gitter.
;;
;; You need a like like this in your ~/.authinfo
;; machine gitter.im password token
;;
;; where token comes from going to https://developer.gitter.im/apps

;;; Code:

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
	  (list (plist-get room :name)
		(plist-get room :id)))))


(defun sg-send-region-to-room (r1 r2 room-id)
  "Send the region (R1 to R2) to a gitter room with ROOM-ID."
  (interactive (list (region-beginning)
		     (region-end)
		     (let ((rooms (sg-get-rooms)))
		       (cadr (assoc (completing-read "Room: " rooms) rooms)))))
  (let* ((token (sg-get-token))
	 (text (cond
		((eq major-mode 'org-mode)
		 (org-export-string-as
		  (buffer-substring r1 r2)
		  'md t '(:with-toc nil :with-tags nil)))
		(t
		 (buffer-substring r1 r2))))
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
	 (room-id (cadr (assoc (or (org-entry-get (point) "gitter-room")
				   (completing-read "Room: " rooms))
			       rooms)))
	 (result))
    (save-excursion
      (unless (looking-at org-heading-regexp)
	(org-previous-visible-heading 1))
      (org-mark-element)
      (setq result (sg-send-region-to-room (region-beginning)
					   (region-end)
					   room-id)))
    (org-entry-put (point) "gitter-room-id" room-id)
    (org-entry-put (point) "gitter-message-id" (plist-get result :id))
    (org-entry-put (point) "gitter-sent" (plist-get result :sent))))


(defun sg-update-heading-message ()
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
		  'md t '(:with-toc nil :with-tags nil))))
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

(provide 'scimax-gitter)

;;; scimax-gitter.el ends here
