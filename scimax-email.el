;;; scimax-email.el --- Email functions

;;; Commentary:
;;

;;; Code:
;; * Regular email functions

;;;###autoload
(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  (let ((content (buffer-substring start end))
	cite
	keys
	bibfile p1 p2
	(bib-entries '()))
    (goto-char (region-beginning))

    (save-restriction
      (narrow-to-region start end)
      (org-element-map (org-element-parse-buffer) 'link
	(lambda (link)
	  (when (-contains? org-ref-cite-types (org-element-property :type link))
	    (setq keys (s-split "," (org-element-property :path link)))
	    (loop for key in keys
		  do
		  (setq bibfile
			(cdr (org-ref-get-bibtex-key-and-file key)))
		  (with-current-buffer (find-file-noselect bibfile)
		    (bibtex-search-entry key)
		    (save-excursion
		      (bibtex-beginning-of-entry)
		      (setq p1 (point))
		      (bibtex-end-of-entry)
		      (setq p2 (point)))
		    (add-to-list 'bib-entries (buffer-substring-no-properties p1 p2))))))))

    (compose-mail)
    (message-goto-body)
    (insert content)
    (insert "\n\n% Bibtex Entries:\n\n")
    (loop for bib-entry in bib-entries
	  do
	  (insert bib-entry))
    (message-goto-to)))

;;;###autoload
(defun email-buffer ()
  "Send buffer as the body of an email."
  (interactive)
  (email-region (point-min) (point-max)))


(defvar *email-heading-point* nil
  "Global variable to store point in for returning.")

(defvar *email-to-addresses* nil
  "Global variable to store to address in email.")

(defvar *email-mu4e-link-to-message* nil
  "Global var to store mu4e link to Message-ID of last email.")

(defun email-heading-return ()
  "After returning from compose do this in the heading.
Sets SENT-ON, TO and a Message-ID property.
Removes unsent tag if there, and adds sent to tags"
  (switch-to-buffer (marker-buffer  *email-heading-point*))
  (goto-char (marker-position  *email-heading-point*))
  (setq *email-heading-point* nil)
  (org-set-property "SENT-ON" (current-time-string))
  ;; reset this incase you added new ones
  (org-set-property "TO" (mapconcat 'identity  *email-to-addresses* ", "))
  (org-set-property "Message-ID" *email-mu4e-link-to-message*)
  ;; remove unsent tag if it is there, and add sent
  (let ((tags (org-get-tags-at)))
    (add-to-list 'tags "sent")
    (setq tags (-remove-item "unsent" tags))
    (org-set-tags-to tags)))


(defun email-send-action ()
  "Send action for `compose-mail'."
  (setq
   *email-to-addresses*
   (mapcar
    'cadr
    (mail-extract-address-components (mail-fetch-field "TO") t)))
  (setq *email-mu4e-link-to-message*
	(format "[[mu4e:msgid:%s][%s (%s)]]"
		;; borrowed from https://github.com/girzel/gnorb/blob/master/gnorb-utils.el#L137
		(replace-regexp-in-string
		 "\\(\\`<\\|>\\'\\)" "" (mail-fetch-field "Message-ID"))
		(mail-fetch-field "Subject")
		(current-time-string))))

;;;###autoload
(defun email-heading ()
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties
TO
CC
BCC
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive)
					; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (org-mark-subtree)
    (let ((content (buffer-substring (point) (mark)))
	  (TO (org-entry-get (point) "TO" t))
	  (CC (org-entry-get (point) "CC" t))
	  (BCC (org-entry-get (point) "BCC" t))
	  (SUBJECT (nth 4 (org-heading-components)))
	  (OTHER-HEADERS (read (or (org-entry-get (point) "OTHER-HEADERS") "()")))
	  (continue nil)
	  (switch-function nil)
	  (yank-action nil)
	  (send-actions '((email-send-action . nil)))
	  (return-action '(email-heading-return)))

      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
      (message-goto-body)
      (insert content)
      (when CC
	(message-goto-cc)
	(insert CC))
      (when BCC
	(message-goto-bcc)
	(insert BCC))
      (if TO
	  (message-goto-body)
	(message-goto-to)))))

;;;###autoload
(defun email-heading-body (send)
  "Send the current org-mode heading content as the body of an email.

Does not include the headline
Use these properties on the headline to create the email.
TO
CC
BCC
SUBJECT (or use the headline)
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

With prefix arg SEND, sends immediately.

Save when it was sent as a SENT property on the headline. This is
overwritten on subsequent sends."
  (interactive "P")
					; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((TO (org-entry-get (point) "TO" t))
          (CC (org-entry-get (point) "CC" t))
          (BCC (org-entry-get (point) "BCC" t))
          (SUBJECT (or (org-entry-get (point) "SUBJECT" t)
		       (nth 4 (org-heading-components))))
          (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
	  (content (progn
                     (unless (org-on-heading-p) (outline-previous-heading))
                     (let ((headline (org-element-at-point)))
		       (org-end-of-meta-data)
                       (buffer-substring
			(point)
                        (org-element-property :contents-end headline)))))
          (continue nil)
          (switch-function nil)
          (yank-action nil)
          (send-actions '((email-send-action . nil)))
          (return-action '(email-heading-return)))

      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions return-action)
      (message-goto-body)
      (insert content)
      (when CC
        (message-goto-cc)
        (insert CC))
      (when BCC
        (message-goto-bcc)
        (insert BCC))
      (if TO
          (message-goto-body)
        (message-goto-to))
      (when send
	(message-send-and-exit)))))

;;;###autoload
(defun email-bibtex-entry ()
  "Email bibtex entry/pdf that the cursor is in."
  (interactive)

  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((key (reftex-get-bib-field "=key=" (bibtex-parse-entry t)))
	   (pdf (expand-file-name
		 (concat key ".pdf")
		 org-ref-pdf-directory)))
      (bibtex-copy-entry-as-kill)
      (compose-mail)
      (message-goto-body)
      (insert (pop bibtex-entry-kill-ring))
      (message-goto-subject)
      (insert (concat "Bibtex entry: " key))
      (when (file-exists-p pdf)
	(mml-attach-file pdf))
      (message-goto-to))))

;; * Mail-merge
;; mail-merge library for using org-mode, mu4e and email.el to send mail
;; merges. The idea is to write a mail-template that can be formatted by
;; `s-format', use emacs-lisp to generate a data-source that will populate each
;; template and generate an org-mode heading for each message using
;; `mail-merge-make-headings'. Then, you can review the messages, edit as
;; needed, and finally send them via `mail-merge'.

;;; Code:

(defun mail-merge-make-headings (s-template data-source)
  "Create the mail headings.
S-TEMPLATE is an `s-format' string.  DATA-SOURCE is an alist of
entries that will be used to expand the S-TEMPLATE and generate
the headings.

Each entry in DATA-SOURCE must contain \"TO\" which is the email
address(es) to send the message to.  Also a \"SUBJECT\" must be
included, as well as a \"HEADLINE\" which will be used in the
headline instead of the subject.

The function will make a headline called Messages as a subheading
of the current heading, and each message will be a subheading of
the Messages heading.

an org-id will be created for each message. you can use ${ID} in
the S-TEMPLATE to refer to the ID property of the generated
message headline.

Each message will be tagged :unsent:

This function does not send the messages.

Example usage:

 (mail-merge-make-headings
 \"Dear ${name},

  Please check this file: ${file}.

  -----------------------
  Please do not delete this.
  [[id:${ID}]]

  \"
 '(((\"TO\" . \" some@person.com \")
    (\"name\" . \"Someone\")
    (\"file\" . \" tees.org \")
    (\"SUBJECT\" . \" [J] Person \"))))"
  ;; create Messages heading if needed
  (save-restriction
    (org-narrow-to-subtree)
    (let ((this-id nil))
      (save-excursion
	(unless (and (outline-next-heading)
		     (string= "Messages" (nth 4 (org-heading-components))))
	  (org-insert-subheading nil)
	  (insert "Messages"))
	(setq this-id (org-id-get-create)))

      ;; create Message entries
      (loop for data in data-source
	    do (save-excursion
		 (save-restriction
		   (org-narrow-to-subtree)
		   (goto-char (cdr (org-id-find this-id)))
		   (org-insert-heading-after-current)
		   (org-do-demote)
		   (setq data (add-to-list 'data
					   (cons "ID" (org-id-get-create))))
		   (outline-previous-heading)
		   (end-of-line)
		   (insert (or (cdr (assoc "HEADLINE" data))
			       (cdr (assoc "SUBJECT" data))))
		   (org-end-of-meta-data)
		   (insert (s-format s-template 'aget data))
		   ;; refill now that it is expanded
		   (outline-previous-heading)
		   (save-restriction
		     (org-narrow-to-subtree)
		     (goto-char (point-min))
		     (fill-region (point-min) (or (re-search-forward "^--"
								     nil t)
						  (point-max))))
		   (org-entry-put (point) "TO" (cdr (assoc "TO" data)))
		   (when (cdr (assoc "SUBJECT" data))
		     (org-entry-put (point) "SUBJECT" (cdr (assoc "SUBJECT" data))))
		   (org-set-tags-to (-uniq (append '("unsent") (org-get-tags-at))))))))))


;;;###autoload
(defun mail-merge-send-heading (&optional just-send)
  "Create message with org-heading body at point using heading properties.
With prefix arg, also send the message and move to the next one."
  (interactive "P")
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
		     (unless (org-on-heading-p) (outline-previous-heading))
		     (let ((headline (org-element-at-point)))
		       (buffer-substring
			(progn (org-end-of-meta-data t) (point))
			(org-element-property :contents-end headline)))))
	  (TO (org-entry-get (point) "TO" t))
	  (CC (org-entry-get (point) "CC" t))
	  (BCC (org-entry-get (point) "BCC" t))
	  (SUBJECT (replace-regexp-in-string
		    "{{.*}} "
		    ""
		    (or (org-entry-get (point) "SUBJECT" t)
			(nth 4 (org-heading-components)))))
	  (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
	  (continue nil)
	  (switch-function nil)
	  (yank-action nil)
	  (send-actions '((email-send-action . nil)))
	  (return-action '(email-heading-return)))

      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function
		    yank-action send-actions return-action)
      (message-goto-body)
      (insert content)
      (when CC
	(message-goto-cc)
	(insert CC))
      (when BCC
	(message-goto-bcc)
	(insert BCC))
      ;; move point back to the top
      (message-goto-to)
      (when just-send
	(message-send-and-exit))))
  (org-todo "DONE")

  (let ((tags (-remove
	       (lambda (x) (string= x "unsent"))
	       (org-get-tags-at))))
    (add-to-list 'tags "sent")
    (org-set-tags-to tags))
  (message  (format "sent to %s" (org-entry-get (point) "TO")))
  (outline-hide-entry)
  (outline-next-heading)
  (outline-show-entry))


;;;###autoload
(defun mail-merge ()
  "Run a mail-merge in the current heading.
This will map over entries tagged unsent with a TO property, and
mail the body of each heading using
`mail-merge-send-heading'. Headings tagged ignore will be ignored."
  (interactive)
  (org-map-entries
   (lambda ()
     (mail-merge-send-heading t)
     (sleep-for 0.2))
   ;; on headings that are tagged unsent
   "unsent-ignore+TO={.}"))


;; ** Speed commands for mail-merge
(defun org-speed-mail-merge (keys)
  "Find the command to run for KEYS."
  (when (or (and (bolp) (looking-at org-outline-regexp)
		 (not (null (org-entry-get (point) "TO")))))
    (cdr (assoc keys org-speed-commands-mail-merge))))

(defun mail-merge-speed-key-help ()
  "Print speed key help."
  (with-output-to-temp-buffer "*Help*"
    (princ "Mail merge speed commands\n==========================\n")
    (mapc #'org-print-speed-command org-speed-commands-mail-merge)
    (princ "\n")
    (princ "User-defined Speed commands\n===========================\n")
    (mapc #'org-print-speed-command org-speed-commands-user)
    (princ "Built-in Speed commands\n=======================\n")
    (mapc #'org-print-speed-command org-speed-commands-default))
  (with-current-buffer "*Help*"
    (setq truncate-lines t)))

(setq org-speed-commands-mail-merge
      '(("m" . (mail-merge-send-heading))
	("s" . (mail-merge-send-heading t))
	("?" . mail-merge-speed-key-help)))

(add-hook 'org-speed-command-hook 'org-speed-mail-merge)

(provide 'scimax-email)

;;; email.el ends here
