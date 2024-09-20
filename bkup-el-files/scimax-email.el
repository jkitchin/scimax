;;; scimax-email.el --- Email functions -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:
;; * Regular email functions
(require 'org-ref-export)

;;;###autoload
(defun email-region (start end)
  "Send region as the body of an email."
  (interactive "r")
  
  (let* ((org-export-before-parsing-hook '((lambda (_)
					     (goto-char (point-min))
					     (unless (re-search-forward "bibliography:" nil t)
					       (goto-char (point-max))
					       (insert (format
							"\nbibliography:%s"
							(if (stringp bibtex-completion-bibliography)
							    bibtex-completion-bibliography
							  (string-join
							   bibtex-completion-bibliography ",")))))
					     (org-ref-csl-preprocess-buffer 'ascii))))
	 (org-export-show-temporary-export-buffer nil)
	 
	 (content (progn
		    (org-ascii-export-as-ascii nil nil nil t)
		    (with-current-buffer "*Org ASCII Export*"
		      (buffer-string)))))
    

    (compose-mail)
    (message-goto-body)
    (insert content)
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
		(current-time-string)))
  
  (save-excursion
    (switch-to-buffer (marker-buffer  *email-heading-point*))
    (goto-char (marker-position  *email-heading-point*))
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (setq *email-heading-point* nil)
    (org-set-property "SENT-ON" (current-time-string))
    ;; reset this incase you added new ones
    (org-set-property "TO" (mapconcat 'identity  *email-to-addresses* ", "))
    (org-set-property "Message-ID" *email-mu4e-link-to-message*)
    ;; remove unsent tag if it is there, and add sent
    (let ((tags (org-get-tags)))
      (add-to-list 'tags "sent")
      (setq tags (-remove-item "unsent" tags))
      (org-set-tags-to tags)))
  (mu4e-update-mail-and-index t))

;;;###autoload
(defun email-heading (send)
  "Send the current org-mode heading as the body of an email, with headline as the subject.

use these properties if they exist
TO
CC
BCC
SUBJECT
OTHER-HEADERS is an alist specifying additional
header fields.  Elements look like (HEADER . VALUE) where both
HEADER and VALUE are strings.

with prefix arg SEND, send immediately.

Save when it was sent as a SENT property. this is overwritten on
subsequent sends."
  (interactive "P")
					; store location.
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (org-mark-subtree)
    (let* ((org-export-before-parsing-hook '((lambda (_)
					       (unless (re-search-forward "bibliography:" nil t)
						 (goto-char (point-max))
						 (insert (format "\nbibliography:%s"
								 (if (stringp bibtex-completion-bibliography)
								     bibtex-completion-bibliography
								   (string-join bibtex-completion-bibliography ","))))))
					     org-ref-csl-preprocess-buffer))
	   (content (org-export-string-as
		     (buffer-substring (point) (mark)) 'ascii t))
	   
	   (TO (org-entry-get (point) "TO" t))
	   (CC (org-entry-get (point) "CC" t))
	   (BCC (org-entry-get (point) "BCC" t))
	   (SUBJECT (or (org-entry-get (point) "SUBJECT" t) (nth 4 (org-heading-components))))
	   (OTHER-HEADERS (read (or (org-entry-get (point) "OTHER-HEADERS") "()")))
	   (continue nil)
	   (switch-function nil)
	   (yank-action nil)
	   (send-actions '((email-send-action . nil))))

      (compose-mail TO SUBJECT OTHER-HEADERS
		    continue switch-function yank-action
		    send-actions)
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
    (let* ((TO (org-entry-get (point) "TO" t))
           (CC (org-entry-get (point) "CC" t))
           (BCC (org-entry-get (point) "BCC" t))
           (SUBJECT (or (org-entry-get (point) "SUBJECT" t)
			(nth 4 (org-heading-components))))
           (OTHER-HEADERS (eval (org-entry-get (point) "OTHER-HEADERS")))
	   (org-export-before-parsing-hook '((lambda (_)
					       (goto-char (point-min))
					       (unless (re-search-forward "bibliography:" nil t)
						 (goto-char (point-max))
						 (insert (format
							  "\nbibliography:%s"
							  (if (stringp bibtex-completion-bibliography)
							      bibtex-completion-bibliography
							    (string-join
							     bibtex-completion-bibliography ",")))))
					       (org-ref-csl-preprocess-buffer 'ascii))))
	   (content (progn
                      (unless (org-at-heading-p) (outline-previous-heading))
                      (let ((headline (org-element-at-point)))
			(org-end-of-meta-data)
			(save-restriction
			  (narrow-to-region (point)
					    (org-element-property :contents-end headline))
			  (org-ascii-export-as-ascii nil nil nil t)
			  (with-current-buffer "*Org ASCII Export*"
			    (buffer-string))))))
           (continue nil)
           (switch-function nil)
           (yank-action nil)
           (send-actions '((email-send-action . nil))))

      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function yank-action send-actions)
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
    (let* ((key (bibtex-completion-get-key-bibtex))
	   (pdf (bibtex-completion-find-pdf key)))
      (bibtex-copy-entry-as-kill)
      (compose-mail)
      (message-goto-body)
      (insert (pop bibtex-entry-kill-ring))
      (message-goto-subject)
      (insert (concat "Bibtex entry: " key))
      (when (and pdf (file-exists-p pdf))
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

#+name: data
| TO           | name | application-id | subject    |
|--------------+------+----------------+------------|
| some@one.com  | Bill |            123 | [J] person |
| some@two.com  | John |            456 | [J] two    |


#+BEGIN_SRC emacs-lisp :var d=data
(mail-merge-make-headings
 \"Dear ${name},

  Thank you for submitting application ${application-id}.

  -----------------------
  Please do not delete this.
  [[id:${ID}]]

  \"
  (cl-loop for (to name id subject) in d collect
	   (list (cons \"TO\" to)
                 (cons \"name\" name)
                 (cons \"application-id\" id)
		 (cons \"SUBJECT\" subject))))
#+END_SRC

This only creates the messages. It does not send them.

See `mail-merge-send-heading' to send one heading (e.g. to test it).

See `mail-merge' to send them all.

See also the speed keys below to send each heading manually."
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
      (cl-loop for data in data-source
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
		      (org-set-tags-to (-uniq (append '("unsent") (org-get-tags))))
				     
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
			(org-entry-put (point) "SUBJECT" (cdr (assoc "SUBJECT" data))))))))))


;;;###autoload
(defun mail-merge-send-heading (&optional just-send)
  "Create message with org-heading body at point using heading properties.
With prefix arg, also send the message and move to the next one."
  (interactive "P")
  (setq *email-heading-point* (set-marker (make-marker) (point)))
  (save-excursion
    (let ((content (progn
		     (unless (org-at-heading-p) (outline-previous-heading))
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
	  (send-actions '((email-send-action . nil))))

      (compose-mail TO SUBJECT OTHER-HEADERS continue switch-function
		    yank-action send-actions)
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
	       (org-get-tags))))
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

;; Should I make a mail-merge speed command, M? or should that always require
;; thinking. I lean towards thinking.
(setq org-speed-commands-mail-merge
      '(("m" . (mail-merge-send-heading))
	("s" . (mail-merge-send-heading t))
	("?" . mail-merge-speed-key-help)))

(add-hook 'org-speed-command-hook 'org-speed-mail-merge)

(provide 'scimax-email)

;;; scimax-email.el ends here
