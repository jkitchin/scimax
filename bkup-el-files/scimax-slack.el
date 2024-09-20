;;; scimax-slack.el --- a Slack integration for scimax and org-mode

;;; Commentary:
;; Light wrapper around the emacs-slack package to enable some org-mode integration

;; scimax-slack integration
;; https://github.com/yuya373/emacs-slack
;;
;; `slack-start' to get slack started
;;
;; It looks like you have to get a token for a workspace

(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "kitchingroup"
   :default t
   :token (auth-source-pick-first-password :host "kitchingroup.slack.com"
					   :user "jkitchin@andrew.cmu.edu")
   :subscribed-channels '(general random)
   :full-and-display-names t))



;; https://github.com/titaniumbones/ox-slack
;;
;; this has some export that is different than below
(require 'ox-slack)

(defhydra slack (:color blue)
  "slack"
  ("st" slack-change-current-team "team")
  ("sc" slack-channel-select "channel")
  ("sr" slack-select-rooms "room")
  ("su" slack-select-unread-rooms "unread")
  ("si" slack-im-select "instant message"))


;; * mrkdwn exporter
;;
;; Slack seems to use some basic form of markdown with some differences. This is
;; a custom exporter to manage those:
;; 1. Put code in ```{code}```
(defun scimax-mrkdwn-src-block (src contents info)
  (format "```\n%s\n```" (org-element-property :value src)))


(defun scimax-mrkdown-underline (underline contents info)
  "underline not supported"
  (buffer-substring (org-element-property :contents-begin underline)
		    (org-element-property :contents-end underline)))


(defun scimax-mrkdwn-strike-through (strike-through contents info)
  (format "~%s~" (buffer-substring (org-element-property :contents-begin strike-through)
				   (org-element-property :contents-end strike-through))))

;; TODO
;; Images/equations
;; cite links
;; file attachments
(org-export-define-derived-backend 'scimax-mrkdwn 'md
  :translate-alist '((src-block . scimax-mrkdwn-src-block)
		     (underline . scimax-mrkdwn-underline)
		     (strike-through . scimax-mrkdwn-strike-through)))



;; * Send to slack

(defun scimax-org-to-slack ()
  (interactive)
  "Send region or heading to a slack channel."
  (let* ((team (slack-team-select))
	 (candidates (append (cl-loop for team in (list team)
				      for channels = (slack-team-channels team)
				      nconc channels)
			     (cl-loop for team in (list team)
				      for ims = (cl-remove-if #'(lambda (im)
								  (not (oref im is-open)))
							      (slack-team-ims team))
				      nconc ims)))
	 (room (slack-room-select candidates team))
	 (text (if (region-active-p)
		   (buffer-substring
		    (region-beginning)
		    (region-end))
		 (save-excursion
		   (save-restriction
		     (when (not (org-at-heading-p))
		       (org-previous-visible-heading 1))
		     (widen)
		     (org-narrow-to-subtree)
		     (buffer-string)))))
	 (msg (org-export-string-as text 'scimax-mrkdwn nil '(:with-toc nil :with-tags nil))))
    (slack-message-send-internal msg
				 room
				 team)))

(defun slack-elfeed-entry ()
  (interactive)
  (let* ((title (elfeed-entry-title elfeed-show-entry))
	 (url (elfeed-entry-link elfeed-show-entry))
	 (content (elfeed-entry-content elfeed-show-entry))
	 (text (format-spec "Title: %t

%c

%u"
			    `((?t . ,title)
			      (?c . ,(html-to-markdown (elfeed-deref content)))
			      (?u . ,url))))
	 (msg (org-export-string-as text 'scimax-mrkdwn nil '(:with-toc nil :with-tags nil)))
	 (team (slack-team-select))
	 (candidates (append (cl-loop for team in (list team)
				      for channels = (slack-team-channels team)
				      nconc channels)
			     (cl-loop for team in (list team)
				      for ims = (cl-remove-if #'(lambda (im)
								  (not (oref im is-open)))
							      (slack-team-ims team))
				      nconc ims)))
	 (room (slack-room-select candidates team)))
    
    (slack-message-send-internal msg
				 room
				 team)))

(provide 'scimax-slack)

;;; scimax-slack.el ends here
