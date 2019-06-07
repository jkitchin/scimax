;;; kitchingroup.el --- Kitchingroup utility functions

;;; Commentary:
;; This module is mostly for members of my research group.

(use-package gitter)
(require 'scimax-notebook)
(require 'cal-iso)

;;; Code:

(defcustom box-drive-root (expand-file-name "~/Box/")
  "Root directory where Box Drive exists."
  :group 'kitchingroup)


(defcustom kitchingroup-root (file-name-as-directory
			      (expand-file-name
			       "kitchingroup"
			       box-drive-root))
  "Directory where kitchingroup files will be stored."
  :group 'kitchingroup)


(defcustom kitchingroup-personal-root nil
  "Directory where personal files for the kitchingroup are.
Usually at ~/Box/andrewid."
  :group 'kitchingroup)


(defcustom kitchingroup-github-id nil
  "Your Github id.
This should be defined in user/preload.el, e.g. (setq kitchingroup-github-id \"your-id\")"
  :group 'kitchingroup)


(defun kitchingroup-new-document (path template)
  "Create a new document at PATH using TEMPLATE.
TEMPLATE should be a yasnippet name and should be a string."
  (if (file-exists-p path)
      (find-file path)
    ;; we need to make it.
    (find-file path)
    (yas-expand-snippet (yas-lookup-snippet template)))
  (goto-char (point-min)))


(defun iso-week-to-time(year week day)
  "Convert ISO YEAR WEEK DAY to elisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun kitchingroup-weekly-report ()
  "Open a weekly report.
You will be prompted with a calendar to pick a date. If you pick
a Monday, you get the report due on that day. If you pick any
other day, you get the report due on the following Monday."
  (interactive)
  (let* ((date (org-read-date nil t))
	 kg-due-date kg-review-date
	 dir
	 full-dir
	 fname)
    (cond
     ;; If you choose a monday, you get the previous week
     ((string= "Mon" (format-time-string "%a" date))
      (setq kg-due-date (iso-week-to-time
			 (string-to-number
			  (format-time-string
			   "%Y" date))
			 ;; This is current week
			 (string-to-number
			  (format-time-string
			   "%V" date))
			 ;; 1 is for Monday
			 1)
	    kg-review-date (iso-week-to-time
			    (string-to-number
			     (format-time-string
			      "%Y" date))
			    ;; This is current week
			    (string-to-number
			     (format-time-string
			      "%V" date))
			    ;; 2 is for Tuesday
			    2)
	    dir (format "reports/%s/" (format-time-string "%Y-%m-%d" kg-due-date))
	    full-dir (expand-file-name dir kitchingroup-personal-root)
	    fname (expand-file-name "weekly-report.org" full-dir))
      ;; make sure the directory exists
      (unless (file-directory-p full-dir)
	(make-directory full-dir t))

      ;; open file if it exists
      (if (file-exists-p fname)
      	  (find-file fname)
      	;; make file if it doesn't
      	(find-file fname)
      	(yas-expand-snippet
      	 (yas-lookup-snippet "weekly-report")
	 nil nil `((kg-due-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-due-date))
		   (kg-review-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-review-date)))))
      (save-buffer)
      (goto-char (point-min)))
     ;; Otherwise you get the report due the monday after the selected date.
     (t
      (setq kg-due-date (iso-week-to-time
			 (string-to-number
			  (format-time-string
			   "%Y" date))
			 ;; This is due next week
			 (+ (string-to-number
			     (format-time-string
			      "%V" date))
			    1)
			 ;; 1 is for Monday
			 1)
	    kg-review-date (iso-week-to-time
			    (string-to-number
			     (format-time-string
			      "%Y" date))
			    ;; This is current week
			    (string-to-number
			     (format-time-string
			      "%V" date))
			    ;; 2 is for Tuesday
			    2)
	    dir (format "reports/%s/" (format-time-string "%Y-%m-%d" kg-due-date))
	    full-dir (expand-file-name dir kitchingroup-personal-root)
	    fname (expand-file-name "weekly-report.org" full-dir))

      ;; make sure the directory exists
      (unless (file-directory-p full-dir)
	(make-directory full-dir t))

      ;; open file if it exists
      (if (file-exists-p fname)
	  (find-file fname)
	;; make file if it doesn't
	(find-file fname)
	(yas-expand-snippet
	 (yas-lookup-snippet "weekly-report")
	 nil nil `((kg-due-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-due-date))
		   (kg-review-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-review-date)))))
      (save-buffer)
      (goto-char (point-min))))))


(defun kitchingroup-calendar ()
  "Open the Kitchin Group Google calendar."
  (interactive)
  (browse-url "https://calendar.google.com/calendar?cid=NXE3aHRwMDZxazk3djltcHBzdHVwdmpkczRAZ3JvdXAuY2FsZW5kYXIuZ29vZ2xlLmNvbQ"))


(defun kitchingroup-mail-archives ()
  "Open the Kitchin group email archive in a browser."
  (interactive)
  (browse-url "https://lists.andrew.cmu.edu/mailman/private/kitchin-group/"))


(defun kitchingroup-send-mail ()
  "Send an email to the Kitchin group email list."
  (interactive)
  (compose-mail "kitchin-group@lists.andrew.cmu.edu")
  (message-goto-subject))


(defun kitchingroup-gitter ()
  "Open the kitchin group gitter in erc.
First get a gitter account. Then go to
https://developer.gitter.im/apps to get your token. Finally, add
this line:

machine gitter.im password here-is-your-token

to ~/.authinfo."
  (interactive)
  (gitter--open-room "kitchingroup/community" "5c2df7f3d73408ce4fb38107"))


(defun kitchingroup-erc ()
  "Open #kitchingroup on erc.
Go to https://irc.gitter.im/ to get your token, and save it in ~/.authinfo like this:
machine irc.gitter.im password your-token
"
  (interactive)
  (require 'erc)
  (let ((password (let* ((plist (car (auth-source-search
				      :max 1 :host "irc.gitter.im")))
			 (k (plist-get plist :secret)))
		    (if (functionp k)
			(funcall k)))))
    (erc-ssl :server "irc.gitter.im"
	     :full-name "John Kitchin"
	     :port 6667
	     :nick "jkitchin"
	     :password password)
    (erc-nickserv-identify password)
    (erc-join-channel "#kitchingroup")))


;; * Old git integration

;; [2019-01-27 Sun] This is harder to get to work than I want; I spend too much
;; time fixing merge conflicts for people. We are switching over to Box.com to
;; see if that works better using Box Drive for syncing.

(defun kitchingroup-submit-weekly ()
  "Submit the weekly report.
This is a convenience method for committing and pushing the
weekly report. It checks to make sure the directory size is below
10MB, and makes a bibliography file if needed. This file exists
so we can have a simple link to click for this action."
  (interactive)
  (message "We do not use this anymore. Please see Prof. Kitchin.")
  ;; (let ((info (org-babel-lob-get-info '(babel-call (:call "kitchingroup-weekly-push")))))
  ;;   (org-babel-execute-src-block nil info))
  )


(defun kitchingroup-pull-weekly ()
  "Submit the weekly report.
This is a convenience method for pulling. This file exists so we
can have a simple link to click for this action."
  (interactive)
  (message "We do not use this anymore. Please see Prof. Kitchin.")
  ;; (let ((info (org-babel-lob-get-info '(babel-call (:call "kitchingroup-weekly-pull")))))
  ;;   (org-babel-execute-src-block nil info))
  )

;; (defun kitchingroup-kitchinhub-repo ()
;;   "Check for existence of your repo and get it if needed."
;;   (when (null kitchingroup-github-id)
;;     (error "`kitchingroup-github-id' is nil. Please set it in %s/user/preload.el"
;; 	   scimax-dir))

;;   (unless (file-directory-p kitchingroup-root)
;;     (make-directory kitchingroup-root t))

;;   (if (not (file-directory-p (expand-file-name kitchingroup-github-id kitchingroup-root)))
;;       ;; get it
;;       (let ((default-directory kitchingroup-root))
;; 	(shell-command (format "git clone git@github.com:KitchinHUB/%s.git"
;; 			       kitchingroup-github-id)))
;;     0))


(provide 'kitchingroup)

;;; kitchingroup.el ends here
