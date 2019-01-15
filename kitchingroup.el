;;; kitchingroup.el --- Kitchingroup utility functions

;;; Commentary:
;; This module is mostly for members of my research group.

(require 'scimax-notebook)

(defcustom kitchingroup-github-id nil
  "Your Github id.
This should be defined in user/preload.el, e.g. (setq kitchingroup-github-id \"your-id\")"
  :group 'kitchingroup)


(when (null kitchingroup-github-id)
  (warn "`kitchingroup-github-id' is nil. Please set it in %s/user/preload.el" scimax-dir))


(defcustom kitchingroup-root (file-name-as-directory
			      (expand-file-name "kitchingroup" nb-notebook-directory))
  "Directory where kitchingroup files will be stored."
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


(defun kitchingroup-kitchinhub-repo ()
  "Check for existence of your repo and get it if needed."
  (when (null kitchingroup-github-id)
    (error "`kitchingroup-github-id' is nil. Please set it in %s/user/preload.el"
	   scimax-dir))

  (unless (file-directory-p kitchingroup-root)
    (make-directory kitchingroup-root t))

  (if (not (file-directory-p (expand-file-name kitchingroup-github-id kitchingroup-root)))
      ;; get it
      (let ((default-directory kitchingroup-root))
	(shell-command (format "git clone git@github.com:KitchinHUB/%s.git"
			       kitchingroup-github-id)))
    0))


(defun iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun kitchingroup-weekly-report ()
  "Open the report for the date prompted.
If you pick a monday, you get the report due on that day. If you
pick any other day, you get the report due on the following
Monday."
  (interactive)
  (let* ((repo-dir (expand-file-name kitchingroup-github-id nb-notebook-directory))
	 (date (org-read-date nil t))
	 kg-due-date
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
	    dir (format "reports/%s/" (format-time-string "%Y-%m-%d" kg-due-date))
	    full-dir (expand-file-name dir repo-dir)
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
	 nil nil `((kg-due-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-due-date)))))
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

	    dir (format "reports/%s/" (format-time-string "%Y-%m-%d" kg-due-date))
	    full-dir (expand-file-name dir repo-dir)
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
	 nil nil `((kg-due-date-string ,(format-time-string "<%Y-%m-%d %a>" kg-due-date)))))
      (save-buffer)
      (goto-char (point-min))))))


(defun kitchingroup-weekly-report ()
  "Create/open this week's progress report.
This function will create a folder called reports/year-mm-dd and
put a weekly-report template inside it, or open the one that
exists. dd will be at the beginning of the week (Monday). The
report is for the previous week."
  (interactive)
  (let* ((dir (format "reports/%s/" (format-time-string "%Y-%m-%d"
							(iso-week-to-time
							 (string-to-number
							  (format-time-string
							   "%Y" (current-time)))
							 (string-to-number
							  (format-time-string
							   "%V" (current-time)))
							 ;; 1 is for Monday
							 1))))
	 (fname (f-join dir "weekly-report.org"))
	 (repo-dir (expand-file-name kitchingroup-github-id nb-notebook-directory))
	 (projectile-switch-project-action (lambda ()
					     (unless (file-directory-p
						      (expand-file-name dir))
					       (make-directory dir t))
					     (if (file-exists-p fname)
						 (find-file fname)
					       (find-file fname)
					       (yas-expand-snippet
						(yas-lookup-snippet "weekly-report"))))))
    ;; check that this exists, and if not get it.
    (unless (file-directory-p (expand-file-name kitchingroup-github-id
						nb-notebook-directory))
      (let ((default-directory nb-notebook-directory))
	(shell-command (format "git clone git@github.com:KitchinHUB/%s.git"
			       kitchingroup-github-id)))
      (projectile-add-known-project repo-dir))

    ;; Now switch to the repo
    (projectile-switch-project-by-name
     repo-dir
     nil)))


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


(use-package gitter)

(defun kitchingroup-gitter ()
  "Open the kitchin group gitter in erc.
First get a gitter account. Then go to https://developer.gitter.im/apps to get your token. Finally, add this line
machine gitter.im password here-is-your-token
to ~/.authinfo"
  (interactive)
  (gitter--open-room "kitchingroup/community" "5c2df7f3d73408ce4fb38107"))

(provide 'kitchingroup)

;;; kitchingroup.el ends here
