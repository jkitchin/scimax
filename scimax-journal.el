;;; scimax-journal.el --- Journal commands for orgmode
;; * Journal

;;; Commentary:
;; 

;;; Code:

(defvar journal-root-dir "~/vc/journal"
  "Directory for journal entries.")

(unless (file-directory-p journal-root-dir)
  (let ((dir (file-name-as-directory journal-root-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (let ((default-directory dir))
	(shell-command "git init")))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)))


(defun journal-new-entry ()
  "Add new entry to journal.
Add new day if necessary, otherwise, add to current day."
  (interactive)
  (let* ((date (calendar-current-date))
	 (year (elt date 2))
	 (month (elt date 0))
	 (day (elt date 1))
	 (org-file (f-join journal-root-dir
			   (number-to-string year)
			   (format "%02d" month)
			   (format "%02d" day)
			   (format "%s-%02d-%02d.org" year month day))))
    (mkdir (f-join journal-root-dir
		   (number-to-string year)
		   (format "%02d" month)
		   (format "%02d" day))
	   t)
    (find-file org-file)))


(defun journal-open ()
  "Open the `journal-root-dir'."
  (interactive)
  (find-file journal-root-dir))


(defun journal-go-to-file ()
  "Open a file in the `journal-root-dir'."
  (interactive)
  (projectile-find-file-in-directory journal-root-dir))


(defun journal-open-heading ()
  "Open a heading in a journal file."
  (interactive)
  (let ((default-directory journal-root-dir))
    (ivy-org-jump-to-heading-in-directory t)))


(defun journal-grep (regex)
  "Run grep on the files in the journal.
Argument REGEX the pattern to grep for."
  (interactive "sPattern: ")
  (rg regex "*.org" journal-root-dir))


(defun journal-heading ()
  "Jump to a heading in the journal."
  (interactive)
  (let ((default-directory journal-root-dir))
    (ivy-org-jump-to-heading-in-directory t)))


(defhydra journal (:color blue)
  "Journal"
  ("j" (journal-open) "Open journal")
  ("f" (journal-go-to-file) "Open a journal file")
  ("n" (journal-new-entry) "New entry")
  ("g" journal-grep "grep journal")
  ("h" journal-open-heading "Open to heading"))

(provide 'scimax-journal)

;;; scimax-journal.el ends here
