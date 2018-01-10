;;; scimax-journal.el --- Journal commands for orgmode

;;; Commentary:
;; This is a set of functions to make journaling in org-mode easier. The main
;; entry point is a hydra menu bound to `journal/body'. I suggest you bind this
;; to a convenient key of your choice, perhaps H-j.
;;
;; The hydra menu provides easy access to the following functions:
;; 
;; `journal-new-entry' which creates a new entry for the day, or opens the current day.
;; 
;; `journal-open' which just opens the journal in the main root directory
;; defined by `journal-root-dir'.
;;
;; `journal-open-heading' to open a heading in the journal
;;
;; `journal-git-grep' search the journal using `counsel-git-grep
;; 
;;; Code:
(require 'rg)
(require 'scimax-org)

(defvar journal-root-dir "~/vc/journal"
  "Directory for journal entries.")

(unless (file-directory-p journal-root-dir)
  (let ((dir (file-name-as-directory journal-root-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-directory-p (file-name-as-directory (expand-file-name ".git" dir)))
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


(defun journal-git-grep (regex)
  "Run grep on the files in the journal.
Argument REGEX the pattern to grep for."
  (let ((default-directory journal-root-dir)) 
    (counsel-git-grep)))


(defun journal-grep (regex)
  "Run grep on the files in the journal.
 Argument REGEX the pattern to grep for."
  (interactive "sPattern: ")
  (let ((default-directory journal-root-dir))
    (cond
     ((featurep 'rg)
      (rg regex "*.org" journal-root-dir))
     (t
      (projectile-grep regex)))))


(defhydra journal (:color blue)
  "Journal"
  ("j" (journal-open) "Open journal")
  ("f" (journal-go-to-file) "Open a journal file")
  ("n" (journal-new-entry) "New entry")
  ("g" journal-grep "grep journal")
  ("h" journal-open-heading "Open to heading"))

(provide 'scimax-journal)

;;; scimax-journal.el ends here
