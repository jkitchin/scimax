;;; scimax-journal.el --- Journal commands for orgmode

;;; Commentary:
;; This is a set of functions to make journaling in org-mode easier. The main
;; entry point is a hydra menu bound to `scimax-journal/body'. I suggest you bind this
;; to a convenient key of your choice, perhaps H-j.
;;
;; The hydra menu provides easy access to the following functions:
;;
;; `scimax-journal-new-entry' which creates a new entry for the day, or opens the current day.
;;
;; `scimax-journal-open' which just opens the journal in the main root directory
;; defined by `scimax-journal-root-dir'.
;;
;; `scimax-journal-open-heading' to open a heading in the journal
;;
;; `scimax-journal-git-grep' search the journal using `counsel-git-grep
;;
;;; Code:
(when (featurep 'rg)
  (require 'rg))
(require 'scimax-org)

(defvar scimax-journal-root-dir "~/vc/journal"
  "Directory for journal entries.")

(defcustom scimax-journal-new-entry-hook
  '()
  "List of functions to run in a new entry.")


(unless (file-directory-p scimax-journal-root-dir)
  (let ((dir (file-name-as-directory scimax-journal-root-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-directory-p (file-name-as-directory (expand-file-name ".git" dir)))
      (let ((default-directory dir))
	(shell-command "git init")))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)))


(defun scimax-journal-new-entry ()
  "Add new entry to journal.
Add new day if necessary, otherwise, add to current day."
  (interactive)
  (let* ((date (calendar-current-date))
	 (year (elt date 2))
	 (month (elt date 0))
	 (day (elt date 1))
	 (journal-entry-dir (f-join scimax-journal-root-dir
				    (number-to-string year)
				    (format "%02d" month)
				    (format "%02d" day)))
	 (org-file (f-join journal-entry-dir
			   (format "%s-%02d-%02d.org" year month day)))
	 (run-hooks (file-exists-p org-file)))

    (when (not (file-directory-p journal-entry-dir))
      (mkdir journal-entry-dir t))

    (find-file org-file)
    (when run-hooks
      (run-hooks 'scimax-journal-new-entry-hook))))


(defun scimax-journal-open ()
  "Open the `scimax-journal-root-dir'."
  (interactive)
  (find-file scimax-journal-root-dir))


(defun scimax-journal-go-to-file ()
  "Open a file in the `scimax-journal-root-dir'."
  (interactive)
  (projectile-find-file-in-directory scimax-journal-root-dir))


(defun scimax-journal-open-heading ()
  "Open a heading in a scimax-journal file."
  (interactive)
  (let ((default-directory scimax-journal-root-dir))
    (ivy-org-jump-to-heading-in-directory t)))


(defun scimax-journal-git-grep (regex)
  "Run grep on the files in the scimax-journal.
Argument REGEX the pattern to grep for."
  (let ((default-directory scimax-journal-root-dir))
    (counsel-git-grep)))


(defun scimax-journal-grep (regex)
  "Run grep on the files in the journal.
 Argument REGEX the pattern to grep for."
  (interactive "sPattern: ")
  (let ((default-directory scimax-journal-root-dir))
    (cond
     ((featurep 'rg)
      (rg regex "*.org" scimax-journal-root-dir))
     (t
      (projectile-grep regex)))))


(defun scimax-journal-entries ()
  "Get a list of journal entries.
These are not sorted."
  (f-entries scimax-journal-root-dir
	     (lambda (f)
	       (and (f-ext? f "org")
		    (string-match "[0-9]\\{4,\\}-[0-9]\\{2,\\}-[0-9]\\{2,\\}"
				  (file-name-nondirectory f))))
	     t))


(defvar scimax-journal-entry-filenames
  (scimax-journal-entries)
  "List of known journal entries.")


(defun scimax-journal-next-entry (&optional refresh)
  (interactive "P")
  (when refresh
    (setq scimax-journal-entry-filenames (scimax-journal-entries)))
  (when-let ((pos (cl-position (buffer-file-name) scimax-journal-entry-filenames :test 'string=))
	     (next (< pos (length scimax-journal-entry-filenames))))
    (find-file (nth (+ 1 pos) scimax-journal-entry-filenames))))


(defun scimax-journal-previous-entry (&optional refresh)
  (interactive "P")
  (when refresh
    (setq scimax-journal-entry-filenames (scimax-journal-entries)))
  (when-let ((pos (cl-position (buffer-file-name) scimax-journal-entry-filenames :test 'string=))
	     (previous (> pos 0)))
    (find-file (nth (- pos 1) scimax-journal-entry-filenames))))


(defhydra scimax-journal (:color blue)
  "Scimax-Journal"
  ("n" scimax-journal-next-entry "Next entry" :color red)
  ("p" scimax-journal-previous-entry "Previous entry" :color red)
  ("j" (scimax-journal-open) "Open journal")
  ("o" (scimax-journal-go-to-file) "Open a journal file")
  ("e" (scimax-journal-new-entry) "New entry")
  ("g" scimax-journal-grep "grep journal")
  ("h" scimax-journal-open-heading "Open to heading"))

(provide 'scimax-journal)

;;; scimax-journal.el ends here
