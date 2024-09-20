;;; scimax-journal.el --- Journal commands for orgmode

;;; Commentary:
;; This is a set of functions to make journaling in org-mode easier. A journal
;; entry is defined as an org-file in `scimax-journal-root-dir' that follows the
;; name convention year/month/day/year-month-day.org. These entries are saved in
;; an avl-tree to make it easy to navigate them. This data structure should keep
;; the entries sorted by date. The format of these filenames is not flexible at
;; this point.

;; There are many interactive functions, but I consider the main entry point to
;; be a hydra menu bound to `scimax-journal/body'. I suggest you bind this to a
;; convenient key of your choice, perhaps H-j. Then, you have the following actions:
;;
;; H-j j to open a new entry or the current entry
;; H-j e to open an entry from a previous date
;; H-j h to open a heading in the journal
;; H-j f to open a file in the journal (uses projectile)
;;
;; H-j n/p to navigate to the next/previous entries by date.
;;
;; There are a variety of search options. These commands should also take a
;; numeric prefix command to increase the number of units to search, e.g. H-j
;; C-u 2 sw would run swiper on the last two weeks of entries. Swiper is not
;; particularly fast for large time ranges, and requires the buffers to be
;; opened.

;; Swiper options:
;; H-j sr prompts you to pick two dates, and then uses swiper on that range
;; H-j sw uses swiper to search entries in the last week
;; H-j sm uses swiper to search entries in the last month
;; H-j sy uses swiper to search entries in the last year
;;
;; There are also grep versions of those commands, and agenda versions. With the
;; agenda commands you can use all the agenda search capabilities, e.g. tags,
;; keywords, properties, etc. The grep commands tend to be faster than swiper
;; and do not require the buffers to be open.
;;
;; H-j gr and H-j ar on a range
;; H-j gw and H-j aw for the last week
;; H-j gm and H-j gm for the last month
;; H-j gy and H-j ay for the last year
;; H-j ga and H-j aa for all the entries.
;;
;; The journal is registered as a projectile project, so you can also use any
;; functionality from `projectile' to search/find files, etc.
;; `projectile-ag' is another tool for searching.
;;
;; You can make a project specific journal with
;; `scimax-journal-make-directory-local'. This uses directory-local variables.
;;
;;; Code:

(require 'calendar)
(require 'scimax-org)


(defcustom scimax-journal-root-dir "~/vc/journal/"
  "Directory for journal entries.
This is the default, system journal. See
`scimax-journal-make-directory-local' to setup a local, project
journal."
  :group 'scimax-journal
  :type 'directory)


(defcustom scimax-journal-cache ".scimax-journal-cache"
  "File name for the cache.
It will be expanded in `scimax-journal-root-dir'."
  :group 'scimax-journal
  :type 'file)


(defface scimax-journal-calendar-entry-face
  '((t (:foreground "DarkOrange2" :weight bold)))
  "Face for highlighting scimax-journal entries in M-x calendar."
  :group 'scimax-journal)


(defcustom scimax-journal-new-entry-hook
  '()
  "List of functions to run in a new entry.
These functions take no arguments and they are run in the buffer
of the new entry."
  :group 'scimax-journal
  :type '(repeat function))


;; this creates the journal directory and makes it a projectile project. This is
;; useful for searching, opening files, etc.
(unless (file-directory-p scimax-journal-root-dir)
  (let ((dir (file-name-as-directory scimax-journal-root-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-exists-p (expand-file-name ".projectile" dir))
      (let ((default-directory dir))
	(shell-command "touch .projectile")))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)))



(defun scimax-journal-write-cache (tree)
  "Write TREE to cache on disk."
  (with-temp-file (expand-file-name scimax-journal-cache scimax-journal-root-dir)
    (print tree (current-buffer))))


(defun scimax-journal-read-cache ()
  "Read the tree from the cache on disk."
  (hack-dir-local-variables)
  (hack-dir-local-variables-non-file-buffer)
  (unless (file-exists-p (expand-file-name
			  scimax-journal-cache
			  scimax-journal-root-dir))
    (scimax-journal-update-cache))
  (with-temp-buffer
    (hack-dir-local-variables)
    (hack-dir-local-variables-non-file-buffer)
    (insert-file-contents (expand-file-name
			   scimax-journal-cache
			   scimax-journal-root-dir))
    (read (current-buffer))))


(defun scimax-journal-entries ()
  "Return the tree of entries."
  (scimax-journal-read-cache))


(defun scimax-journal-describe ()
  "Describe setup of journal in the current buffer."
  (interactive)
  (let ((entry-list (scimax-journal-get-list-of-entries)))
    (message (s-format "current-directory: ${default-directory}

scimax-journal-root-dir: ${scimax-journal-root-dir}
cache-file: ${cache-file}

Number of entries: ${nentries}
first-entry: ${first-entry}
last-entry: ${last-entry}
"
		       'aget
		       (list (cons 'default-directory default-directory)
			     (cons 'scimax-journal-root-dir scimax-journal-root-dir)
			     (cons 'cache-file (expand-file-name
						scimax-journal-cache
						scimax-journal-root-dir))
			     (cons 'nentries (length entry-list))
			     (cons 'first-entry (car entry-list))
			     (cons 'last-entry (car (last entry-list))))))))


(defun scimax-journal-entries-tree ()
  "Get an avl-tree of journal entries, sorted by the date.
This will loop over all the entries in the journal, so the
results are usually cached."
  (let ((entries (f-entries scimax-journal-root-dir
			    (lambda (f)
			      (and (f-ext? f "org")
				   ;; this is the year-month-day pattern. It is
				   ;; not a sophisticated match
				   (string-match
				    "[0-9]\\{4,\\}-[0-9]\\{2,\\}-[0-9]\\{2,\\}"
				    (file-name-nondirectory f))))
			    t))
	(tree (avl-tree-create (lambda (fname1 fname2)
				 (message "processing %s and %s" fname1 fname2)
				 (time-less-p
				  (org-read-date nil t (file-name-base fname1))
				  (org-read-date nil t (file-name-base fname2)))))))

    (cl-loop for entry in entries
	     do
	     (avl-tree-enter tree entry))
    tree))


(defun scimax-journal-get-list-of-entries (&optional refresh)
  "Return a list of entries in the journal from the cache.
Use an optional prefix arg REFRESH to force refresh the cache."
  (interactive "P")
  (when (or refresh (not (scimax-journal-entries)))
    (scimax-journal-update-cache))
  (avl-tree-flatten (scimax-journal-entries)))


(defun scimax-journal-update-cache ()
  "Update the cache with the output of the function `scimax-journal-entries'."
  (interactive)
  (scimax-journal-write-cache (scimax-journal-entries-tree)))


(defvar scimax-journal-tree)

(defun scimax-journal-mark-entries ()
  "Mark entries in a calendar when there are journal entries."
  ;; scimax-journal-tree is lexically bound. Otherwise, you don't get the right
  ;; journal when it is locally bound.
  (cl-loop for fname in (avl-tree-flatten scimax-journal-tree)
	   do
	   (let* ((bf (split-string (file-name-base fname) "-"))
		  (year (nth 0 bf))
		  (month (nth 1 bf))
		  (day (nth 2 bf))
		  (d (mapcar 'string-to-number (list month day year))))
	     (when (calendar-date-is-visible-p d)
	       (calendar-mark-visible-date
		d
		'scimax-journal-calendar-entry-face)))))


(defun scimax-journal-open-entry (date-string global)
  "Add new entry to journal for DATE-STRING.
DATE-STRING should be in the form \"year-month-day\". Add new day
if necessary, otherwise, add to current day. if GLOBAL is non-nil
open in the default journal even if there is a local journal."
  (interactive (list
		(let ((scimax-journal-tree (scimax-journal-entries))
		      (calendar-today-visible-hook))
		  (progn
		    (add-hook 'calendar-today-visible-hook
			      'scimax-journal-mark-entries)
		    (org-read-date)))
		current-prefix-arg))
  (hack-dir-local-variables)
  (hack-dir-local-variables-non-file-buffer)
  (let* ((date (split-string date-string "-"))
	 (year (elt date 0))
	 (month (elt date 1))
	 (day (elt date 2))
	 (journal-entry-dir (f-join
			     (if global
				 (default-value 'scimax-journal-root-dir)
			       scimax-journal-root-dir)
			     year
			     month
			     day))
	 (org-file (f-join journal-entry-dir
			   (format "%s-%s-%s.org" year month day)))
	 ;; we only run hooks on new files. If the file exists, we do not want
	 ;; to run hooks.
	 (run-hooks (not (file-exists-p org-file)))
	 (tree (scimax-journal-entries)))

    (when (not (file-directory-p journal-entry-dir))
      (mkdir journal-entry-dir t))

    (find-file org-file)

    (unless (avl-tree-member tree org-file)
      (avl-tree-enter tree org-file)
      ;; this is to make sure we save the new entry
      (scimax-journal-write-cache tree))

    (when run-hooks
      (run-hooks 'scimax-journal-new-entry-hook))

    (when (local-variable-p 'scimax-journal-root-dir)
      (setq header-line-format (format "local in %s" scimax-journal-root-dir)))))


(defun scimax-journal-delete-entry ()
  "Delete the entry and file associated with the buffer."
  (interactive)
  (let* ((fname (buffer-file-name))
	 (tree (scimax-journal-entries)))
    (if (not (avl-tree-member tree fname))
	(message "%s doesn't seem to be a journal file. Not deleting.")
      (when (y-or-n-p (format "Really delete %s? " fname)))
      ;; Now we delete it.
      (avl-tree-delete tree fname)
      (scimax-journal-write-cache tree)
      (kill-buffer)
      (delete-file fname))))


(defun scimax-journal-open ()
  "Open the `scimax-journal-root-dir'."
  (interactive)
  (find-file scimax-journal-root-dir))


(defun scimax-journal-go-to-file ()
  "Open a file in the `scimax-journal-root-dir'."
  (interactive)
  (projectile-find-file-in-directory scimax-journal-root-dir))


(defun scimax-journal-open-heading ()
  "Open a heading in a scimax-journal file.
Slow when you have a large journal or many files."
  (interactive)
  (let ((default-directory scimax-journal-root-dir))
    ;; Note this next function is in scimax-org.
    (ivy-org-jump-to-heading-in-directory t)))


;; * Entry navigation

(defun scimax-journal-next-entry ()
  "Go to next entry (by date) after the one you are in."
  (interactive)
  (let* ((entries (avl-tree-flatten (scimax-journal-entries)))
	 (n (length entries))
	 (i (cl-position (buffer-file-name)  entries
			 :test (lambda (item entry) (string= item entry))))
	 (next (when i
		 (nth (min (cl-incf i) n) entries))))
    (if next
	(find-file next)
      ;; If you aren't in an entry there is no match so we just go to the last entry.
      (find-file (car (last entries))))))


(defun scimax-journal-previous-entry ()
  "Go to previous entry (by date)from the one you are in."
  (interactive)
  (let* ((entries (avl-tree-flatten (scimax-journal-entries)))
	 (i (cl-position (buffer-file-name)  entries
			 :test (lambda (item entry) (string= item entry))))
	 (prev (when i (nth (max (cl-decf i) 0) entries))))
    (if prev
	(find-file prev)
      ;; If you aren't in an entry there is no match so we just go to the second
      ;; to last entry.
      (find-file (car (last (butlast entries)))))))


;; * Refiling an entry

(defun scimax-journal-refile-entry ()
  "Refile a heading to another place in the journal.
This is usually for moving an entry from the past to the future.
This is faster than trying to do it through the agenda. I don't
use this a lot, but Shreyas thought it would be helpful."
  (interactive)
  (let* ((refile-file (completing-read
		       "Choose file: "
		       (reverse
			(scimax-journal-get-list-of-entries))))
	 (org-agenda-files (list refile-file))
	 (org-refile-targets '((org-agenda-files :maxlevel . 3))))
    (org-refile)))



;; * Search functions

(defun scimax-journal-grep (regex &optional case-sensitive)
  "Run grep on all the files in `scimax-journal-root-dir'.
Argument REGEX the pattern to grep for.
CASE-SENSITIVE is optional to make the search case sensitive if non-nil."
  (interactive "sPattern: \nP")
  (let ((default-directory scimax-journal-root-dir))
    (grep (format "grep -nH %s --recursive %s *"
		  (if case-sensitive "" "-i")
		  regex))))


(defun scimax-journal-get-entries (t1 t2)
  "Return a list of entry files between T1 and T2.
T1 and T2 are org-dates in string form."
  ;; Make sure t1 is less than t2
  (when (org-time> t1 t2)
    (let ((a t1)
	  (b t2))
      (setq t1 b
	    t2 a)))
  (cl-loop for entry in (avl-tree-flatten (scimax-journal-entries))
	   if (and (org-time>= (file-name-base entry) t1)
		   (org-time>= t2 (file-name-base entry)))
	   collect
	   entry))


(defun scimax-journal-deadgrep (query)
  "Search journal with `deadgrep'."
  (interactive (list (if (region-active-p)
			 (buffer-substring (region-beginning) (region-end))
		       (read-string "Query: "))))
  (let ((default-directory  scimax-journal-root-dir))
    (deadgrep query)))


;; ** Swiper searches
;; These can be slow.

(defun scimax-journal-swiper-range (t1 t2)
  "Run Swiper on entries between T1 and T2."
  (interactive (list
		(org-read-date nil nil nil "First date: ")
		(org-read-date nil nil nil "Second date: ")))
  (let* ((swiper-multi-buffers nil)
	 (swiper-multi-candidates nil)
	 (this-command 'ivy-done))
    (mapc 'swiper-multi-action-1 (mapcar
				  (lambda (f)
				    (buffer-name (find-file-noselect f)))
				  (scimax-journal-get-entries
				   (org-read-date nil nil t1)
				   (org-read-date nil nil t2))))

    (ivy-read "Swiper: " swiper-multi-candidates
    	      :action #'swiper-multi-action-2
    	      :unwind #'swiper--cleanup
    	      :caller 'swiper-multi)))


(defun scimax-journal-swiper-last-week (&optional n)
  "Run swiper on entries from the last week.
Use a numeric prefix arg N to increase the number of weeks."
  (interactive "p")
  (scimax-journal-swiper-range (format "-%sw" n) "today"))


(defun scimax-journal-swiper-last-month (&optional n)
  "Run swiper on entries from the last month.
Use a numeric prefix arg N to increase the number of months."
  (interactive "p")
  (scimax-journal-swiper-range (format "-%sm" n) "today"))


(defun scimax-journal-swiper-last-year ()
  "Run swiper on entries from the last year.
This may be very slow."
  (interactive)
  (scimax-journal-swiper-range "-1y" "today"))


;; ** Agenda searches

(defun scimax-journal-agenda-range (t1 t2)
  "Show an agenda for journal entries for the range T1 to T2."
  (interactive (list
		(org-read-date nil nil nil "First date: ")
		(org-read-date nil nil nil "Second date: ")))
  (let ((org-agenda-files (scimax-journal-get-entries
			   (org-read-date nil nil t1)
			   (org-read-date nil nil t2))))
    (org-agenda)))


;; TODO
(defun scimax-journal-agenda ()
  "Open an ‘org-agenda’ for journal entries.
This may be slow for large journals, and will result in all the
entries getting opened."
  (interactive)
  (let ((org-agenda-files (scimax-journal-get-list-of-entries)))
    (org-agenda)))


(defun scimax-journal-agenda-last-week (&optional n)
  "Show an agenda for journal entries for the last week.
Use a numeric prefix arg N to go N weeks back (Default=1)."
  (interactive "p")
  (scimax-journal-agenda-range (format "-%sw" n) "today"))


(defun scimax-journal-agenda-last-month (&optional n)
  "Show an agenda for journal entries for the last month.
Use a numeric prefix arg N to go N months back (Default=1)."
  (interactive "p")
  (scimax-journal-agenda-range (format "-%sm" n) "today"))


(defun scimax-journal-agenda-last-year ()
  "Show an agenda for journal entries for the last year."
  (interactive)
  (scimax-journal-agenda-range "-1y" "today"))

;; ** grep regexp searching

(defun scimax-journal-find-regexp-range (regexp t1 t2)
  "Find all match for REGEXP in journal entries between T1 and T2.
Adapted from `dired-do-find-regexp'.

REGEXP should use constructs supported by your local `grep' command."
  (interactive (list (read-string "Search marked files (regexp): ")
		     (org-read-date)
		     (org-read-date)))
  (require 'grep)
  (defvar grep-find-ignored-files)
  (defvar grep-find-ignored-directories)
  (let* ((files (scimax-journal-get-entries
		 (org-read-date nil nil t1)
		 (org-read-date nil nil t2)))
         (ignores (nconc (mapcar
                          (lambda (s) (concat s "/"))
                          grep-find-ignored-directories)
                         grep-find-ignored-files))
         (xrefs (cl-mapcan
                 (lambda (file)
                   (xref-matches-in-directory regexp "*" file
					      (and (file-directory-p file)
						   ignores)))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil)))


(defun scimax-journal-find-regexp-last-week (regexp)
  "Grep for REGEXP in journal entries for the last week."
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1w")
   (org-read-date nil nil "today")))


(defun scimax-journal-find-regexp-last-month (regexp)
  "Grep for REGEXP in journal entries for the last month."
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1m")
   (org-read-date nil nil "today")))


(defun scimax-journal-find-regexp-last-year (regexp)
  "Grep for REGEXP in journal entries for the last year."
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1y")
   (org-read-date nil nil "today")))


;; * Directory local journals
;; To have a project specific journal, we have to use directory local variables.
(defun scimax-journal-make-directory-local (journal-root-name)
  "Setup directory local variables so you can have a project specific journal.
Argument JOURNAL-ROOT-NAME Name of journal directory."
  (interactive (list (read-string "Journal root name:" "journal")))
  (let* ((project-root (projectile-project-root))
	 (journal-root (file-name-as-directory (f-join project-root journal-root-name)))
	 (default-directory project-root))
    (add-dir-local-variable nil 'scimax-journal-root-dir journal-root)
    (unless (file-directory-p journal-root)
      (make-directory journal-root t))
    (scimax-journal-update-cache)
    ;; The last two commands leave .dir-locals.el open. We save and close it next.
    (save-buffer)
    (kill-buffer)
    (hack-dir-local-variables)))


;; * Hydra for scimax journal
(defhydra scimax-journal (:color blue :hint nil)
  "
Scimax-Journal"
  ("sr" scimax-journal-swiper-range  "Swiper date range" :column "Swiper")
  ("sw" scimax-journal-swiper-last-week "Swiper last week" :column "Swiper")
  ("sm" scimax-journal-swiper-last-month "Swiper last month" :column "Swiper")
  ("sy" scimax-journal-swiper-last-year "Swiper last year" :column "Swiper")

  ("gg" scimax-journal-grep "grep journal" :column "Grep")
  ("gd" scimax-journal-deadgrep "grep journal" :column "Grep")
  ("gw" scimax-journal-find-regexp-last-week "grep last week" :column "Grep")
  ("gm" scimax-journal-find-regexp-last-month "grep last month" :column "Grep")
  ("gy" scimax-journal-find-regexp-last-year "grep last year" :column "Grep")
  ("gr" scimax-journal-find-regexp-range "grep a date range" :column "Grep")

  ("j" (scimax-journal-open-entry (format-time-string "%Y-%m-%d" (current-time)) current-prefix-arg) "Open today" :column "Open")
  ("f" (scimax-journal-go-to-file) "Open a journal file" :column "Open")
  ("e" scimax-journal-open-entry "Open entry" :column "Open")
  ("h" scimax-journal-open-heading "Open to heading" :column "Open")
  ("d" scimax-journal-delete-entry "Delete entry" :column "Open")

  ("n" scimax-journal-next-entry "Next entry" :color red :column "Navigation")
  ("p" scimax-journal-previous-entry "Previous entry" :color red :column "Navigation")

  ("aa" scimax-journal-agenda "agenda" :column "Agenda")
  ("ar" scimax-journal-agenda-range "agenda range" :column "Agenda")
  ("aw" scimax-journal-agenda-last-week "agenda last week"  :column "Agenda")
  ("am" scimax-journal-agenda-last-month "agenda last month"  :column "Agenda")
  ("ay" scimax-journal-agenda-last-year "agenda last year"  :column "Agenda"))




(provide 'scimax-journal)

;;; scimax-journal.el ends here
