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


(require 'scimax-org)
(use-package pcache)

(defvar scimax-journal-root-dir "~/vc/journal"
  "Directory for journal entries.")

(defcustom scimax-journal-new-entry-hook
  '()
  "List of functions to run in a new entry.")


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


(require 'calendar)

(defface scimax-journal-calendar-entry-face
  '((t (:foreground "DarkOrange2" :weight bold)))
  "Face for highlighting scimax-journal entries in M-x calendar."
  :group 'scimax-journal)


(defun scimax-journal-mark-entries ()
  "Mark entries in a calendar when there are journal entries."
  (loop for (fname . time) in (pcache-get scimax-journal-entries 'entries)
	do
	(let* ((bf (split-string (file-name-base fname) "-"))
	       (year (nth 0 bf))
	       (month (nth 1 bf))
	       (day (nth 2 bf))
	       (d (mapcar 'string-to-number (list month day year))))
	  (when (calendar-date-is-visible-p d)
	    (calendar-mark-visible-date d 'scimax-journal-calendar-entry-face)))))


(defun scimax-journal-open-entry (date-string)
  "Add new entry to journal for DATE.
DATE should be in the form \"year-month-day\".
Add new day if necessary, otherwise, add to current day."
  (interactive (list
		(let ((calendar-today-visible-hook))
		  (progn
		    (add-hook 'calendar-today-visible-hook 'scimax-journal-mark-entries)
		    (org-read-date)))))
  (let* ((date (split-string date-string "-"))
	 (year (elt date 0))
	 (month (elt date 1))
	 (day (elt date 2))
	 (journal-entry-dir (f-join scimax-journal-root-dir
				    year
				    month
				    day))
	 (org-file (f-join journal-entry-dir
			   (format "%s-%s-%s.org" year month day)))
	 (run-hooks (file-exists-p org-file))
	 (tree (pcache-get scimax-journal-entries 'entries)))

    (when (not (file-directory-p journal-entry-dir))
      (mkdir journal-entry-dir t))

    (find-file org-file)

    (when run-hooks
      (run-hooks 'scimax-journal-new-entry-hook))

    (unless (avl-tree-member tree org-file)
      (avl-tree-enter tree org-file))))


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
    (ivy-org-jump-to-heading-in-directory t)))


;; (defun scimax-journal-git-grep ()
;;   "Run `counsel-git-grep' on the files in the scimax-journal.
;; Note this may only work if you set your journal up as a git repo and commit the files to it."
;;   (interactive)
;;   (let ((default-directory scimax-journal-root-dir))
;;     (counsel-git-grep)))


(defun scimax-journal-grep (regex &optional case-sensitive)
  "Run grep on the files in the journal.
 Argument REGEX the pattern to grep for."
  (interactive "sPattern: \nP")
  (let ((default-directory scimax-journal-root-dir))
    (grep (format "grep -nH %s --recursive %s *"
		  (if case-sensitive "" "-i")
		  regex))))


(defun scimax-journal-entries ()
  "Get an avl-tree of journal entries, sorted by the date."
  (let ((entries (f-entries scimax-journal-root-dir
			    (lambda (f)
			      (and (f-ext? f "org")
				   (string-match "[0-9]\\{4,\\}-[0-9]\\{2,\\}-[0-9]\\{2,\\}"
						 (file-name-nondirectory f))))
			    t))
	(tree (avl-tree-create (lambda (fname1 fname2)
				 (time-less-p
				  (org-read-date nil t  (file-name-base fname1))
				  (org-read-date nil t  (file-name-base fname2)))))))

    (loop for entry in entries
	  do
	  (avl-tree-enter tree entry))
    tree))


(defvar scimax-journal-entries (pcache-repository "scimax-journal")
  "Persistent cache to store entries.")


(unless (pcache-get scimax-journal-entries 'entries)
  (pcache-put scimax-journal-entries 'entries (scimax-journal-entries)))


(defun scimax-journal-next-entry ()
  (interactive)
  (let* ((entries (avl-tree-flatten (pcache-get scimax-journal-entries 'entries)))
	 (n (length entries))
	 (i (cl-position (buffer-file-name)  entries
			 :test (lambda (item entry) (string= item entry)))))
    (when (nth (min (incf i) n) entries)
      (find-file (nth (min (incf i) n) entries)))))


(defun scimax-journal-previous-entry ()
  "Go to next entry."
  (interactive)
  (let* ((entries (avl-tree-flatten (pcache-get scimax-journal-entries 'entries)))
	 (i (cl-position (buffer-file-name)  entries
			 :test (lambda (item entry) (string= item entry)))))
    (when (nth (max (decf i) 0) entries)
      (find-file (nth (max (decf i) 0) entries)))))


;; * Search functions
(defun scimax-journal-get-entries (t1 t2)
  "Return a list of entry files between T1 and T2.
T1 and T2 are org-dates in string form."
  ;; Make sure t1 is less than t2
  (when (org-time> t1 t2)
    (let ((a t1)
	  (b t2))
      (setq t1 b
	    t2 a)))
  (loop for entry in (avl-tree-flatten (pcache-get scimax-journal-entries 'entries))
	if (and (org-time> (file-name-base entry) t1)
		(org-time> t2 (file-name-base entry)))
	collect
	entry))


;; (defun scimax-journal-grep-range (t1 t2 regexp &optional case-sensitive)
;;   "Search the journal from T1 to T2 for REGEXP.
;; T1 and T2 are strings as selected from `org-read-date'
;; With a prefix arg, make it CASE-SENSTIVE."
;;   (interactive (list
;; 		(org-read-date)
;; 		(org-read-date)
;; 		(read-input "Search for: ")
;; 		current-prefix-arg))
;;   (message "Searching for %s in %S" regexp (scimax-journal-get-entries t1 t2))
;;   (pop-to-buffer (grep
;; 		  (format "grep -nH %s %s %s"
;; 			  (if (null case-sensitive) "-i" "")
;; 			  regexp
;; 			  (s-join " " (scimax-journal-get-entries t1 t2))))))


;; (defun scimax-journal-grep-last-year (regexp &optional case-sensitive)
;;   "Search the last year of entries."
;;   (interactive (list (read-input "Search for: ")
;; 		     current-prefix-arg))
;;   (scimax-journal-grep-range (org-read-date nil nil "-1y")
;; 			     (org-read-date nil nil "today")
;; 			     regexp case-sensitive))


;; (defun scimax-journal-grep-last-month (regexp &optional case-sensitive)
;;   "Search the last month of entries."
;;   (interactive (list (read-input "Search for: ")
;; 		     current-prefix-arg))
;;   (scimax-journal-grep-range (org-read-date nil nil "-1m")
;; 			     (org-read-date nil nil "today")
;; 			     regexp case-sensitive))


;; (defun scimax-journal-grep-last-week (regexp &optional case-sensitive)
;;   "Search the last month of entries."
;;   (interactive (list (read-input "Search for: ")
;; 		     current-prefix-arg))
;;   (scimax-journal-grep-range (org-read-date nil nil "-1w")
;; 			     (org-read-date nil nil "today")
;; 			     regexp case-sensitive))

;; ** Swiper searches
;; These can be slow.

(defun scimax-journal-swiper-range (t1 t2)
  "Run Swiper on entries between T1 and T2."
  (interactive (list
		(org-read-date)
		(org-read-date)))
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


(defun scimax-journal-swiper-last-week ()
  (interactive)
  (scimax-journal-swiper-range "-1w" "today"))


(defun scimax-journal-swiper-last-month ()
  (interactive)
  (scimax-journal-swiper-range "-1m" "today"))


(defun scimax-journal-swiper-last-year ()
  (interactive)
  (scimax-journal-swiper-range "-1y" "today"))

(defun scimax-journal-agenda-range (t1 t2)
  "Show an agenda for the range T1 to T2."
  (interactive (list
		(org-read-date)
		(org-read-date)))
  (let ((org-agenda-files (scimax-journal-get-entries
			   (org-read-date nil nil t1)
			   (org-read-date nil nil t2))))
    (org-agenda)))

;; ** grep regexp searching

(defun scimax-journal-find-regexp-range (regexp t1 t2)
  "Find all matches for REGEXP in journal entries between T1 and T2.
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
                   (xref-collect-matches regexp "*" file
                                         (and (file-directory-p file)
                                              ignores)))
                 files)))
    (unless xrefs
      (user-error "No matches for: %s" regexp))
    (xref--show-xrefs xrefs nil t)))

(defun scimax-journal-find-regexp-last-week (regexp)
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1w")
   (org-read-date nil nil "today")))


(defun scimax-journal-find-regexp-last-month (regexp)
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1m")
   (org-read-date nil nil "today")))


(defun scimax-journal-find-regexp-last-year (regexp)
  (interactive "sSearch marked files (regexp): ")
  (scimax-journal-find-regexp-range
   regexp
   (org-read-date nil nil "-1y")
   (org-read-date nil nil "today")))


;; * Hydra for scimax journal
(defhydra scimax-journal (:color blue :hint nil)
  "
Scimax-Journal
Swiper       Grep          Open              Navigate       Agenda
---------------------------------------------------------------------
_sr_: range  _gr_: range   _j_: today        _n_: next      _a_: agenda
_sw_: week   _gw_: week    _e_: entry        _p_: previous
_sm_: month  _gm_: month   _h_: heading
_sy_: year   _gy_: year    _f_: file
"
  ("a" scimax-journal-agenda-range "agenda")

  ("n" scimax-journal-next-entry "Next entry" :color red)
  ("p" scimax-journal-previous-entry "Previous entry" :color red)

  ("j" (scimax-journal-open-entry (org-read-date nil nil "today")) "Open today")
  ("f" (scimax-journal-go-to-file) "Open a journal file")
  ("e" scimax-journal-open-entry "Open entry")
  ("h" scimax-journal-open-heading "Open to heading")

  ("sr" scimax-journal-swiper-last-week "Swiper date range")
  ("sw" scimax-journal-swiper-last-week "Swiper last week")
  ("sm" scimax-journal-swiper-last-month "Swiper last month")
  ("sy" scimax-journal-swiper-last-year "Swiper last year")

  ("gg" scimax-journal-grep "grep journal")
  ("gw" scimax-journal-find-regexp-last-week "grep last week")
  ("gm" scimax-journal-find-regexp-last-month "grep last month")
  ("gy" scimax-journal-find-regexp-last-year "grep last year")
  ("gr" scimax-journal-find-regexp-range "grep a date range"))

(provide 'scimax-journal)

;;; scimax-journal.el ends here
