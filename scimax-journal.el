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


(defun scimax-journal-git-grep ()
  "Run `counsel-git-grep' on the files in the scimax-journal.
Note this may only work if you set your journal up as a git repo and commit the files to it."
  (interactive)
  (let ((default-directory scimax-journal-root-dir))
    (counsel-git-grep)))


(defun scimax-journal-grep (regex &optional case-sensitive)
  "Run grep on the files in the journal.
 Argument REGEX the pattern to grep for."
  (interactive "sPattern: \nP")
  (let ((default-directory scimax-journal-root-dir))
    (grep (format "grep -nH %s --recursive %s *"
		  (if case-sensitive "" "-i")
		  regex))))


(defun scimax-journal-entries ()
  "Get a list of journal entries.
These are not sorted and rely on the pattern I defined for the file name."
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
  (when-let ((pos (cl-position (buffer-file-name)
			       scimax-journal-entry-filenames :test 'string=))
	     (previous (> pos 0)))
    (find-file (nth (- pos 1) scimax-journal-entry-filenames))))


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
  (let* ((entries (loop for entry in scimax-journal-entry-filenames
			collect
			(org-read-date nil nil (file-name-base entry))))
	 (valid (mapcar (lambda (e)
			  (and
			   (org-time>= e t1)
			   (org-time<= e t2)))
			entries))
	 (decorated (-zip valid
			  scimax-journal-entry-filenames)))
    (loop for (keep . entry) in decorated
	  if keep collect entry)))


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
  "Show an agenda."
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
  (interactive (list (read-input "Search marked files (regexp): ")
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
_sr_: range  _gr_: range   _e_: new          _n_: next      _a_: agenda
_sw_: week   _gw_: week    _h_: heading      _p_: previous
_sm_: month  _gm_: month   _f_: file
_sy_: year   _gy_: year    _j_: journal dir
"
  ("a" scimax-journal-agenda-range "agenda")

  ("n" scimax-journal-next-entry "Next entry" :color red)
  ("p" scimax-journal-previous-entry "Previous entry" :color red)

  ("j" (scimax-journal-open) "Open journal directory")
  ("f" (scimax-journal-go-to-file) "Open a journal file")
  ("e" (scimax-journal-new-entry) "New entry")
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
