(require 'yasnippet)
(require 'cal-iso)

(defun iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value.
Day 0 = Sun
Day 6 = Sat"
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

(defun scimax-filter-days (year month day-of-week)
  "For a year, filter all the days in MONTH that have DAY-OF-WEEK.
MONTH is a string of the month of interest.
DAY-OF-WEEK is a string for the day of interest.
Returns a list of org time stamps."
  (let* ((d (date-to-time (format "<%s-01-01 nil>" year)))
	 (one-day (seconds-to-time (* 60 60 24)))
	 (months '("January" "February" "March" "April" "May"
		   "June" "July" "August" "September" "October"
		   "November" "December"))
	 (days-of-week '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
	 (month-index (+ 1 (cl-position month months :test 'string=))) ; months start at 1
	 (day-of-week-index (cl-position day-of-week days-of-week :test 'string=))
	 (all-days (cl-loop for i from 1 to 366 do
			    (setq d (time-add d one-day))
			    ;; (seconds minutes hour day month year dow dst utcoff)
			    collect (decode-time d))))
    (mapcar (lambda (dt) (format-time-string "<%Y-%m-%d %a>" (apply 'encode-time dt)))
    	    (-filter (lambda (dt)
    		       (and
    			(= (nth 4 dt) month-index)
    			(= (nth 6 dt) day-of-week-index)))
    		     all-days))))

(defun scimax-get-src-header-val-snippet ()
  "Returns a string for a header value snippet using completion."
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (header-vals (org-babel-combine-header-arg-lists
		       org-babel-common-header-args-w-values
		       (when (boundp lang-headers) (eval lang-headers t))))
	 (header (completing-read "Header: "
				  (mapcar 'symbol-name (mapcar 'car header-vals))))
	 (vals (cdr (assoc (intern-soft header) header-vals)))
	 (val (completing-read "Value: " (if (eq vals :any)
					     '()
					   vals))))
    (format ":${1:%s} ${2:%s} " header val)))


(defun scimax-get-src-block-with-lang-snippet ()
  "Return a string for a src-block with language snippet.
I was not able to use a simple choice in a field for this,
because it seems org was trying to add syntax highlighting to the
block before there was a language defined."
  (let ((lang (completing-read "Lang: " (mapcar 'car org-babel-load-languages))))
    (format "#+BEGIN_SRC %s
$0
#+END_SRC" lang)))


(defun scimax-insert-table-ncolumns ()
  (concat "| $0 " (s-join " " (cl-loop for i below
				       (read-number "N columns: ")
				       collect "| "))))


(defvar scimax-installed-bibliography-styles
  (when (executable-find "kpsewhich")
    (mapcar 'file-name-nondirectory
	    (mapcar 'file-name-sans-extension
		    (-flatten
		     (mapcar (lambda (path)
			       (setq path (replace-regexp-in-string "!" "" path))
			       ;; [2019-08-21 Wed] I added a check to only do
			       ;; absolute paths. BSTINPUTS may contain "."
			       ;; which might be a problem depending on when
			       ;; this variable is defined, e.g. if it is in
			       ;; your home, maybe it would search a lot of
			       ;; directories. I don't see this, but issue #300
			       ;; https://github.com/jkitchin/scimax/issues/300
			       ;; inspired this addition.
			       (when (and (not (file-symlink-p path))
					  (file-name-absolute-p path)
					  (file-directory-p path)
					  ;; you probably do not want to do this
					  ;; in your home path
					  (not (string= (expand-file-name "~/") path)))
				 ;; I am not super sure if this should be
				 ;; recursive. I find more styles when it is
				 ;; recursive.
				 (f-entries path (lambda (f) (f-ext? f "bst")) t)))
			     (split-string
			      ;; https://tex.stackexchange.com/questions/431948/get-a-list-of-installed-bibliography-styles-with-kpsewhich?noredirect=1#comment1082436_431948
			      (shell-command-to-string "kpsewhich -expand-path '$BSTINPUTS'")
			      ":"))))))
  "List of installed bibliography styles.")


(defvar scimax-installed-latex-packages nil
  "List of known installed packages.")

;; We start this async so it probably gets done by the time we need it. This is
;; slow, so we don't want to do it on each time. This approach seems more
;; reliable than looking for sty files using kpsewhich like I did for the
;; bibliography styles
(when (and (null scimax-installed-latex-packages)
	   (executable-find "tlmgr"))
  (require 'async)
  (async-start
   `(lambda ()
      (require 'cl)
      (mapcar
       (lambda (s)
	 (second (split-string (first (split-string s ":")) " ")))
       (cl-loop for line in (process-lines ,(executable-find "tlmgr")  "info" "--only-installed")
		if (and (stringp line) (string= "i" (substring line 0 1)))
		collect line)))

   (lambda (result)
     (setq scimax-installed-latex-packages result))))

(defun scimax-nbgrader-cell-id ()
  "Generate a random 6 letter string."
  (let ((s "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (cl-loop for i to 6 concat (char-to-string (elt  s (random (length s)))))))
