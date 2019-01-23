(require 'yasnippet)
(require 'cal-iso)

(defun iso-week-to-time(year week day)
  "Convert ISO year, week, day to elisp time value."
  (apply #'encode-time
         (append '(0 0 0)
                 (-select-by-indices
                  '(1 0 2)
                  (calendar-gregorian-from-absolute (calendar-iso-to-absolute
                                                     (list week day year)))))))

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
			       (when (file-directory-p path)
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
