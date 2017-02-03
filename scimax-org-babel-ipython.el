(require 'ob-ipython)

(add-to-list 'org-structure-template-alist
	     '("ip" "#+BEGIN_SRC ipython :session :results output drawer\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

(setq org-babel-default-header-args:ipython
      '((:results . "output replace")
	(:session . "none")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")))

(defun scimax-install-ipython-lexer ()
  "Install the IPython lexer for Pygments.
You need this to get syntax highlighting."
  (interactive)
  (unless (= 0
	     (shell-command
	      "python -c \"import pygments.lexers; pygments.lexers.get_lexer_by_name('ipython')\""))
    (shell-command "pip install git+git://github.com/sanguineturtle/pygments-ipython-console")))

;; * Enhancements to ob-ipython

(defun ob-ipython-inline-image (b64-string)
  "Write the b64-string to a file.
Returns an org-link to the file."
  (let* ((f (md5 b64-string))
	 (d "inline")
	 (tfile (or (pop ob-ipython-inline-image-paths)
		    (expand-file-name
		     (concat f ".png")
		     d)))
	 (link (format "[[file:%s]]" tfile)))
    (unless (file-directory-p d)
      (make-directory d)) 
    (ob-ipython--write-base64-string tfile b64-string)
    link))


(defun ob-ipython--async-callback (status &rest args)
  "This function is the callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results."
  (let* ((ret (ob-ipython--eval (if (>= (url-http-parse-response) 400)
				    (ob-ipython--dump-error (buffer-string))
				  (goto-char url-http-end-of-headers)
				  (let ((json-array-type 'list))
				    (json-read)))))
	 (result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 result-type)
    (with-current-buffer *async-ob-ipython*
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "async-abcd-1234-\\(output\\|value\\)" nil t)
	  (setq result-type (buffer-substring (point) (line-beginning-position)))
	  (beginning-of-line)
	  (kill-line)
	  (cond
	   ((s-ends-with? "output" result-type)
	    (insert
	     (concat
	      output
	      (format "%s"
		      (mapconcat
		       'identity
		       (loop for res in result
			     ;; if (and (eq 'text/plain (car res)) (cdr res))
			     ;; collect (cdr res)
			     if (eq 'text/html (car res))
			     collect (format "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
					     (cdr res))
			     if (eq 'text/latex (car res))
			     collect (format "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
					     (cdr res))
			     if (eq 'image/png (car res))
			     collect (ob-ipython-inline-image (cdr res)))
		       "\n")))))
	   ((s-ends-with? "value" result-type) 
	    (insert
	     (cdr (assoc 'text/plain result)))))
	  (org-redisplay-inline-images)
	  (while ob-ipython-inline-image-paths
	    (let ((f (pop ob-ipython-inline-image-paths)))
	      (when (file-exists-p f)
		(delete-file )))))))))


(defun ob-ipython--execute-request-asynchronously (code name)
  "This function makes an asynchronous request.
A callback function replaces the results."
  (let ((url-request-data code)
        (url-request-method "POST"))
    (url-retrieve
     (format "http://%s:%d/execute/%s"
	     ob-ipython-driver-hostname
	     ob-ipython-driver-port
	     name)
     ;; the callback function
     'ob-ipython--async-callback)))


(defvar ob-ipython-inline-image-paths '()
  "List of current file paths in this result.")


;; This overwrites the ob-ipython function and adds better inline image support,
;; and adds async support.
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((file (cdr (assoc :file params)))
         (session (cdr (assoc :session params)))
	 (async (cdr (assoc :async params)))
         (result-type (cdr (assoc :result-type params))))
    (org-babel-ipython-initiate-session session params)

    ;; Check the current results for filenames.
    (let ((location (org-babel-where-is-src-block-result))
	  current-results)
      (when location
	(save-excursion
	  (goto-char location)
	  (when (looking-at (concat org-babel-result-regexp ".*$"))
	    (setq results (buffer-substring-no-properties
			   location
			   (save-excursion 
			     (forward-line 1) (org-babel-result-end)))))))
      (with-temp-buffer
	(insert (or results ""))
	(goto-char (point-min))
	(while (re-search-forward "\\[\\[file:\\(.*?\\)\\]\\]" nil t)
	  (push (match-string 1) ob-ipython-inline-image-paths))))
    
    (if async
	(progn
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "async-abcd-1234" nil t)
	      (error "It looks like an async process is already running.")))
	  (ob-ipython--execute-request-asynchronously
	   (org-babel-expand-body:generic
	    (encode-coding-string body 'utf-8)
	    params (org-babel-variable-assignments:python params))
	   (ob-ipython--normalize-session session))
	  (setq *async-ob-ipython* (current-buffer))
	  (format "async-abcd-1234-%s" result-type))
      
      (-when-let (ret (ob-ipython--eval
		       (ob-ipython--execute-request
			(org-babel-expand-body:generic
			 (encode-coding-string body 'utf-8)
			 params (org-babel-variable-assignments:python params))
			(ob-ipython--normalize-session session))))
	(let ((result (cdr (assoc :result ret)))
	      (output (cdr (assoc :output ret))))
	  (if (eq result-type 'output)
	      (prog1
		  (concat
		   output 
		   (format "%s"
			   (mapconcat 'identity
				      (loop for res in result
					    ;; if (and (eq 'text/plain (car res)) (cdr res))
					    ;; collect (cdr res)
					    if (eq 'text/html (car res))
					    collect (format "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n" (cdr res))
					    if (eq 'text/latex (car res))
					    collect (format "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n" (cdr res))
					    if (eq 'image/png (car res))
					    collect (ob-ipython-inline-image (cdr res)))
				      "\n")))
		(setq  ob-ipython-inline-image-paths '()))
	    (ob-ipython--create-stdout-buffer output)
	    (cond ((and file (string= (f-ext file) "png"))
		   (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file)))
		  ((and file (string= (f-ext file) "svg"))
		   (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file)))
		  (file (error "%s is currently an unsupported file extension." (f-ext file)))
		  (t (->> result (assoc 'text/plain) cdr)))
	    (while ob-ipython-inline-image-paths
	      (let ((f (pop ob-ipython-inline-image-paths)))
		(when (file-exists-p f)
		  (delete-file ))))))))))
