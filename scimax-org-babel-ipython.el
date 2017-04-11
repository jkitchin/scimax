;;; scimax-org-babel-ipython.el --- Scimax enhancements to ob-ipython

;;; Commentary:
;; 

(require 'ob-ipython)

;;; Code:

(add-to-list 'org-structure-template-alist
	     '("ip" "#+BEGIN_SRC ipython :session :results output drawer\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

(setq org-babel-default-header-args:ipython
      '((:results . "output replace drawer")
	(:session . t)
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))

(defun scimax-install-ipython-lexer ()
  "Install the IPython lexer for Pygments.
You need this to get syntax highlighting."
  (interactive)
  (unless (= 0
	     (shell-command
	      "python -c \"import pygments.lexers; pygments.lexers.get_lexer_by_name('ipython')\""))
    (shell-command "pip install git+git://github.com/sanguineturtle/pygments-ipython-console")))

;; * Commands like the jupyter notebook

(defun org-babel-insert-block (&optional below)
  "Insert a src block above the current point.
With prefix arg BELOW, insert it below the current point."
  (interactive "P")
  (cond
   ((and (org-in-src-block-p) below)
    ;; go to end, and insert block
    (let* ((src (org-element-context))
	   (start (org-element-property :begin src))
	   (end (org-element-property :end src))
	   location)
      (goto-char start)
      (setq location (org-babel-where-is-src-block-result nil nil))
      (if (not  location)
	  (goto-char end)
	(goto-char location)
	(goto-char (org-element-property :end (org-element-context))))
      (insert "\n#+BEGIN_SRC ipython

#+END_SRC\n\n")
      (forward-line -3)))

   ((org-in-src-block-p)
    ;; goto begining and insert
    (goto-char (org-element-property :begin (org-element-context)))
    (insert "\n#+BEGIN_SRC ipython

#+END_SRC\n\n")
    (forward-line -3))

   (t
    (beginning-of-line)
    (insert "\n#+BEGIN_SRC ipython

#+END_SRC\n")
    (forward-line -2))))


(defun org-babel-split-src-block (&optional below)
  "Split the current src block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
	 (language (org-element-property :language el))
	 (parameters (org-element-property :parameters el)))

    (beginning-of-line)
    (insert (format "#+END_SRC

#+BEGIN_SRC %s %s\n" language parameters))
    (beginning-of-line)
    (when (not below)
      (org-babel-previous-src-block))))

(define-key org-mode-map (kbd "H--") #'org-babel-split-src-block)

(defun org-babel-execute-to-point ()
  "Execute all the blocks up to and including the one point is on."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
	(org-babel-execute-src-block)))))

;; * Enhancements to ob-ipython

(defun ob-ipython-inline-image (b64-string)
  "Write the B64-STRING to a file.
Returns an org-link to the file."
  (let* ((f (md5 b64-string))
	 (d "ipython-inline-images")
	 (tfile (concat d "/ob-ipython-" f ".png"))
	 (link (format "[[file:%s]]" tfile)))
    (unless (file-directory-p d)
      (make-directory d))
    (ob-ipython--write-base64-string tfile b64-string)
    link))


(defun ob-ipython--async-callback (status &rest args)
  "Callback function for `ob-ipython--execute-request-asynchronously'.
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
	  (setq result-type (match-string 1))
	  (replace-match "")
	  (cond
	   ((string= "output" result-type)
	    (insert
	     (concat
	      (s-trim output)
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
	   ((string= "value" result-type)
	    (insert
	     (cdr (assoc 'text/plain result)))))
	  (org-redisplay-inline-images))))
    (setq *async-ob-ipython* nil)))


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


;; This overwrites the ob-ipython function and adds better inline image support,
;; and adds async support.
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((file (cdr (assoc :file params)))
         (session (cdr (assoc :session params)))
	 (async (cdr (assoc :async params)))
         (result-type (cdr (assoc :result-type params)))
	 results)
    (org-babel-ipython-initiate-session session params)

    ;; Check the current results for inline images and delete the files.
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
	(while (re-search-forward
		"\\[\\[file:\\(ipython-inline-images/ob-ipython-.*?\\)\\]\\]" nil t)
	  (let ((f (match-string 1)))
	    (when (file-exists-p f)
	      (delete-file f))))))
    
    (if async
	(progn
	  ;; this limits us to running one async process at a time. It does not
	  ;; support multiple sessions in one org-file.
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward "async-abcd-1234" nil t)
	      (error "It looks like an async process is already running")))
	  (setq *async-ob-ipython* (current-buffer))
	  (ob-ipython--execute-request-asynchronously
	   (org-babel-expand-body:generic
	    (encode-coding-string body 'utf-8)
	    params (org-babel-variable-assignments:python params))
	   (ob-ipython--normalize-session session))
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
	      (concat
	       output
	       (format "%s"
		       (mapconcat 'identity
				  (loop for res in result
					;; if (and (eq 'text/plain (car res)) (cdr res))
					;; collect (cdr res)
					if (eq 'text/html (car res))
					collect (format
						 "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
						 (cdr res))
					if (eq 'text/latex (car res))
					collect (format
						 "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
						 (cdr res))
					if (eq 'image/png (car res))
					collect (ob-ipython-inline-image (cdr res)))
				  "\n")))
	    ;; The result here is a value. We should still get inline images though.
	    (ob-ipython--create-stdout-buffer output)
	    (concat
	     (->> result (assoc 'text/plain) cdr)
	     (format "\n%s"
		     (mapconcat 'identity
				(loop for res in result
				      ;; if (and (eq 'text/plain (car res)) (cdr res))
				      ;; collect (cdr res)
				      if (eq 'text/html (car res))
				      collect (format
					       "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
					       (cdr res))
				      if (eq 'text/latex (car res))
				      collect (format
					       "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
					       (cdr res))
				      if (eq 'image/png (car res))
				      collect (ob-ipython-inline-image (cdr res)))
				"\n")))))))))


(defun org-babel-execute-async:ipython (&optional arg)
  (interactive)
  (let* ((body (org-element-property :value (org-element-context)))
	 (params (nth 2 (org-babel-get-src-block-info)))
	 (file (cdr (assoc :file params)))
         (session (cdr (assoc :session params)))
	 (async (cdr (assoc :async params)))
	 (results (cdr (assoc :results params)))
         (result-type (cdr (assoc :result-type params))))
    (org-babel-ipython-initiate-session session params)
    
    (if (not async)
	(org-babel-execute:ipython body params)
      ;; Check the current results for inline images and delete the files.
      (let ((location (org-babel-where-is-src-block-result))
	    current-results)
	(when location
	  (save-excursion
	    (goto-char location)
	    (when (looking-at (concat org-babel-result-regexp ".*$"))
	      (setq current-results (buffer-substring-no-properties
				     location
				     (save-excursion
				       (forward-line 1) (org-babel-result-end)))))))
	(with-temp-buffer
	  (insert (or current-results ""))
	  (goto-char (point-min))
	  (while (re-search-forward
		  "\\[\\[file:\\(ipython-inline-images/ob-ipython-.*?\\)\\]\\]" nil t)
	    (let ((f (match-string 1)))
	      (when (file-exists-p f)
		(delete-file f))))))
      ;; Now we run the async
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward "async-abcd-1234" nil t)
	  (error "It looks like an async process is already running")))
      
      (setq *async-ob-ipython* (current-buffer))
      (org-babel-remove-result)
      (org-babel-insert-result (format "async-abcd-1234-%s" result-type)
			       (split-string  results " " t))
      (save-excursion
	(re-search-forward (format "async-abcd-1234-%s" result-type))
	(flyspell-delete-region-overlays (match-beginning 0) (match-end 0))
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-1]
	    `(lambda ()
	       (interactive)
	       (message "Interrupting the kernel.")
	       (save-excursion
		 (org-babel-previous-src-block)
		 (org-babel-remove-result))
	       (ob-ipython-interrupt-kernel (cdr (assoc
						  (or ,session "default")
						  (ob-ipython--get-kernel-processes))))))
	  (set-text-properties
	   (match-beginning 0) (match-end 0)
	   `(font-lock-face (:foreground "red")
			    local-map ,map
			    mouse-face highlight
			    help-echo "Click to interrupt async process"))))
      (setq font-lock-extra-managed-props (delq 'local-map font-lock-extra-managed-props))
      ;; finally call the async command.
      (message "running async")
      (ob-ipython--execute-request-asynchronously
       (org-babel-expand-body:generic
	(encode-coding-string body 'utf-8)
	params (org-babel-variable-assignments:python params))
       (ob-ipython--normalize-session session)))))

(provide 'scimax-org-babel-ipython)

;;; scimax-org-babel-ipython.el ends here
