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


(defun org-babel-execute-ipython-buffer-async ()
  "Execute all the ipython blocks in the buffer asynchronously."
  (interactive)
  (org-block-map
   (lambda ()
     (when (string= (first (org-babel-get-src-block-info)) "ipython")
       (org-babel-execute-async:ipython)))
   (point-min)
   (point-max)))


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

(defun ob-ipython--format-result (result)
  "Format a RESULT from an ipython cell."
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
                     "\n")))


;;* Asynchronous ipython
(defvar *org-babel-async-ipython-running-cell* nil
  "A cons cell (buffer . name) of the current cell.")


(defvar *org-babel-async-ipython-queue* '()
  "Queue of cons cells (buffer . name) for cells to run.")


(defun org-babel-get-name-create ()
  "Get the name of a src block or add a uuid as the name."
  (if-let (name (fifth (org-babel-get-src-block-info)))
      name
    (save-excursion
      (let ((el (org-element-context))
	    (id (org-id-uuid)))
	(goto-char (org-element-property :begin el))
	(insert (format "#+NAME: %s\n" id))
	id))))


(defun org-babel-get-session ()
  "Return current session.
I wrote this because params returns none instead of nil. But in
that case the session name appears to be default."
  (let ((session (cdr (assoc :session (third (org-babel-get-src-block-info))))))
    (if (and session (not (string= "none" session)))
	session
      "default")))


(org-link-set-parameters
 "async-queued"
 :follow (lambda (path)
	   (let* ((f (split-string path " " t))
		  (name (first f)))
	     (setq *org-babel-async-ipython-queue*
		   (remove (rassoc name *org-babel-async-ipython-queue*)
			   *org-babel-async-ipython-queue*)))
	   (save-excursion
	     (org-babel-previous-src-block)
	     (org-babel-remove-result)))
 :face '(:foreground "red")
 :help-echo "Queued")


(org-link-set-parameters
 "async-running"
 :follow (lambda (path)
	   (ob-ipython-interrupt-kernel (org-babel-get-session))
	   (save-excursion
	     (org-babel-previous-src-block)
	     (org-babel-remove-result))
	   ;; clear the blocks in the queue.
	   (loop for (buffer . name) in *org-babel-async-ipython-queue*
		 do
		 (save-window-excursion
		   (with-current-buffer buffer
		     (org-babel-goto-named-src-block name)
		     (org-babel-remove-result))))
	   (setq *org-babel-async-ipython-queue* nil
		 *org-babel-async-ipython-running-cell* nil))
 :face '(:foreground "green4")
 :help-echo "Running")


(defun org-babel-async-ipython-clear-queue ()
  "Clear the queue and all pending results."
  (interactive)
  (loop for (buffer . name) in *org-babel-async-ipython-queue*
	do
	(save-window-excursion
	  (with-current-buffer buffer
	    (org-babel-goto-named-src-block name)
	    (org-babel-remove-result))))
  (setq *org-babel-async-ipython-running-cell* nil
	*org-babel-async-ipython-queue* '()))


(defun org-babel-async-ipython-process-queue ()
  "Run the next job in the queue."
  (if-let ((not-running (not *org-babel-async-ipython-running-cell*))
	   (queue *org-babel-async-ipython-queue*)
	   ;; It seems we cannot pop queue, which is a local copy.
	   (cell (pop *org-babel-async-ipython-queue*))
	   (buffer (car cell))
	   (name (cdr cell)))
      (save-window-excursion
	(with-current-buffer buffer
	  (org-babel-goto-named-src-block name)
	  (setq *org-babel-async-ipython-running-cell* cell)
	  (let* ((rep)
		 (params (third (org-babel-get-src-block-info)))
		 (session (org-babel-get-session))
		 (body (org-babel-expand-body:generic
			(s-join
			 "\n"
			 (append
			  (org-babel-variable-assignments:python
			   (third (org-babel-get-src-block-info)))
			  (list
			   (encode-coding-string
			    (org-element-property :value (org-element-context)) 'utf-8))))
			params)))
	    (ob-ipython--execute-request-asynchronously
	     body session)
	    (save-excursion
	      (re-search-forward (format
				  "\\[\\[async-queued: %s \\(output\\|value\\)\\]\\]"
				  name nil t))
	      (setq rep (format "[[async-running: %s %s]]" name (match-string 1)))
	      (replace-match rep))
	    (ob-ipython--normalize-session
	     (cdr (assoc :session (third (org-babel-get-src-block-info)))))
	    rep)))))


(defun ob-ipython--async-callback (status &rest args)
  "Callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results." 
  (let* ((ret (ob-ipython--eval (if (>= (url-http-parse-response) 400)
				    (ob-ipython--dump-error (buffer-string))
				  (goto-char url-http-end-of-headers)
				  (let* ((json-array-type 'list)
					 (json (json-read)))
				    (when (string= "error" (cdr (assoc 'msg_type (elt json 0))))
				      (with-current-buffer (car *org-babel-async-ipython-running-cell*)
					(org-babel-goto-named-src-block (cdr *org-babel-async-ipython-running-cell*))
					(org-babel-remove-result))
				      (org-babel-async-ipython-clear-queue)) 
				    json))))
	 (result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 (current-cell *org-babel-async-ipython-running-cell*)
	 (name (cdr current-cell))
	 result-type)
    (with-current-buffer (car current-cell)
      (save-excursion
	(org-babel-goto-named-src-block name)
	(re-search-forward (format "\\[\\[async-running: %s \\(output\\|value\\)\\]\\]" name))
	(setq result-type (match-string 1))
	(replace-match "")
	(cond
	 ((string= "output" result-type)
	  (insert
	   (concat
	    (s-trim output)
	    (ob-ipython--format-result result))))
	 ((string= "value" result-type)
	  (insert
	   (cdr (assoc 'text/plain result)))))
	(org-redisplay-inline-images)))
    (setq *org-babel-async-ipython-running-cell* nil)
    ;; see if there is another thing in the queue.
    (org-babel-async-ipython-process-queue)))


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


(defun org-babel-execute-async:ipython ()
  "Execute the block at point asynchronously."
  (interactive)
  (when (and (org-in-src-block-p)
	     (string= "ipython" (first (org-babel-get-src-block-info))))
    (let* ((name (org-babel-get-name-create)) 
	   (params (third (org-babel-get-src-block-info))) 
	   (session (cdr (assoc :session params)))
	   (results (cdr (assoc :results params)))
	   (result-type (cdr (assoc :result-type params))))
      (org-babel-ipython-initiate-session session params)

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
      (org-babel-remove-result)
      (org-babel-insert-result
       (format "[[async-queued: %s %s]]" (org-babel-get-name-create) result-type)
       (cdr (assoc :result-params (third (org-babel-get-src-block-info)))))

      (add-to-list '*org-babel-async-ipython-queue* (cons (current-buffer) name) t)

      ;; It appears that the result of this call is put into the results at this point.
      (or
       (org-babel-async-ipython-process-queue)
       (format "[[async-queued: %s %s]]" (org-babel-get-name-create) result-type)))))

(add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-execute-async:ipython)

(provide 'scimax-org-babel-ipython)

;;; scimax-org-babel-ipython.el ends here
