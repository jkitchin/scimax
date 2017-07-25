;;; scimax-org-babel-python.el --- Scimax extensions for Python in org-mode

;;; Commentary:
;; `beacon' is required - https://github.com/Malabarba/beacon fellow
;; The main goal in this library is providing asynchronous Python execution in org-mode.

;; `number-line-src-block' will put temporary line numbers in a src block that
;; disappear when you type.

;; The variable `org-babel-async-python-show-results' determines if a buffer
;; pops up during execution.

;; The variable `org-babel-async-python-show-line-numbers' determines if line
;; numbers are shown in the src-block if an exception occurs. They disappear
;; when you press a key.

;; The command `org-babel-async-execute:python' executes the src-block at point
;; asynchronously, i.e. you can continue using Emacs while it runs. The results
;; section has two links while it is running, one to open the results buffer,
;; and one to cancel the process. After the job is done, the results are
;; inserted into the buffer.

;; If there is an Exception, then the Traceback is inserted into the buffer,
;; with clickable links to the line(s) causing the error. The cursor will jump
;; to the last line in the Traceback that is in the src-block and shine a beacon
;; to show you where the error is.

;; The links described above are not persistent, and are not regenerated if you
;; close the buffer and reopen it. They are created when you execute the
;; src-block, and they should be active until you close the buffer.

;; `autopep8' will reformat a src-block.

;; `pylint' will run pylint on a src-block and generate a buffer with links to
;; lines that need attention.


;; * Numbered lines in code blocks
(defvar number-line-overlays '()
  "List of overlays for line numbers.")

(make-variable-buffer-local 'number-line-overlays)

(defun number-line-src-block ()
  "Add line numbers to an org src-block."
  (interactive)
  (save-excursion
    (let* ((src-block (org-element-context))
           (nlines (- (length
                       (s-split
                        "\n"
                        (org-element-property :value src-block)))
                      1)))
      ;; clear any existing overlays
      (when number-line-overlays
	(mapc 'delete-overlay
	      number-line-overlays)
	(setq number-line-overlays '()))

      (goto-char (org-element-property :begin src-block))
      ;; the beginning may be header, so we move forward to get the #+BEGIN
      ;; line. Then jump one more to get in the code block
      (while (not (looking-at "#\\+BEGIN"))
	(forward-line))
      (forward-line)
      (loop for i from 1 to nlines
            do
            (beginning-of-line)
            (let (ov)
              (setq ov (make-overlay (point)(point)))
              (overlay-put
	       ov
	       'before-string (propertize
			       (format "%03s:" (number-to-string i))
			       'font-lock-face '(:foreground "black" :background "gray80")
			       'local-map (let ((map (make-sparse-keymap)))
					    (define-key map [mouse-1]
					      (lambda ()
						(interactive)
						(mapc 'delete-overlay
						      number-line-overlays)
						(setq number-line-overlays '())))
					    map)))
	      (overlay-put ov 'mouse-face 'highlight)
	      (overlay-put ov 'help-echo "Click to remove")
	      (overlay-put ov 'local-map (let ((map (make-sparse-keymap)))
					   (define-key map [mouse-1]
					     (lambda ()
					       (interactive)
					       (mapc 'delete-overlay
						     number-line-overlays)
					       (setq number-line-overlays '())))
					   map))
              (add-to-list 'number-line-overlays ov))
            (next-line))))
  (add-hook 'post-command-hook 'number-line-src-block nil 'local))


;; * Asynchronous python

(defvar org-babel-async-python-show-results nil
  "Determines if the async buffer is shown while the src block runs.")

(defvar org-babel-async-python-show-line-numbers t
  "Determines if line numbers are shown after an exception.")

(defun org-babel-async-execute:python (&optional arg)
  "Execute the python src-block at point asynchronously.

:var headers are supported.
:results output is all that is supported for output.

The variable `org-babel-async-python-show-results' determines if
a new window will pop up showing you the output as it appears,
and the output in that window will be put in the RESULTS section
of the code block. If there is an exception, the cursor will jump
back to the line it occurred on in the code block, and the files
in the traceback are clickable.

Use a prefix arg to force it to run if it is already running, or
there is an async marker present.

Note that if there are side effects from the code block these do
not get undone when you kill the process, e.g. if you modify
files.

To make C-c C-c use this, try this.
 (add-to-list 'org-ctrl-c-ctrl-c-hook 'org-babel-async-execute:python)"
  (interactive "P") 
  (when (and (org-in-src-block-p) 
	     (string= "python" (nth 0 (org-babel-get-src-block-info))))
    (let* ((current-file (buffer-file-name))
	   (cb (current-buffer))
	   (code (org-remove-indentation
		  (org-element-property :value (org-element-context))))
	   (varcmds (org-babel-variable-assignments:python
		     (nth 2 (org-babel-get-src-block-info))))
	   (params (nth 2 (org-babel-get-src-block-info)))
	   (wc (current-window-configuration))
	   (file-line-regexp "File \"\\(.*\\)\",? line \\([0-9]*\\)")
	   py-file
	   md5-hash
	   pbuffer 
	   process)
      
      ;; First, check if something is running
      (let ((location (org-babel-where-is-src-block-result))
	    results)
	(when location
	  (save-excursion
	    (goto-char location)
	    (when (looking-at (concat org-babel-result-regexp ".*$"))
	      (setq results (buffer-substring-no-properties
			     location
			     (save-excursion 
			       (forward-line 1) (org-babel-result-end)))))))
	(with-temp-buffer (insert (or results ""))
			  (goto-char (point-min))
			  (when (re-search-forward "<async:\\(.*\\)>" nil t)
			    (setq md5-hash (match-string 1))))
	(when md5-hash 
	  (if (and (get-process md5-hash) (not arg))
	      (error "%s is running. Use prefix arg to kill it."
		     (match-string 0 results))
	    ;; we want to kill stuff, delete results and continue. we either
	    ;; asked for it to be killed, or the process is dead/stale
	    (if (get-process md5-hash)
		(interrupt-process (format "*py-%s*" md5-hash))
	      (when (get-buffer (format "*py-%s*" md5-hash))
		(kill-buffer (format "*py-%s*" md5-hash)))))))

      ;; If there are numbered lines, we remove them, and the hook that was
      ;; updating them.
      (when number-line-overlays 
	(mapc 'delete-overlay
	      number-line-overlays)
	(setq number-line-overlays '())
	(remove-hook 'post-command-hook 'number-line-src-block 'local)
	nil)
      
      ;; Get the md5 for the current block
      (with-temp-buffer
	(dolist (cmd varcmds)
	  (insert cmd)
	  (insert "\n"))
	(insert code)
	(setq md5-hash (md5 (buffer-string))
	      pbuffer (format "*py-%s*" md5-hash)
	      py-file (expand-file-name (format "pymd5-%s.py" md5-hash))))

      ;; create the file to run
      (with-temp-file py-file
	(dolist (cmd varcmds)
	  (insert cmd)
	  (insert "\n"))
	(insert code))

      ;; get rid of old results, and put a place-holder for the new results to
      ;; come. The place holder is clickable, and kills the process.
      (org-babel-remove-result)
      (org-babel-insert-result
       (format "<Open results> <async:%s> click to kill" md5-hash)
       (cdr (assoc :result-params
		   (nth 2 (org-babel-get-src-block-info)))))

      ;; make the placeholder clickable
      (save-excursion
	(re-search-forward (format "<async:%s> click to kill" md5-hash))
	(flyspell-delete-region-overlays (match-beginning 0) (match-end 0))
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-1]
	    (lambda ()
	      (interactive) 
	      (org-babel-previous-src-block) 
	      (org-babel-kill-async))) 
	  (set-text-properties
	   (match-beginning 0) (match-end 0)
	   `(font-lock-face (:foreground "red")
			    local-map ,map
			    mouse-face highlight
			    help-echo "Click to kill async process"))))
      (save-excursion
	(re-search-forward "<Open results>")
	(flyspell-delete-region-overlays (match-beginning 0) (match-end 0))
	(let ((map (make-sparse-keymap)))
	  (define-key map [mouse-1]
	    `(lambda ()
	       (interactive)
	       (pop-to-buffer ,pbuffer)
	       (use-local-map (copy-keymap org-mode-map))
	       (setq header-line-format "Press q to quit. Press k to abort.")
	       (local-set-key "q"
			      #'(lambda ()
				  (interactive)
				  (delete-window)))
	       (local-set-key "k"
			      #'(lambda ()
				  (interactive) 
				  (quit-window t) 
				  (switch-to-buffer ,cb)
				  (goto-char (point-min))
				  (re-search-forward ,md5-hash)
				  (org-babel-previous-src-block) 
				  (org-babel-kill-async)))))
	  
	  (set-text-properties
	   (match-beginning 0) (match-end 0)
	   `(font-lock-face (:foreground "green4")
			    local-map ,map
			    mouse-face highlight
			    help-echo "Click to open results buffer"))))

      (setq font-lock-extra-managed-props (delq 'local-map font-lock-extra-managed-props))

      ;; open the results buffer to see the results in when we want it.
      (when org-babel-async-python-show-results
	(switch-to-buffer-other-window pbuffer))

      ;; run the code
      (setq process (start-process
		     md5-hash
		     pbuffer
		     python-shell-interpreter
		     py-file))


      ;; when the process is done, run this code to put the results in the
      ;; org-mode buffer.
      (set-process-sentinel
       process
       `(lambda (process event) 
	  (delete-file ,py-file) 
	  (let* ((line-number))
	    (unless (string= "finished\n" event) 
	      ;; Probably got an exception. Let's parse it and move
	      ;; point to where it belongs in the code block.
	      (with-current-buffer ,pbuffer
		(setq results (buffer-string))
		(goto-char (point-min))
		;; get the last line that matches the code block
		(while (re-search-forward
			(format "\"\\(%s\\)\", line \\([0-9]+\\)" ,py-file) nil t)
		  (replace-match "Org SRC" nil nil nil 1)
		  (setq line-number (string-to-number (match-string 2))))))

	    ;; Now get the results and insert them
	    (save-window-excursion
	      (save-excursion
		(save-restriction
		  ;; Make sure we end up deleting the temp file and buffer
		  (unwind-protect
		      (with-current-buffer ,cb
			(widen)
			(goto-char (point-min))
			(when (re-search-forward
			       (format "<async:%s>" ,md5-hash)
			       nil t)
			  (org-babel-previous-src-block)
			  (org-babel-remove-result) 
			  (org-babel-insert-result
			   (with-current-buffer ,pbuffer
			     (buffer-string))
			   (cdr (assoc :result-params
				       (nth 2 (org-babel-get-src-block-info)))))))
		    ;; delete the results buffer then delete the tempfile.
		    ;; finally, delete the process.
		    (when (get-buffer ,pbuffer)
		      (kill-buffer ,pbuffer))
		    (when process
		      (delete-process process))))))

	    ;; restore window configuration
	    (set-window-configuration ,wc)
	    (org-redisplay-inline-images)
	    
	    ;; Finally, if we got a line number, add click properties to file
	    ;; lines, move point and shine beacon
	    (when line-number
	      (save-excursion
		(while (re-search-forward ,file-line-regexp nil t) 
		  (let ((map (make-sparse-keymap))
			(start (match-beginning 1))
			(end (match-end 1))
			(fname (match-string 1))
			(ln (string-to-number (match-string 2)))) 
		    (define-key map [mouse-1]
		      `(lambda ()
			 (interactive)
			 (if (string-match "Org SRC" ,fname)
			     (progn
			       (org-babel-previous-src-block)
			       (goto-char (org-element-property :begin (org-element-context)))
			       (forward-line ,ln)
			       ;; For some reason clicking on these links
			       ;; sometimes folds the results drawer. This makes
			       ;; sure it is unfolded.
			       (when (-contains? (cdr
						  (assoc
						   :result-params
						   (nth 2 (org-babel-get-src-block-info))))
						 "drawer")
				 (save-excursion
				   (search-forward ":RESULTS:")
				   (org-flag-drawer nil))))
			   ;; regular file
			   (find-file ,fname)
			   (goto-line ,ln))))
		    (flyspell-delete-region-overlays start end)
		    (set-text-properties
		     start
		     end 
		     `(font-lock-face org-link
				      mouse-face highlight 
				      local-map ,map
				      help-echo "Click to open")))))

	      (goto-char (org-element-property :begin (org-element-context)))
	      (forward-line (- line-number (length (org-babel-variable-assignments:python
						    (nth 2 (org-babel-get-src-block-info)))))) 
	      (message "%s" results)
	      (let ((beacon-color "red")) (beacon--shine))
	      (when org-babel-async-python-show-line-numbers
		(number-line-src-block)))))))))


(defun org-babel-kill-async ()
  "Kill the current async process.
Run this in the code block that is running."
  (interactive)
  (let ((location (org-babel-where-is-src-block-result))
	results)
    (when location
      (save-excursion
	(goto-char location)
	(when (looking-at (concat org-babel-result-regexp ".*$"))
	  (setq results (buffer-substring-no-properties
			 location
			 (save-excursion 
			   (forward-line 1) (org-babel-result-end))))))) 
    (when (and results (string-match "<async:\\(.*\\)>" results))
      (interrupt-process (match-string 1 results)))))

;; * autopep8

(defun autopep8 ()
  "Replace Python code block contents with autopep8 corrected code."
  (interactive)
  (unless (executable-find "autopep8")
    (if (executable-find "pip")
	(shell-command "python -c \"import pip; pip.main(['install','autopep8'])\"")
      (shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['-U','autopep8'])\"")))
  (let* ((src (org-element-context))
	 (beg (org-element-property :begin src))
	 (value (org-element-property :value src))) 
    (save-excursion
      (goto-char beg)
      (search-forward value)
      (shell-command-on-region
       (match-beginning 0)
       (match-end 0)
       "autopep8 -a -a -" nil t))))


;; * pylint
(defvar pylint-options
  '()
  "List of options to use with pylint.")

(setq pylint-options
      '("-r no "			 ; no reports
	;; we are not usually writing programs where it
	;; makes sense to be too formal on variable
	;; names.
	"--disable=invalid-name "
	;; don't usually have modules, which triggers
	;; this when there is not string at the top
	"--disable=missing-docstring "
	;; superfluous-parens is raised with print(),
	;; which I am promoting for python3
	;; compatibility.
	"--disable=superfluous-parens "	;

	;; these do not seem important for my work.
	"--disable=too-many-locals "	;

	;; this is raised in solving odes and is
	;; unimportant for us.
	"--disable=unused-argument "	;
	"--disable=unused-wildcard-import "
	"--disable=redefined-outer-name "
	;; this is triggered a lot from fsolve
	"--disable=unbalanced-tuple-unpacking "
	"--disable=wildcard-import "
	"--disable=redefined-builtin "
	;; I dont mind semicolon separated lines
	"--disable=multiple-statements "
	;; pylint picks up np.linspace as a no-member error. That does not make sense.
	"--disable=no-member "
	"--disable=wrong-import-order "
	"--disable=unused-import "))


(defun pylint ()
  "Run pylint on a source block.
Opens a buffer with links to what is found. This function installs pylint if needed."
  (interactive)
  (let ((eop (org-element-at-point))
	(temporary-file-directory ".")
        (cb (current-buffer))
	(n) ; for line number
	(cn) ; column number
	(content) ; error on line
	(pb "*pylint*") 
	(link)
	(tempfile))

    (unless (executable-find "pylint")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pylint'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['pylint'])\"")))

    ;; rm buffer if it exists
    (when (get-buffer pb) (kill-buffer pb))

    ;; only run if in a python code-block
    (when (and (eq 'src-block (car eop))
	       (string= "python" (org-element-property :language eop)))

      ;; tempfile for the code
      (setq tempfile (make-temp-file "org-py-check" nil ".py"))
      ;; create code file
      (with-temp-file tempfile
	(insert (org-element-property :value eop)))
      
      ;; pylint
      (let ((status (shell-command
		     (concat
		      "pylint "
		      (mapconcat 'identity pylint-options " ")
		      " "
		      ;; this is the file to check.
		      (file-name-nondirectory tempfile))))

	    ;; remove empty strings
	    (output (delete "" (split-string
				(with-current-buffer "*Shell Command Output*"
				  (buffer-string)) "\n"))))

	;; also remove this line so the output is empty if nothing
	;; comes up
	(setq output (delete
		      "No config file found, using default configuration"
		      output))

	(kill-buffer "*Shell Command Output*")
	(if output
	    (progn
	      (set-buffer (get-buffer-create pb))
	      (insert (format "\n\n* pylint (status = %s)\n" status))
	      (insert "pylint checks your code for errors, style and convention. Click on the links to jump to each line.

")

	      (dolist (line output)
		;; pylint gives a line and column number
		(if
		    (string-match "[A-Z]:\\s-+\\([0-9]*\\),\\s-*\\([0-9]*\\):\\(.*\\)"
				  line)
		    (let ((line-number (match-string 1 line))
			  (column-number (match-string 2 line))
			  (content (match-string 3 line)))

		      (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s)(forward-line 0)(forward-char %s))][%s]]\n"
					 cb
					 (org-element-property :begin eop)
					 line-number
					 column-number
					 line)))
		  ;; no match, just insert line
		  (setq link (concat line "\n")))
		(insert link)))
	  (message "pylint was clean!")))

      (when (get-buffer pb)
	;; open the buffer
	(switch-to-buffer-other-window pb)
	(goto-char (point-min))
	(insert "Press q to close the window\n")
	(org-mode)
	(org-cycle '(64))  ; open everything
	;; make read-only and press q to quit
	(setq buffer-read-only t)
	(use-local-map (copy-keymap org-mode-map))
	(local-set-key "q" #'(lambda () (interactive) (kill-buffer)))
	(switch-to-buffer-other-window cb))
      ;; final cleanup and delete file
      (delete-file tempfile))))


(provide 'scimax-org-babel-python)

;;; scimax-org-babel-python.el ends here
