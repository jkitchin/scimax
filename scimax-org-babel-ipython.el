;;; scimax-org-babel-ipython.el --- Scimax enhancements to ob-ipython

;;; Commentary:
;;

(require 'ob-ipython)

(defcustom scimax-ipython-command "jupyter"
  "Command to launch the jupyter kernel.")

;; overwrites the ob-python function to get jupyter instead of hard-coded
;; ipython.
(defun ob-ipython--kernel-repl-cmd (name)
  (list scimax-ipython-command "console" "--existing" (format "emacs-%s.json" name)))

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


;;* Logging

(defcustom org-babel-ipython-debug nil
  "If non-nil, log messages.")


(defun ob-ipython-log (msg &optional &rest args)
  (when org-babel-ipython-debug
    (with-current-buffer (get-buffer-create "*ob-ipython-log*")
      (org-mode)
      (insert (format "ob-ipython: \n%s\n\n"
		      (apply 'format msg args))))))


;;* Commands like the jupyter notebook has

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


;;* Enhancements to ob-ipython
;; This allows unicode chars to be sent to the kernel
;; https://github.com/jkitchin/scimax/issues/67
(defun ob-ipython--execute-request (code name)
  (let ((url-request-data (encode-coding-string code 'utf-8))
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://%s:%d/execute/%s"
                                  ob-ipython-driver-hostname
                                  ob-ipython-driver-port
                                  name))
      (if (>= (url-http-parse-response) 400)
          (ob-ipython--dump-error (buffer-string))
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))


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

(defun ob-ipython--format-result (result result-type)
  "Format a RESULT from an ipython cell.
Return RESULT-TYPE if specified. This comes from a header argument :ob-ipython-results"
  (cl-flet ((format-result (type value)
             (case type
               ('text/plain (concat value "\n"))
               ('text/html (format
                            "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
                            value))
               ('text/latex (format
                             "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT\n"
                             values))
               ('image/png (concat (ob-ipython-inline-image value) "\n"))))
            (select-result-type (type result)
               (if type
                   (--filter (eq (car it) (intern type)) result)
                 result)))
    (->> result
         (select-result-type result-type)
         (--map (format-result (car it) (cdr it)))
         (apply #'concat "\n"))))

;;* A better synchronous execute function
(defcustom ob-ipython-number-on-exception t
  "If non-nil add line numbers to src-blocks when there is an exception.")


;; modified function to get better error feedback
(defun ob-ipython--create-traceback-buffer (traceback)
  "Creates a traceback error when an exception occurs.
Sets up a local key to jump back to the Exception."
  (let* ((src (org-element-context))
	 (buf (get-buffer-create "*ob-ipython-traceback*"))
	 (curwin (current-window-configuration))
	 N)
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max)))
      (goto-char (point-min))
      (re-search-forward "-+> \\([0-9]+\\)")
      (setq N (string-to-number (match-string 1)))
      (use-local-map (copy-keymap special-mode-map))
      (local-set-key "q" `(lambda ()
			    (interactive)
			    (bury-buffer)
			    (set-window-configuration ,curwin)
			    (if (not org-babel-async-ipython)
				(goto-char ,(org-element-property :begin src))
			      ;; on an async cell
			      (let ((cell *org-babel-async-ipython-running-cell*))
				(message "%s" cell)
				(org-babel-async-ipython-clear-queue)
				(switch-to-buffer
				 ,(car *org-babel-async-ipython-running-cell*))
				(org-babel-goto-named-src-block
				 ,(cdr *org-babel-async-ipython-running-cell*))))
			    (while (not (looking-at "#\\+BEGIN"))
			      (forward-line))
			    (forward-line ,N)
			    (when ob-ipython-number-on-exception
			      (number-line-src-block)))))
    (pop-to-buffer buf)))


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
	     (ob-ipython--format-result
	      result
	      (cdr (assoc :ob-ipython-results params))))
	  ;; The result here is a value. We should still get inline images though.
	  (ob-ipython--create-stdout-buffer output)
	  (ob-ipython--format-result
	   result (cdr (assoc :ob-ipython-results params))))))))


(defun org-babel-execute-to-point ()
  "Execute all the blocks up to and including the one point is on."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
	(org-babel-execute-src-block)))))

;;** fixing ob-ipython-inspect

;; I edited this to get the position relative to the beginning of the block
(defun ob-ipython--inspect (buffer pos)
  "Get the request result for an inspect of POS in BUFFER."
  (let* ((code (with-current-buffer buffer
                 (buffer-substring-no-properties (point-min) (point-max))))
         (resp (ob-ipython--inspect-request code pos 0))
         (status (ob-ipython--extract-status resp)))
    (if (string= "ok" status)
        (ob-ipython--extract-result resp)
      (error (ob-ipython--extract-error resp)))))


;; I added the narrow to block. It seems to work ok in the special edit window, and it also seems to work ok if we just narrow the block temporarily.
(defun ob-ipython-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (save-restriction
    ;; Note you may be in a special edit buffer in which case it is not
    ;; necessary to narrow.
    (when (org-in-src-block-p) (org-narrow-to-block))
    (-if-let (result (->> (ob-ipython--inspect buffer
					       (- pos (point-min)))
			  (assoc 'text/plain) cdr))
	(ob-ipython--create-inspect-buffer result)
      (message "No documentation was found."))))

(define-key org-mode-map (kbd "M-.") #'ob-ipython-inspect)


;;* Asynchronous ipython

(defcustom org-babel-async-ipython t
  "If non-nil run ipython asynchronously.")


(defvar *org-babel-async-ipython-running-cell* nil
  "A cons cell (buffer . name) of the current cell.")


(defvar *org-babel-async-ipython-queue* '()
  "Queue of cons cells (buffer . name) for cells to run.")


;; adapted from https://github.com/zacharyvoase/humanhash/blob/master/humanhash.py
(defvar org-babel-src-block-words
  '("ack" "alabama" "alanine" "alaska" "alpha" "angel" "apart" "april"
    "arizona" "arkansas" "artist" "asparagus" "aspen" "august" "autumn"
    "avocado" "bacon" "bakerloo" "batman" "beer" "berlin" "beryllium"
    "black" "blossom" "blue" "bluebird" "bravo" "bulldog" "burger"
    "butter" "california" "carbon" "cardinal" "carolina" "carpet" "cat"
    "ceiling" "charlie" "chicken" "coffee" "cola" "cold" "colorado"
    "comet" "connecticut" "crazy" "cup" "dakota" "december" "delaware"
    "delta" "diet" "don" "double" "early" "earth" "east" "echo"
    "edward" "eight" "eighteen" "eleven" "emma" "enemy" "equal"
    "failed" "fanta" "fifteen" "fillet" "finch" "fish" "five" "fix"
    "floor" "florida" "football" "four" "fourteen" "foxtrot" "freddie"
    "friend" "fruit" "gee" "georgia" "glucose" "golf" "green" "grey"
    "hamper" "happy" "harry" "hawaii" "helium" "high" "hot" "hotel"
    "hydrogen" "idaho" "illinois" "india" "indigo" "ink" "iowa"
    "island" "item" "jersey" "jig" "johnny" "juliet" "july" "jupiter"
    "kansas" "kentucky" "kilo" "king" "kitten" "lactose" "lake" "lamp"
    "lemon" "leopard" "lima" "lion" "lithium" "london" "louisiana"
    "low" "magazine" "magnesium" "maine" "mango" "march" "mars"
    "maryland" "massachusetts" "may" "mexico" "michigan" "mike"
    "minnesota" "mirror" "mississippi" "missouri" "mobile" "mockingbird"
    "monkey" "montana" "moon" "mountain" "muppet" "music" "nebraska"
    "neptune" "network" "nevada" "nine" "nineteen" "nitrogen" "north"
    "november" "nuts" "october" "ohio" "oklahoma" "one" "orange"
    "oranges" "oregon" "oscar" "oven" "oxygen" "papa" "paris" "pasta"
    "pennsylvania" "pip" "pizza" "pluto" "potato" "princess" "purple"
    "quebec" "queen" "quiet" "red" "river" "robert" "robin" "romeo"
    "rugby" "sad" "salami" "saturn" "september" "seven" "seventeen"
    "shade" "sierra" "single" "sink" "six" "sixteen" "skylark" "snake"
    "social" "sodium" "solar" "south" "spaghetti" "speaker" "spring"
    "stairway" "steak" "stream" "summer" "sweet" "table" "tango" "ten"
    "tennessee" "tennis" "texas" "thirteen" "three" "timing" "triple"
    "twelve" "twenty" "two" "uncle" "under" "uniform" "uranus" "utah"
    "vegan" "venus" "vermont" "victor" "video" "violet" "virginia"
    "washington" "west" "whiskey" "white" "william" "winner" "winter"
    "wisconsin" "wolfram" "wyoming" "xray" "yankee" "yellow" "zebra"
    "zulu")
  "List of words to make readable names from.")


(defcustom org-babel-ipython-name-length 4
  "Number of words to use in generating a name.")


(defun generate-human-readable-name ()
  "Generate a human readable name for a src block.
The name should be unique to the buffer."
  (random t)
  (let ((N (length org-babel-src-block-words))
	(current-names (org-element-map (org-element-parse-buffer)
			   'src-block (lambda (el)
					(org-element-property
					 :name el))))
	result)
    (catch 'name
      (while t
	(setq result (s-join
		      "-"
		      (loop for i from 0 below org-babel-ipython-name-length collect
			    (elt org-babel-src-block-words (random N)))))
	(unless (member result current-names)
	  (throw 'name result))))))


(defvar org-babel-ipython-name-generator 'generate-human-readable-name
  "Function to generate a name for a src block.
The default is the human-readable name generator
`generate-human-readable-name'. The function should generate a
name that is unique within the document. You might also like
`org-id-uuid'.")


(defun org-babel-get-name-create ()
  "Get the name of a src block or add a name."
  (if-let (name (fifth (org-babel-get-src-block-info)))
      name
    (save-excursion
      (let ((el (org-element-context))
	    (id (funcall org-babel-ipython-name-generator)))
	(goto-char (org-element-property :begin el))
	(insert (format "#+NAME: %s\n" id))
	id))))

(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer.")

(defun org-babel-get-session ()
  "Return current session.
I wrote this because params returns none instead of nil. But in
that case the process that ipython uses appears to be default."
  (let ((session (cdr (assoc :session (third (org-babel-get-src-block-info))))))
    (if (and session (not (string= "none" session)))
	session
      "default")))

;;** async links

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
	   (ob-ipython-kill-kernel
	    (cdr
	     (assoc
	      (org-babel-get-session)
	      (ob-ipython--get-kernel-processes))))
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

;;** src block text properties

(defun org-babel-src-block-get-property (property)
  "Return the PROPERTY associated with the src block."
  (save-excursion
    (goto-char (org-element-property :begin (org-element-context)))
    (ob-ipython-log "Text properties: %S" (text-properties-at (point)))
    (get-text-property (point) property)))


(defun org-babel-src-block-put-property (property value)
  "Add a text property to the src-block"
  (save-excursion
    (goto-char (org-element-property :begin (org-element-context)))
    (put-text-property (line-beginning-position) (line-end-position) property value)))


;;** async queue functions

(defun org-babel-async-ipython-clear-queue ()
  "Clear the queue and all pending results."
  (interactive)
  (loop for (buffer . name) in *org-babel-async-ipython-queue*
	do
	(save-window-excursion
	  (with-current-buffer buffer
	    (ob-ipython-log "Clearing %s in %s" name buffer)
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
	  (ob-ipython-log "Setting up %S to run." cell)
	  (let* ((running-link (format "[[async-running: %s %s]]"
				       (org-babel-src-block-get-property 'org-babel-ipython-name)
				       (org-babel-src-block-get-property 'org-babel-ipython-result-type)))
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

	    (org-babel-remove-result)
	    (org-babel-insert-result
	     running-link
	     (cdr (assoc :result-params (third (org-babel-get-src-block-info)))))
	    (ob-ipython--normalize-session
	     (cdr (assoc :session (third (org-babel-get-src-block-info)))))
	    running-link)))
    (ob-ipython-log "Cannot process a queue.
    Running: %s
    Queue: %s" *org-babel-async-ipython-running-cell* *org-babel-async-ipython-queue*)
    nil))


;;** async execute functions

(defun ob-ipython--execute-request-asynchronously (code name)
  "This function makes an asynchronous request.
CODE is a string containing the code to execute.
NAME is the name of the kernel, usually \"default\".
A callback function replaces the results."
  (let ((url-request-data code)
        (url-request-method "POST"))
    (ob-ipython-log "Running %S\non kernel %s" code name)
    (url-retrieve
     (format "http://%s:%d/execute/%s"
	     ob-ipython-driver-hostname
	     ob-ipython-driver-port
	     name)
     ;; the callback function
     'ob-ipython--async-callback)))


(defun ob-ipython--async-callback (status &rest args)
  "Callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results."
  (ob-ipython-log "Entering callback for %s" *org-babel-async-ipython-running-cell*)
  (let* ((ret (ob-ipython--eval (if (>= (url-http-parse-response) 400)
				    (ob-ipython--dump-error (buffer-string))
				  (goto-char url-http-end-of-headers)
				  (let* ((json-array-type 'list)
					 (json (json-read)))
				    ;; This means there was an exception.
				    (when (string= "error"
						   (cdr
						    (assoc 'msg_type (elt json 0))))
				      (with-current-buffer
					  (car *org-babel-async-ipython-running-cell*)
					(org-babel-goto-named-src-block
					 (cdr *org-babel-async-ipython-running-cell*))
					(org-babel-remove-result)))
				    json))))
	 (result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 params
	 (current-cell *org-babel-async-ipython-running-cell*)
	 (name (cdr current-cell))
	 (result-type))
    (with-current-buffer (car current-cell)
      (save-excursion
	(org-babel-goto-named-src-block name)
	(setq result-type (org-babel-src-block-get-property 'org-babel-ipython-result-type))
	(org-babel-src-block-put-property 'org-babel-ipython-executed  t)
	(ob-ipython-log "Got a result-type of %s\n return from the kernel:  %S" result-type ret)
	(setq params (third (org-babel-get-src-block-info)))
	(org-babel-remove-result)
	(cond
	 ((string= "output" result-type)
	  (let ((res (concat
		      output
		      (ob-ipython--format-result
		       result (cdr (assoc :ob-ipython-results params))))))
	    (when (not (string= "" (s-trim res)))
	      (org-babel-insert-result
	       (s-trim res)
	       (cdr (assoc :result-params (third (org-babel-get-src-block-info))))))))
	 ((string= "value" result-type)
	  (org-babel-insert-result
	   (cdr (assoc 'text/plain result))
	   (cdr (assoc :result-params (third (org-babel-get-src-block-info)))))))
	(org-redisplay-inline-images)))
    (setq *org-babel-async-ipython-running-cell* nil)
    ;; see if there is another thing in the queue.
    (org-babel-async-ipython-process-queue)))


(defun org-babel-execute-async:ipython ()
  "Execute the block at point asynchronously."
  (interactive)
  (when (and (org-in-src-block-p)
	     (string= "ipython" (first (org-babel-get-src-block-info))))
    (let* ((name (org-babel-get-name-create))
	   (params (third (org-babel-get-src-block-info)))
	   (session (cdr (assoc :session params)))
	   (results (cdr (assoc :results params)))
	   (result-type (cdr (assoc :result-type params)))
	   (queue-link (format "[[async-queued: %s %s]]"
			       (org-babel-get-name-create) result-type)))
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

      ;; Now we run the async. First remove the old results and insert a link.
      (org-babel-remove-result)

      ;; Set text properties
      (org-babel-src-block-put-property 'org-babel-ipython-result-type result-type)
      (org-babel-src-block-put-property 'org-babel-ipython-name name)
      (org-babel-src-block-put-property 'org-babel-ipython-executed nil)

      (org-babel-insert-result
       queue-link
       (cdr (assoc :result-params (third (org-babel-get-src-block-info)))))

      (add-to-list '*org-babel-async-ipython-queue* (cons (current-buffer) name) t)
      (ob-ipython-log "Added %s to the queue.
    The current running cell is %s.
    The queue contains %S."
		      name
		      *org-babel-async-ipython-running-cell*
		      *org-babel-async-ipython-queue*)
      ;; It appears that the result of this function is put into the results at this point.
      (or
       (org-babel-async-ipython-process-queue)
       queue-link))))


(defun scimax-ob-ipython-close ()
  "Cleanup function for when buffer closes."
  ;; first we kill the kernel
  (let ((bf (format "*ob-ipython-kernel-%s*"
		    (org-babel-get-session))))
    (when (get-buffer bf)
      (kill-buffer bf)))
  ;; now if there are no active kernels we clean up the buffers
  (unless (ob-ipython--get-kernel-processes)
    (loop for buf in '("*ob-ipython-client-driver*"
		       "*ob-ipython-traceback*"
		       "*ob-ipython-stdout*"
		       "*ob-ipython-debug*"
		       "*ob-ipython-inspect*"
		       "*Python*")
	  do
	  (when (get-buffer buf)
	    (kill-buffer buf)))))


(defun scimax-execute-ipython-block ()
  "Execute the block at point.
If the variable `org-babel-async-ipython' is non-nil, execute it asynchronously.
This function is used in a C-c C-c hook to make it work like other org src blocks."
  (when (and (org-in-src-block-p)
	     (string= "ipython" (first (org-babel-get-src-block-info))))

    (when ob-ipython-buffer-unique-kernel
      ;; Use buffer local variables for this.
      (make-local-variable 'org-babel-default-header-args:ipython)

      ;; remove the old session info
      (setq org-babel-default-header-args:ipython
	    (remove (assoc :session org-babel-default-header-args:ipython)
		    org-babel-default-header-args:ipython))

      ;; add the new session info
      (let ((session-name (if-let (bf (buffer-file-name))
			      (md5 (expand-file-name bf))
			    (org-id-uuid))))
	(add-to-list 'org-babel-default-header-args:ipython
		     (cons :session session-name))
	(ob-ipython-log "running kernel %s" session-name))

      (add-hook 'kill-buffer-hook #'scimax-ob-ipython-close t t))

    (if org-babel-async-ipython
	(org-babel-execute-async:ipython)
      (org-babel-execute-src-block))))

(add-to-list 'org-ctrl-c-ctrl-c-hook 'scimax-execute-ipython-block)

;;** buffer functions

(defun org-babel-execute-ipython-buffer-to-point-async ()
  "Execute all the ipython blocks in the buffer up to point asynchronously."
  (interactive)
  (org-block-map
   (lambda ()
     (when (string= (first (org-babel-get-src-block-info)) "ipython")
       (org-babel-execute-async:ipython)))
   (point-min)
   (point)))


(defun org-babel-execute-ipython-buffer-async ()
  "Execute all the ipython blocks in the buffer asynchronously."
  (interactive)
  (org-block-map
   (lambda ()
     (when (string= (first (org-babel-get-src-block-info)) "ipython")
       (org-babel-execute-async:ipython)))
   (point-min)
   (point-max)))

;;* The end
(provide 'scimax-org-babel-ipython)

;;; scimax-org-babel-ipython.el ends here
