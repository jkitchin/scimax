;;; scimax-org-babel-ipython.el --- Scimax enhancements to ob-ipython

;;; Commentary:
;;
;; This library expands the ob-ipython library a lot. Many functions are just
;; redefined in here, and many new functions are defined. Most notably this
;; provides asynchronous execution, inspection and completion. It also provides
;; better support for inline figures and the rich output of jupyter.
;;
;; Known limitations:
;; 1. You should not use session names that have a - in them.
;;
;; 2. Sometimes you have to `nuke-ipython' to restart.
;;
;; Customization variables
;;
;; If `org-babel-async-ipython' is non-nil the src blocks will be run
;; asynchrounously allowing you to work in emacs while the code runs. This mode
;; will create names for each code-block if they don't already have a name,
;; defaulting to a human readable name. The number of words in the name is
;; defined by `org-babel-ipython-name-length'. The function used to generate the
;; name is defined in `org-babel-ipython-name-generator'.
;;
;; I prefer a single kernel per org-buffer to prevent cross-talk between them.
;; You can turn this off using the variable `ob-ipython-buffer-unique-kernel'.
;;
;; Set `org-babel-ipython-debug ' to non-nil to get better debugging information
;; in the buffer *ob-ipython-log*.

;; Set `ob-ipython-number-on-exception' to non-nil to get line-number overlays
;; in src blocks when exceptions occur.
;;
;; Convenience commands:
;;
;; `org-babel-insert-block' will insert a new src block above the current point,
;; or below it with a prefix arg.
;;
;; `org-babel-split-src-block' will split the current block leaving point in the
;; upper block, or with a prefix arg in the lower block.
;;
;; `org-babel-execute-to-point' will execute all blocks up to the current point.
;; See also `org-babel-execute-ipython-buffer-to-point-async'
;; `org-babel-execute-ipython-buffer-async'

;; `ob-ipython-inspect' will open a buffer with documentation about the thing at point.
;;
;; `ob-ipython-signature-function' will show a message in the minibuffer about
;; the signature function.
;;
;; You should get completion of python code in the org buffer if
;; `org-babel-ipython-completion ' is set to non-nil.
;;
;; While a cell is running or queued, there will be a link in the results
;; section that you can click on to halt the execution or remove the cell from
;; the queue.
;;
;; To clear the queue use M-x `org-babel-async-ipython-clear-queue'.

;;
;; If you have difficulties try running `debug-ipython'.


(require 'ob-ipython)

(defcustom scimax-ipython-command "jupyter"
  "Command to launch the jupyter kernel."
  :group 'ob-ipython)


(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer."
  :group 'ob-ipython)


(defcustom org-babel-ipython-debug nil
  "If non-nil, log messages."
  :group 'ob-ipython)


(defcustom ob-ipython-number-on-exception t
  "If non-nil add line numbers to src-blocks when there is an exception."
  :group 'ob-ipython)


(defcustom org-babel-async-ipython t
  "If non-nil run ipython asynchronously."
  :group 'ob-ipython)


(defcustom ob-ipython-exception-results t
  "If non-nil put the contents of the traceback buffer as results.")


(defcustom org-babel-ipython-completion t
  "If non-nil enable completion in org-mode."
  :group 'ob-ipython)


(defcustom org-babel-ipython-name-length 4
  "Number of words to use in generating a name."
  :group 'ob-ipython)


(defcustom org-babel-ipython-name-generator 'generate-human-readable-name
  "Function to generate a name for a src block.
The default is the human-readable name generator
`generate-human-readable-name'. The function should generate a
name that is unique within the document. You might also like
`org-id-uuid'."
  :group 'ob-ipython)

(defcustom org-babel-ipython-inline-image-dir "ipython-inline-images"
  "Directory to store ipython generated images."
  :group 'ob-ipython)
(make-variable-buffer-local 'org-babel-ipython-inline-image-dir)

;;; Code:

;; I decided to just remove these from scimax. I think they are all covered in
;; yasnippet now.

;; (if (version< (org-version) "9.2")
;;     (add-to-list 'org-structure-template-alist
;; 		 '("ip" "#+BEGIN_SRC ipython\n?\n#+END_SRC"
;; 		   "<src lang=\"python\">\n?\n</src>")))


(setq org-babel-default-header-args:ipython
      '((:results . "output replace drawer")
	(:session . "ipython")
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


(defun ob-ipython-log (msg &rest args)
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

#+BEGIN_SRC %s %s\n" language (or parameters "")))
    (beginning-of-line)
    (when (not below)
      (org-babel-previous-src-block))))

(define-key org-mode-map (kbd "H--") #'org-babel-split-src-block)


;;* Enhancements to ob-ipython

;; overwrites the ob-python function to get jupyter instead of hard-coded
;; ipython.
(defun ob-ipython--kernel-repl-cmd (name)
  (list scimax-ipython-command "console" "--existing" (format "emacs-%s.json" name)))


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
	 (d org-babel-ipython-inline-image-dir)
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
           ('text/org (concat value "\n"))
			     ('text/plain (concat value "\n"))
			     ('text/html (format
					  "#+BEGIN_EXPORT HTML\n%s\n#+END_EXPORT\n"
					  value))
			     ('text/latex (concat value "\n"))
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

(defun ob-ipython-jump (buffer src-name N)
  "Jump to BUFFER then the src-block with SRC-NAME to line N."
  (pop-to-buffer buffer)
  (org-babel-goto-named-src-block src-name)
  (while (not (looking-at "#\\+BEGIN"))
    (forward-line))
  (forward-line N))


;; modified function to get better error feedback
(defun ob-ipython--create-traceback-buffer (traceback)
  "Creates a traceback error when an exception occurs.
Sets up a local key to jump back to the Exception in the traceback buffer."
  (let* ((src (org-element-context))
         (buf (get-buffer-create "*ob-ipython-traceback*"))
         (curwin (current-window-configuration))
         (exc-buffer *org-babel-ipython-exception-buffer*)
         N pos syntax-error-p)
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max)))

      (goto-char (point-min))
      ;; try to find a regular exception line number
      (if (re-search-forward "-+> \\([0-9]+\\)" nil t)
          (setq N (string-to-number (match-string 1)))
        (setq N 0))
      ;; We may have had a SyntaxError. Let's check.
      (goto-char (point-min))
      (when (re-search-forward "SyntaxError:" nil t)
        (setq syntax-error-p t)
        (goto-char (point-min))
        ;; get the line number
        (when (re-search-forward "File.*, line \\([0-9]+\\)" nil t)
          (setq N (string-to-number (match-string 1)))
          (when (re-search-forward "\\( +\\)^" nil t)
            ;; move point to be on the error
            (backward-char)
            ;; It appears the line is indented 4 spaces, so we cop that off
            (setq pos (- (length (match-string 1)) 4))))


        (use-local-map (copy-keymap special-mode-map))
        (setq header-line-format "Press j to jump to src block. q to bury this buffer. i to insert the traceback as results in the src block.")
        (local-set-key "j" `(lambda ()
                              "Jump to the line in the src block that caused the exception."
                              (interactive)
                              (if (not org-babel-async-ipython)
                                  (goto-char ,(org-element-property :begin src))
                                ;; on an async cell
                                (ob-ipython-jump
                                 *org-babel-ipython-exception-buffer*
                                 (with-current-buffer *org-babel-ipython-exception-buffer*
                                   (cdr (ob-ipython-get-running)))
                                 ,N))
                              (when ,syntax-error-p (forward-char ,pos))
                              (org-babel-async-ipython-clear-queue)
                              (when ob-ipython-number-on-exception
                                (number-line-src-block))))
        (local-set-key "q" `(lambda ()
                              "Bury traceback buffer and jump to the line in the src block that caused the exception."
                              (interactive)
                              (bury-buffer)
                              ;; (set-window-configuration ,curwin)
                              (if (not org-babel-async-ipython)
                                  (goto-char ,(org-element-property :begin src))
                                ;; on an async cell
                                (ob-ipython-jump
                                 *org-babel-ipython-exception-buffer*
                                 (with-current-buffer *org-babel-ipython-exception-buffer*
                                   (cdr (ob-ipython-get-running)))
                                 ,N))
                              (when ,syntax-error-p (forward-char ,pos))
                              (org-babel-async-ipython-clear-queue)
                              (when ob-ipython-number-on-exception
                                (number-line-src-block))))
        (local-set-key "i" `(lambda ()
                              "Insert the traceback as results in the src block that caused the exception."
                              (interactive)
                              (if (not org-babel-async-ipython)
                                  (goto-char ,(org-element-property :begin src))
                                ;; on an async cell
                                (let ((contents (buffer-string))
                                      (buf (pop-to-buffer *org-babel-ipython-exception-buffer*))
                                      (cell (ob-ipython-get-running)))
                                  (org-babel-async-ipython-clear-queue)
                                  (ob-ipython-jump buf (cdr cell) 1)

                                  ;; We linkify the exception lines
                                  (org-babel-insert-result
                                   (with-temp-buffer
                                     (insert contents)
                                     (goto-char (point-min))
                                     (while (re-search-forward "-+> \\([0-9]+\\).*" nil t)
                                       (replace-match
                                        (format "[[elisp:(ob-ipython-jump \"%s\" \"%s\" %s)][%s]]"
                                                (car cell)
                                                (cdr cell)
                                                (match-string 1)
                                                (match-string 0))))
                                     (buffer-string))
                                   (assoc :result-params
                                          (third (org-babel-get-src-block-info)))))))))
      ;; insert results or pop to the traceback buffer
      (if ob-ipython-exception-results
          ;; Here we are inserting the traceback in the org-buffer
          (let ((contents (with-current-buffer buf (buffer-string)))
                (buf (pop-to-buffer *org-babel-ipython-exception-buffer*))
                (cell (ob-ipython-get-running))
                line pos)
            (setq header-line-format (format "%s had an exception." (cdr cell)))
            (ob-ipython-jump buf (cdr cell) 1)
            (org-babel-async-ipython-clear-queue)
            (org-babel-insert-result
             (with-temp-buffer
               (insert contents)
               (goto-char (point-min))
               (while (re-search-forward "-+> \\([0-9]+\\).*" nil t)
                 (replace-match
                  (format "[[elisp:(ob-ipython-jump \"%s\" \"%s\" %s)][%s]]"
                          (car cell)
                          (cdr cell)
                          (match-string 1)
                          (match-string 0))))
               ;; linkify a SyntaxError
               (goto-char (point-min))
               (when (re-search-forward "SyntaxError:" nil t)
                 (goto-char (point-min))
                 ;; get the line number
                 (when (re-search-forward "File.*, line \\([0-9]+\\)" nil t)
                   (setq line (match-string 1))
                   (when (re-search-forward "\\( +\\)^" nil t)
                     ;; It appears the line is indented 4 spaces, so we cop that off
                     (setq pos (- (length (match-string 1)) 4)))
                   ;; I think we can just link the first line. I tried the code
                   ;; line, but sometimes it has a bracket in it which can break
                   ;; the link syntax.
                   (goto-char (point-min))
                   (setf (buffer-substring (line-beginning-position) (line-end-position))
                         (format
                          "[[elisp:(progn (ob-ipython-jump \"%s\" \"%s\" %s)(forward-char %s))][%s]]"
                          (car cell)
                          (cdr cell)
                          line
                          pos
                          (buffer-substring (line-beginning-position) (line-end-position))))))
               (buffer-string))
             ;; these are the headers for the results of the block
             (assoc :result-params
                    (third (org-babel-get-src-block-info)))))

        ;; We are not capturing results so this makes the traceback the current
        ;; buffer
        (ob-ipython-log "Popping to %s" buf)
        (pop-to-buffer buf)
        (setq header-line-format (format "%s had an exception." (cdr (ob-ipython-get-running))))))))


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
(defun ob-ipython--inspect-request (code &optional pos detail)
  "This function is used to inspect code at a position.
This can provide information about the type, etc."
  (let ((url-request-data (json-encode `((code . ,code)
                                         (pos . ,(or pos (length code)))
                                         (detail . ,(or detail 0)))))
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://%s:%d/inspect/%s"
                                  ob-ipython-driver-hostname
                                  ob-ipython-driver-port
                                  (org-babel-get-session)))
      (if (>= (url-http-parse-response) 400)
          (ob-ipython--dump-error (buffer-string))
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))

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
    (when (org-in-ipython-block-p) (org-narrow-to-block))
    (if (ob-ipython-get-running)
	(message "The kernel is busy running %s. Try later." (cdr (ob-ipython-get-running)))

      (-if-let (result (->> (ob-ipython--inspect buffer
						 (- pos (point-min)))
			    (assoc 'text/plain) cdr))
	  (ob-ipython--create-inspect-buffer result)
	(message "No documentation was found.")))))

(define-key org-mode-map (kbd "M-.") #'ob-ipython-inspect)

;;* Eldoc integration

;; I had in mind to integrate this into eldoc, but it for now a standalone
;; function to get a minibuffer message.
;; Note you need my fork of ob-ipython for this to work.

(defun ob-ipython-signature-function (buffer pos)
  "Show a signature of the function at point in the minibuffer."
  (interactive (list (current-buffer) (point)))
  (save-restriction
    ;; Note you may be in a special edit buffer in which case it is not
    ;; necessary to narrow.
    (when (org-in-ipython-block-p) (org-narrow-to-block))
    (-if-let (result (->> (ob-ipython--inspect buffer
					       (- pos (point-min)))
			  (assoc 'text/plain)
			  cdr))
	(progn
	  (when (stringp result)
	    (setq result (ansi-color-apply result)))
	  (cond
	   ((s-starts-with? "Signature:" result)
	    (message (car (split-string result "\n"))))
	   ((s-starts-with? "Docstring:" result)
	    (message (s-join "\n" (-slice (split-string result "\n") 0 2))))
	   (t
	    (message (car (split-string result "\n"))))))
      (message "Nothing found"))))

(define-key org-mode-map (kbd "C-1") #'ob-ipython-signature-function)


;;* Completion

;; This allows you to get completion from the ipython kernel.
(defun ob-ipython--complete-request (code &optional pos)
  "Get completion candidates for the thing at POS from the kernel."
  (let ((url-request-data (json-encode `((code . ,code)
                                         (cursor_pos . ,(or pos (length code))))))
        (url-request-method "POST"))
    (with-current-buffer (url-retrieve-synchronously
                          (format "http://%s:%d/complete/%s"
                                  ob-ipython-driver-hostname
                                  ob-ipython-driver-port
                                  (org-babel-get-session)))
      (if (>= (url-http-parse-response) 400)
          (ob-ipython--dump-error (buffer-string))
        (goto-char url-http-end-of-headers)
        (let ((json-array-type 'list))
          (json-read))))))


(defun ob-ipython-complete ()
  "Get completion candidates for the thing at point."
  (if (ob-ipython-get-running)
      (message "The kernel is busy running %s." (cdr (ob-ipython-get-running)))
    (save-restriction
      (when (org-in-ipython-block-p) (org-narrow-to-block))
      (-if-let (result (->> (ob-ipython--complete-request
			     (buffer-substring-no-properties (point-min) (point-max))
			     (- (point) (point-min)))
			    car
			    (assoc 'content)))
	  (list
	   (cdr (assoc 'matches result))
	   (cdr (assoc 'cursor_start result))
	   (cdr (assoc 'cursor_end result)))))))


(defun ob-ipython-complete-ivy ()
  "Use ivy to complete the thing at point."
  (interactive)
  (let* ((result (ob-ipython-complete))
	 (candidates (first result))
	 (origin (save-restriction
		   (org-narrow-to-block)
		   (point-min)))
	 (beg (+ origin (second result)))
	 (end (+ origin (third result))))
    (ivy-read "Complete: " candidates
	      :action (lambda (candidate)
			(with-ivy-window
			  (setf (buffer-substring beg end) candidate)
			  (forward-char (length candidate)))))))


(define-key org-mode-map (kbd "s-.") #'ob-ipython-complete-ivy)

(defvar ob-ipython-syntax-table
  (make-syntax-table org-mode-syntax-table))

(modify-syntax-entry ?. "_." ob-ipython-syntax-table)
(modify-syntax-entry ?= ".=" ob-ipython-syntax-table)
(modify-syntax-entry ?' "|'" ob-ipython-syntax-table)

;; This is a company backend to get completion while typing in org-mode.
(defun ob-ipython-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (if (and (not (ob-ipython-get-running))
           (member (get-char-property (point) 'lang)
                   '("ipython" "python")))
      (cl-case command
        (interactive (company-begin-backend 'ob-ipython-company-backend))
        (prefix (with-syntax-table ob-ipython-syntax-table
                  (when (looking-back "\\_<[a-zA-Z][a-zA-Z0-9._]*"
                                      (line-beginning-position))
                    (match-string 0))))
        (candidates (car (ob-ipython-complete)))
        ;; sorted => t if the list is already sorted
        (sorted t)
        ;; duplicates => t if there could be duplicates
        (duplicates nil)
        (require-match 'never))
    nil))


;;* Asynchronous ipython

(defun ob-ipython-get-kernel-name ()
  "Get the kernel name for the current buffer."
  (if-let (bf (buffer-file-name))
      (md5 (expand-file-name bf))
    "scratch"))


(defvar *org-babel-async-ipython-running-cell* (make-hash-table :test 'equal)
  "A hash table of (kernel . (buffer . name)) of the current cell for each kernel.")


(defun ob-ipython-get-running ()
  "Get current running cell."
  (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-running-cell*))


(defun ob-ipython-set-running-cell (cell)
  "Set the current running CELL for this buffer."
  (setf (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-running-cell*)
	cell))


(defvar *org-babel-async-ipython-queue* (make-hash-table :test 'equal)
  "A hash table (kernel . (list (buffer . name))) for cells to run.")


(defun ob-ipython-queue ()
  "Return current queue for buffer."
  (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*))


(gv-define-setter ob-ipython-queue (val)
  `(setf (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*)
	 ,val))


(defun ob-ipython-queue-cell (cell)
  "Add CELL to the buffer queue."
  (setf (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*)
	(append
	 (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*)
	 (list cell))))


(defun ob-ipython-remove-cell (cell)
  "Remove CELL from this buffer queue."
  (setf (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*)
	(remove
	 cell
	 (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*))))


(defun ob-ipython-pop-queue ()
  "Return the next item in the queue for this buffer."
  (pop (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*)))


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
    "fanta" "fifteen" "fig" "fillet" "finch" "fish" "five" "fix"
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


(defun org-babel-get-name-create ()
  "Get the name of a src block or add a name to the src block at point."
  (let* ((elem (org-element-at-point))
         (name (org-element-property :name elem)))
    (or name
        (let ((beg (org-element-property :begin elem))
              (id (funcall org-babel-ipython-name-generator)))
          (save-excursion
            (goto-char beg)
            (insert (format "#+NAME: %s\n" id)))
          id))))


(defun org-babel-get-session ()
  "Return current session.
I wrote this because params returns none instead of nil. But in
that case the process that ipython uses appears to be default."
  (if-let (info (org-babel-get-src-block-info 'light))
      (let* ((args (third info))
             (session (cdr (assoc :session args))))
        (if (and session
                 (stringp session)
                 (not (string= "none" session)))
            session
          "default"))
    (error "Not on a src block")))

;;** async links

(org-link-set-parameters
 "async-queued"
 :follow (lambda (path)
	   (let* ((f (split-string path " " t))
		  (name (first f)))
	     (ob-ipython-remove-cell (cons (current-buffer) name)))
	   (save-excursion
	     (org-babel-previous-src-block)
	     (org-babel-remove-result)))
 :face '(:foreground "red")
 :help-echo "Queued")


(org-link-set-parameters
 "async-running"
 :follow (lambda (path)
	   (org-babel-goto-named-src-block path)
	   (nuke-ipython)
	   (save-excursion
	     (org-babel-previous-src-block)
	     (org-babel-remove-result))
	   (ob-ipython-set-running-cell nil)
	   (setf (ob-ipython-queue) nil))
 :face '(:foreground "green4")
 :help-echo "Click to kill kernel")

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
  (loop for (buffer . name) in (ob-ipython-queue)
	do
	(save-window-excursion
	  (with-current-buffer buffer
	    (ob-ipython-log "Clearing %s in %s" name buffer)
	    (org-babel-goto-named-src-block name)
	    (org-babel-remove-result))))
  (ob-ipython-set-running-cell nil)
  (setf (gethash (ob-ipython-get-kernel-name) *org-babel-async-ipython-queue*) nil))


(defun org-babel-async-ipython-process-queue ()
  "Run the next job in the queue."
  (if-let ((not-running (not (ob-ipython-get-running)))
           (cell (ob-ipython-pop-queue))
           (buffer (car cell))
           (name (cdr cell)))
      (save-window-excursion
        (with-current-buffer buffer
          (org-babel-goto-named-src-block name)
          (ob-ipython-set-running-cell cell)
          (ob-ipython-log "Setting up %S to run." cell)
          (let* ((running-link (format
                                "[[async-running: %s]]"
                                (org-babel-src-block-get-property 'org-babel-ipython-name)))
                 (info (org-babel-get-src-block-info))
                 (params (third info))
                 (body
                  (let ((coderef (nth 6 info))
                        (expand
                         (if (org-babel-noweb-p params :eval)
                             (org-babel-expand-noweb-references info)
                           (nth 1 info))))
                    (if (not coderef) expand
                      (replace-regexp-in-string
                       (org-src-coderef-regexp coderef) "" expand nil nil 1))))
                 (result-params (cdr (assoc :result-params params)))
                 (session (--when-let (cdr (assoc :session params))
                            (or (and (not (equal it "none")) it)
                                "default")))
                 (var-lines (org-babel-variable-assignments:python params))
                 (body (encode-coding-string
                        (org-babel-expand-body:generic
                         (org-remove-indentation body) params var-lines)
                        'utf-8)))
            (ob-ipython--execute-request-asynchronously body session)

            (org-babel-remove-result)
            (org-babel-insert-result running-link result-params)
            (ob-ipython--normalize-session session)
            running-link)))
    (ob-ipython-log "Cannot process a queue.
    Running: %s
    Queue: %s"
                    (ob-ipython-get-running)
                    (ob-ipython-queue))
    nil))


;;** async execute functions

(defvar *org-babel-ipython-exception-buffer* nil
  "Global var to store buffer an exception came from.")


(defun ob-ipython--execute-request-asynchronously (code kernel-name)
  "This function makes an asynchronous request.
CODE is a string containing the code to execute.
NAME is the name of the kernel, usually \"default\".
A callback function replaces the results."
  (let ((url-request-data (encode-coding-string code 'utf-8))
        (url-request-method "POST")
	(curbuf (current-buffer)))
    (ob-ipython-log "Running %S\non kernel %s" code kernel-name)
    (setq *org-babel-ipython-exception-buffer* nil)
    (setq header-line-format (format "%s is running on %s" (cdr (ob-ipython-get-running)) kernel-name))
    (url-retrieve
     (format "http://%s:%d/execute/%s"
	     ob-ipython-driver-hostname
	     ob-ipython-driver-port
	     kernel-name)
     ;; the callback function
     'ob-ipython--async-callback
     ;; current buffer this was called from
     (list curbuf))))


(defun ob-ipython--async-callback (status &rest args)
  "Callback function for `ob-ipython--execute-request-asynchronously'.
It replaces the output in the results."
  (ob-ipython-log "Entering callback for %s" *org-babel-async-ipython-running-cell*)
  (let* ((ret (ob-ipython--eval (if (>= (url-http-parse-response) 400)
                                    (ob-ipython--dump-error (buffer-string))
                                  (goto-char url-http-end-of-headers)
                                  (ob-ipython-log "http request: %s"
                                                  (buffer-substring (point-min) (point-max)))
                                  (let* ((json-array-type 'list)
                                         (json (json-read)))
                                    ;; we will need this in the traceback buffer
                                    (setq *org-babel-ipython-exception-buffer* (car args))
                                    ;; This means there was an exception.
                                    (when (string= "error"
                                                   (cdr
                                                    (assoc 'msg_type (elt json 0))))
                                      (with-current-buffer (car args)
                                        (org-babel-goto-named-src-block
                                         (cdr (ob-ipython-get-running)))
                                        (org-babel-remove-result)))
                                    json))))
	 ;; If there are images, they will be in result
         (result (cdr (assoc :result ret)))
	 ;; If there is printed output, it will be in output
         (output (cdr (assoc :output ret)))
         info params result-params result-mime-type
         current-cell name
	 (image-p nil)
         (result-type))

    (with-current-buffer (car args)
      (setq current-cell (ob-ipython-get-running)
            name (cdr current-cell))
      (save-excursion
        (org-babel-goto-named-src-block name)
        (setq result-type (org-babel-src-block-get-property 'org-babel-ipython-result-type))
        (org-babel-src-block-put-property 'org-babel-ipython-executed  t)
        (ob-ipython-log "Got a result-type of %s\n return from the kernel:  %S" result-type ret)
        (setq info (org-babel-get-src-block-info))
        (setq params (third info))
        (setq result-params (cdr (assoc :result-params params)))
        (setq result-mime-type (cdr (assoc :ob-ipython-results params)))
	(when result-mime-type
	  (setq result (-filter (lambda (e) (eq (car e) (intern result-mime-type))) result)))
        (org-babel-remove-result)
        (cond
         ((string= "output" result-type)
	  (let ((res (mapconcat
		      'identity
		      (-remove
		       'null
		       (list
			(when (not (s-blank? output))
			  (format "#+BEGIN_EXAMPLE\n%s#+END_EXAMPLE\n" output))
			(-when-let (vals (-filter (lambda (e) (eq (car e) 'text/org)) result))
			  (mapconcat #'cdr vals "\n\n"))
			(-when-let* ((vals (-filter (lambda (e) (eq (car e) 'text/plain)) result))
				     (joined (mapconcat #'cdr vals "\n"))
				     (not-plot-p (not (s-contains? "<matplotlib.figure.Figure" joined))))
			  (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n" joined))
			(-when-let (vals (-filter (lambda (e) (eq (car e) 'image/png)) result))
			  (setq image-p t)
			  (mapconcat (lambda (e) (ob-ipython-inline-image (cdr e))) vals "\n"))
			(-when-let (vals (-filter (lambda (e) (eq (car e) 'text/latex)) result))
			  (format "#+BEGIN_LATEX\n%s\n#+END_LATEX\n" (mapconcat #'cdr vals "\n")))
			(-when-let (vals (-filter (lambda (e) (eq (car e) 'text/html)) result))
			  (mapconcat #'cdr vals "\n"))))
		      "\n")))
	    (org-babel-insert-result res result-params)
	    (when image-p (org-redisplay-inline-images))))
	 ((string= "value" result-type)
	  (let ((res (ob-ipython--format-result
		      result result-mime-type)))
	    (when (not (s-blank-str? res))
	      (org-babel-insert-result (s-chomp (s-chop-prefix "\n" res)) result-params info))
	    ;; If result contains image, redisplay the images
	    (when (s-contains? "[[file:" res)
	      (org-redisplay-inline-images))))))
      (ob-ipython-set-running-cell nil)
      (setq header-line-format (format "The kernel is %s" (ob-ipython-get-kernel-name))))

    (let ((traceback (get-buffer "*ob-ipython-traceback*")))
      (when traceback (kill-buffer traceback)))
    ;; see if there is another thing in the queue.
    (org-babel-async-ipython-process-queue)))


(defun org-babel-execute-async:ipython ()
  "Execute the block at point asynchronously."
  (interactive)
  (when (org-in-ipython-block-p)
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
       (cdr (assoc :result-params params)))

      (ob-ipython-queue-cell (cons (current-buffer) name))
      (ob-ipython-log "Added %s to the queue.
    The current running cell is %s.
    The queue contains %S."
                      name
                      (ob-ipython-get-running)
                      (ob-ipython-queue))
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


(defun org-in-ipython-block-p (&optional inside)
  "Whether point is in a code source block.
When INSIDE is non-nil, don't consider we are within a src block
when point is at #+BEGIN_SRC or #+END_SRC."
  (let ((case-fold-search t))
    (or (and (equal (get-char-property (point) 'lang) "ipython"))
        (and (not inside)
             (save-excursion
               (beginning-of-line)
               (looking-at-p ".*#\\+\\(begin\\|end\\)_src ipython"))))))


(defun scimax-execute-ipython-block ()
  "Execute the block at point.
If the variable `org-babel-async-ipython' is non-nil, execute it asynchronously.
This function is used in a C-c C-c hook to make it work like other org src blocks."
  (when (org-in-ipython-block-p)

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
			    "scratch")))
	(add-to-list 'org-babel-default-header-args:ipython
		     (cons :session session-name))
	(ob-ipython-log "running kernel %s" session-name))

      (add-hook 'kill-buffer-hook #'scimax-ob-ipython-close t t))

    (when org-babel-ipython-completion
      (add-to-list 'company-backends 'ob-ipython-company-backend)
      (company-mode +1))

    (save-excursion
      (when (s-contains? "-" (org-babel-get-session))
	(user-error "The :session name (%s) cannot contain a -." (org-babel-get-session)))
      (if org-babel-async-ipython
	  (org-babel-execute-async:ipython)
	(org-babel-execute-src-block)))))

(add-to-list 'org-ctrl-c-ctrl-c-hook 'scimax-execute-ipython-block)

;;** buffer functions

(defun org-babel-execute-ipython-buffer-to-point ()
  "Execute all the ipython blocks in the buffer up to point."
  (interactive)
  (let ((s (org-babel-get-session))
        (l (- (point-max) (point))))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block)
                  (<= (point) (- (point-max) l)))
        (when (and (string= (first (org-babel-get-src-block-info)) "ipython")
                   (string= (org-babel-get-session) s))
          (if org-babel-async-ipython
              (progn
                ;; wait until last cell is finished
                (while (ob-ipython-get-running)
                  (sleep-for 0.05))
                (org-babel-execute-async:ipython))
            (org-babel-execute-src-block)))))))


(defun org-babel-execute-ipython-buffer-async ()
  "Execute source code blocks in a buffer.
Call `org-babel-execute-async:ipython' on every ipython source
block in the current buffer."
  (interactive)
  (org-save-outline-visibility t
    (org-babel-map-executables nil
      (when (org-in-ipython-block-p)
        (while (ob-ipython-get-running)
          (sleep-for 0.05))
        (org-babel-execute-async:ipython)))))


(defun org-babel-execute-ipython-subtree-async ()
  "Execute source code blocks in a subtree.
Call `org-babel-execute-async:ipython' on every ipython source
block in the current subtree."
  (interactive)
  (save-excursion
    (save-restriction
      (org-narrow-to-subtree)
      (org-babel-execute-ipython-buffer-async)
      (widen))))


(defun nuke-ipython ()
  "Kill all the ipython associated buffers and processes.
This is normally used to restart everything. Note it may kill all
kernels. It seems necessary to kill everything so that the kernel
in the current document can be restarted. This sometimes takes
longer than I would expect to work."
  (interactive)
  (loop for bufname in (list "*org-babel-ipython-debug*"
			     (format "*ob-ipython-kernel-%s*"
				     (if-let (bf (buffer-file-name))
					 (md5 (expand-file-name bf))
				       "scratch")))
	do
	(when-let (buf (get-buffer bufname)) (kill-buffer buf)))
  (loop for proc in (list (format "kernel-%s" (if-let (bf (buffer-file-name))
						  (md5 (expand-file-name bf))
						"scratch")))
	do
	(when (get-process proc)
	  (ob-ipython-log "Killing proc: %s" proc)
	  (delete-process proc)))
  ;; this is a little more aggressive at clearing out buffers and processes
  (loop for bufname in (list "*ob-ipython-client-driver*"
			     "*Python*")
	do
	(when-let (buf (get-buffer bufname)) (kill-buffer buf)))

  (loop for proc in '("localhost"
		      "client-driver")
	do
	(when (get-process proc)
	  (ob-ipython-log "Killing proc: %s" proc)
	  (delete-process proc)))

  (org-babel-async-ipython-clear-queue)
  (setq header-line-format nil))


(defun debug-ipython ()
  "Open a buffer showing debug information."
  (interactive)
  (switch-to-buffer-other-window (get-buffer-create "*org-babel-ipython-debug*"))
  (read-only-mode -1)
  (erase-buffer)
  (org-mode)
  (insert "[[elisp:nuke-ipython]]\n\n")
  (insert "[[elisp:org-babel-async-ipython-clear-queue]]\n\n")
  (insert (format "\n* Variables

- scimax-ipython-command :: %s
- ob-ipython-buffer-unique-kernel :: %s
- org-babel-ipython-debug :: %s
- ob-ipython-number-on-exception :: %s
- org-babel-async-ipython :: %s
- ob-ipython-exception-results :: %s
- org-babel-ipython-completion :: %s

* Kernel
"
		  scimax-ipython-command
		  ob-ipython-buffer-unique-kernel
		  org-babel-ipython-debug
		  ob-ipython-number-on-exception
		  org-babel-async-ipython
		  ob-ipython-exception-results
		  org-babel-ipython-completion))
  (insert (format "Running in this buffer: %s\n" (ob-ipython-get-running)))
  (insert (format "Queue in this buffer: %S\n\n" (ob-ipython-queue)))
  (insert "Overall running in all kernels:
  kernel: cell running\n")
  (ht-map (lambda (key value)
	    (insert (format "  %s: %s\n" key value)))
	  *org-babel-async-ipython-running-cell*)
  (insert "Overall queue:\n")
  (ht-map (lambda (key value)
	    (insert (format "  %s: %s\n" key value)))
	  *org-babel-async-ipython-queue*)
  (loop for buf in (buffer-list)
	do
	(when (or (s-starts-with? "*ob-ipython" (buffer-name buf))
		  (s-starts-with? "*Python" (buffer-name buf)))

	  (insert (format "* %s\n\n%s\n"
			  (buffer-name buf)
			  (with-current-buffer buf (buffer-string))))))
  (goto-char (point-min)))

;;* The end
(provide 'scimax-org-babel-ipython)

;;; scimax-org-babel-ipython.el ends here
