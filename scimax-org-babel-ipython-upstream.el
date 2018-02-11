;;; scimax-org-babel-ipython-upstream.el --- Modifications to the upstream ob-ipython module

;;; Commentary:
;;

(require 'scimax-ob)

(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer."
  :group 'ob-ipython)

(defcustom ob-ipython-show-mime-types t
  "If non-nil show mime-types in output."
  :group 'ob-ipython)

(defcustom ob-ipython-exception-results t
  "If non-nil put the contents of the traceback buffer as results."
  :group 'ob-ipython)

(add-to-list 'org-structure-template-alist
	     '("ip" "#+BEGIN_SRC ipython\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
	     '("ipv" "#+BEGIN_SRC ipython :results value\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
	     '("plt" "%matplotlib inline\nimport matplotlib.pyplot as plt\n"
	       ""))

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


(scimax-define-src-key ipython (kbd "C-<return>") #'org-ctrl-c-ctrl-c)
(scimax-define-src-key ipython (kbd "S-<return>") #'scimax-execute-and-next-block)
(scimax-define-src-key ipython (kbd "M-<return>") #'scimax-execute-to-point)
(scimax-define-src-key ipython (kbd "s-<return>")
		       (lambda ()
			 "Restart kernel and execute block"
			 (interactive)
			 (ob-ipython-kill-kernel
			  (cdr (assoc (if-let (bf (buffer-file-name))
					  (md5 (expand-file-name bf))
					"scratch")
				      (ob-ipython--get-kernel-processes))))
			 (org-babel-execute-src-block-maybe)))
(scimax-define-src-key ipython (kbd "M-s-<return>") #'scimax-restart-ipython-and-execute-to-point)
(scimax-define-src-key ipython (kbd "H-<return>")
		       (lambda ()
			 "Restart kernel and execute buffer"
			 (interactive)
			 (ob-ipython-kill-kernel
			  (cdr (assoc (if-let (bf (buffer-file-name))
					  (md5 (expand-file-name bf))
					"scratch")
				      (ob-ipython--get-kernel-processes))))
			 (org-babel-execute-buffer)))


(scimax-define-src-key ipython (kbd "H-=") #'scimax-insert-src-block)
(scimax-define-src-key ipython (kbd "H--") #'scimax-split-src-block)
(scimax-define-src-key ipython (kbd "H-/") #'ob-ipython-inspect)
(scimax-define-src-key ipython (kbd "H-r") #'org-babel-switch-to-session)

(scimax-define-src-key ipython (kbd "H-k")
		       (lambda ()
			 (interactive)
			 (when (y-or-n-p "Kill kernel?")
			   (ob-ipython-kill-kernel
			    (cdr (assoc (if-let (bf (buffer-file-name))
					    (md5 (expand-file-name bf))
					  "scratch")
					(ob-ipython--get-kernel-processes))))
			   (setq header-line-format nil)
			   (redisplay))))


;; navigation in block
(scimax-define-src-key ipython (kbd "s-i") #'org-babel-previous-src-block)
(scimax-define-src-key ipython (kbd "s-k") #'org-babel-next-src-block)
(scimax-define-src-key ipython (kbd "H-q") #'scimax-jump-to-visible-block)
(scimax-define-src-key ipython (kbd "H-s-q") #'scimax-jump-to-block)

;; copy block
(scimax-define-src-key ipython (kbd "H-n")
		       (lambda ()
			 "Copy the block and its results."
			 (interactive)
			 (let ((src (org-element-context))
			       (result-start (org-babel-where-is-src-block-result))
			       end)
			   (if result-start
			       (save-excursion
				 (goto-char result-start)
				 (setq end (org-babel-result-end)))
			     (setq end (org-element-property :end src)))

			   (kill-new
			    (buffer-substring
			     (org-element-property :begin src)
			     end)))))

;; kill block
(scimax-define-src-key ipython (kbd "H-w")
		       (lambda ()
			 "Kill the block and its results."
			 (interactive)
			 (let ((src (org-element-context))
			       (result-start (org-babel-where-is-src-block-result))
			       end)
			   (if result-start
			       (save-excursion
				 (goto-char result-start)
				 (setq end (org-babel-result-end)))
			     (setq end (org-element-property :end src)))
			   (kill-region
			    (org-element-property :begin src)
			    end))))


;; clone block
(scimax-define-src-key ipython (kbd "H-c")
		       (lambda (&optional below)
			 "Clone the block."
			 (interactive "P")
			 (let* ((src (org-element-context))
				(code (org-element-property :value src)))
			   (scimax-insert-src-block (not below))
			   (insert code)
			   ;; jump back to start of new block
			   (org-babel-previous-src-block)
			   (org-babel-next-src-block))))

;; move blocks
(scimax-define-src-key ipython (kbd "s-w")
		       (lambda ()
			 "Move block before previous one."
			 (interactive)
			 (let ((src (org-element-context)))
			   (kill-region
			    (org-element-property :begin src)
			    (org-element-property :end src)))
			 (org-babel-previous-src-block)
			 (org-yank)))

(scimax-define-src-key ipython (kbd "s-s")
		       (lambda ()
			 "Move block after next one."
			 (interactive)
			 (let ((src (org-element-context)))
			   (kill-region
			    (org-element-property :begin src)
			    (org-element-property :end src)))
			 (org-babel-next-src-block)
			 (goto-char (org-element-property :end (org-element-context)))
			 (forward-line)
			 (org-yank)))

(scimax-define-src-key ipython (kbd "H-l") #'org-babel-remove-result)
(scimax-define-src-key ipython (kbd "H-s-l")
		       (lambda ()
			 (interactive)
			 (save-excursion
			   (goto-char (point-min))
			   (while (org-babel-next-src-block)
			     (org-babel-remove-result)))))


(defun scimax-merge-ipython-blocks (r1 r2)
  "Merge blocks in the current region (R1 R2).
This deletes the results from each block, and concatenates the
code into a single block in the position of the first block.
Currently no switches/parameters are preserved. It isn't clear
what the right thing to do for those is, e.g. dealing with
variables, etc."
  (interactive "r")
  (save-restriction
    (let* ((blocks (org-element-map (org-element-parse-buffer) 'src-block
		     (lambda (src)
		       (when (string= "ipython" (org-element-property :language src))
			 src))))
	   (first-start (org-element-property :begin (car blocks)))
	   (merged-code (s-join "\n" (loop for src in blocks
					   collect
					   (org-element-property :value src)))))
      ;; Remove blocks
      (loop for src in (reverse blocks)
	    do
	    (goto-char (org-element-property :begin src))
	    (org-babel-remove-result)
	    (setf (buffer-substring (org-element-property :begin src)
				    (org-element-property :end src))
		  ""))
      (goto-char first-start)
      (insert (format "#+BEGIN_SRC ipython
%s
#+END_SRC" (s-trim merged-code))))))

(scimax-define-src-key ipython (kbd "H-m") #'scimax-merge-ipython-blocks)

(defun scimax-jump-to-visible-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (avy-with scimax-jump-to-block
    (avy--generic-jump "#\\+BEGIN_SRC ipython"  nil avy-style (point-min) (point-max))))


(defun scimax-restart-ipython-and-execute-to-point ()
  "Kill the kernel and run src-blocks to point."
  (interactive)
  (call-interactively 'ob-ipython-kill-kernel)
  (scimax-execute-to-point))


;; * Modifications of ob-ipython

;; Modified to make buffer unique kernels automatically
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."

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
      (setq header-line-format (format "Ipython session: %s" session-name))
      (add-to-list 'org-babel-default-header-args:ipython
		   (cons :session session-name))))

  (ob-ipython--clear-output-buffer)
  ;; scimax feature to restart
  (when (assoc :restart params)
    (let ((session (if-let (bf (buffer-file-name))
		       (md5 (expand-file-name bf))
		     "scratch")))
      (ob-ipython-kill-kernel
       (cdr (assoc session
		   (ob-ipython--get-kernel-processes))))
      (cl-loop for buf in (list (format "*Python:%s*" session)
				(format "*ob-ipython-kernel-%s*" session))
	       do
	       (when (get-buffer buf)
		 (kill-buffer buf)))))
  (if (assoc :async params)
      (ob-ipython--execute-async body params)
    (ob-ipython--execute-sync body params)))


;; ** Fine tune the output of blocks
;; It was necessary to redefine these to get selective outputs via :ob-ipython-results
(defun ob-ipython--execute-async (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (sentinel (ipython--async-gen-sentinel))
	 ;; I added this. It is like the command in jupyter, but unfortunately
	 ;; similar to :display in the results from jupyter. This is to specify
	 ;; what you want to see.
	 (display-params (cdr (assoc :display params)))
	 (display (when display-params (mapcar 'intern-soft
					       (s-split " " display-params t)))))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (ob-ipython--execute-request-async
     (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                    params (org-babel-variable-assignments:python params))
     (ob-ipython--normalize-session session)

     `(lambda (ret sentinel buffer file result-type)
	(when ,display-params
	  (setf (cdr (assoc :display (assoc :result ret)))
		(-filter (lambda (el) (memq (car el) ',display))
			 (cdr (assoc :display (assoc :result ret)))))
	  (setf (cdr (assoc :value (assoc :result ret)))
		(-filter (lambda (el) (memq (car el) ',display))
			 (cdr (assoc :value (assoc :result ret))))))
	(let* ((replacement (ob-ipython--process-response ret file result-type)))
	  (ipython--async-replace-sentinel sentinel buffer replacement)))

     (list sentinel (current-buffer) file result-type))
    (format "%s - %s" (length ob-ipython--async-queue) sentinel)))

(defun ob-ipython--execute-sync (body params)
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
	 ;; I added this. It is like the command in jupyter, but unfortunately
	 ;; similar to :display in the results from jupyter. This is to specify
	 ;; what you want to see.
	 (display-params (cdr (assoc :display params)))
	 (display (when display-params (mapcar 'intern-soft
					       (s-split " " display-params t)))))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (-when-let (ret (ob-ipython--eval
                     (ob-ipython--execute-request
                      (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                     params (org-babel-variable-assignments:python params))
                      (ob-ipython--normalize-session session))))
      ;; Now I want to filter out things not in the display we want. Default is everything.
      (when display-params
	(setf (cdr (assoc :display (assoc :result ret)))
	      (-filter (lambda (el) (memq (car el) display))
		       (cdr (assoc :display (assoc :result ret)))))
	(setf (cdr (assoc :value (assoc :result ret)))
	      (-filter (lambda (el) (memq (car el) display))
		       (cdr (assoc :value (assoc :result ret))))))
      (ob-ipython--process-response ret file result-type))))



;; This gives me the output I want.
(defun ob-ipython--process-response (ret file result-type)
  (let* ((result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 (value (cdr (assoc :value result)))
	 (display (cdr (assoc :display result))))
    (s-concat
     (format "# Out[%d]:\n" (cdr (assoc :exec-count ret)))
     (when (and (not (string= "" output)) ob-ipython-show-mime-types) "# output\n")
     (when (not (string= "" output)) output)
     (s-join "\n\n" (loop for (type . value) in (append value display)
			  do
			  (message "Rendering %s" (cons type (cond
							      ((eq type 'image/png)
							       "<img data>")
							      (t value))))
			  collect
			  (ob-ipython--render file (list (cons type value))))))))


;; I added html and latex to this
(defun ob-ipython--render (file-or-nil values)
  (let ((org (lambda (value)
	       "org is verbtatim"
	       (s-join "\n" (list "# text/org" value))))
        (png (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
		 (s-join "\n" (list
			       (if ob-ipython-show-mime-types "# image/png" "")
			       (format "[[file:%s]]" file))))))
        (svg (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (s-join "\n"
			 (list
			  (if ob-ipython-show-mime-types "# image/svg" "")
			  (format "[[file:%s]]" file))))))
        (html (lambda (value)
		(format "#+BEGIN_EXPORT html\n%s\n#+END_EXPORT" value)))
	(latex (lambda (value)
		 (s-join "\n"
			 (list (if ob-ipython-show-mime-types "# text/latex" "")
			       (format "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT" value)))))
        (txt (lambda (value)
               (let ((lines (s-lines value)))
                 (if (cdr lines)
                     (->> lines
                          (-map 's-trim)
                          (s-join "\n  ")
                          (s-concat "  ")
                          (format "%s#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"
				  (if ob-ipython-show-mime-types "# text/plain\n" "")))
		   (s-concat
		    (if ob-ipython-show-mime-types "\n# text/plain\n: "
		      ": ")
		    (car lines)))))))
    (or (-when-let (val (cdr (assoc 'text/org values))) (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values))) (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values))) (funcall svg val))
	(-when-let (val (cdr (assoc 'text/html values))) (funcall html val))
	(-when-let (val (cdr (assoc 'text/latex values))) (funcall latex val))
        (-when-let (val (cdr (assoc 'text/plain values))) (funcall txt val)))))

;; I want an option to get exceptions in the buffer
(defun ob-ipython--eval (service-response)
  (let ((status (ob-ipython--extract-status service-response)))
    (cond ((string= "ok" status) `((:result . ,(ob-ipython--extract-result service-response))
                                   (:output . ,(ob-ipython--extract-output service-response))
                                   (:exec-count . ,(ob-ipython--extract-execution-count service-response))))
          ((string= "abort" status) (error "Kernel execution aborted."))
          ((string= "error" status)
	   (if ob-ipython-exception-results
	       (let ((error-content
		      (->> service-response
			   (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply")
							      (cdr (assoc 'msg_type msg)))))
			   car
			   (assoc 'content)
			   cdr)))
		 `((:result . ,(ob-ipython--extract-result service-response))
		   (:output . ,(org-no-properties
				(ansi-color-apply
				 (s-join "\n" (cdr (assoc 'traceback error-content))))))
		   (:exec-count . ,(ob-ipython--extract-execution-count service-response))))
	     (error (ob-ipython--extract-error service-response)))))))

;; I also want q to go to the offending line
(defun ob-ipython--create-traceback-buffer (traceback)
  "Create a traceback buffer.
Note, this does not work if you run the block async."
  (let ((current-buffer (current-buffer))
	(src (org-element-context))
	(buf (get-buffer-create "*ob-ipython-traceback*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (pop-to-buffer buf)
    (let ((line (re-search-backward "-*> *\\([0-9]*\\) " nil t))
	  line-number)
      (when line
	(setq line-number (string-to-number (match-string 1)))
	(local-set-key "q" `(lambda ()
			      (interactive)
			      (quit-restore-window nil 'bury)
			      (pop-to-buffer ,current-buffer)
			      (goto-char ,(org-element-property :begin src))
			      (forward-line ,line-number)))))))

;; ** inspect from an org buffer
;; This makes inspect work from an org-buffer.

(defun ob-ipython-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (let ((return (org-in-src-block-p))
	(inspect-buffer))
    (when return
      (org-edit-src-code nil "*ob-ipython-src-edit-inspect*"))
    (let ((code (with-current-buffer buffer
		  (buffer-substring-no-properties (point-min) (point-max)))))
      (-if-let (result (->> (ob-ipython--inspect code pos)
			    (assoc 'text/plain)
			    cdr))
	  (setq inspect-buffer (ob-ipython--create-inspect-buffer result))
	(message "No documentation was found. Have you run the cell?")))

    (when return
      (with-current-buffer "*ob-ipython-src-edit-inspect*"
	(org-edit-src-exit)))
    (when inspect-buffer (pop-to-buffer inspect-buffer))))




(provide 'scimax-org-babel-ipython-upstream)

;;; scimax-org-babel-ipython-upstream.el ends here
