;;; scimax-org-babel-ipython-upstream.el --- Modifications to the upstream ob-ipython module

;;; Commentary:
;;

(require 'scimax-ob)

(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer."
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




(add-to-list 'scimax-src-block-keymaps
	     `("ipython" . ,(let ((map (make-composed-keymap
					'()
					org-mode-map)))
			      ;; In org-mode I define RET so we redefine them here
			      (define-key map (kbd "<return>") 'newline)
			      (define-key map (kbd "C-c C-c") 'org-ctrl-c-ctrl-c)
			      ;; Make C-enter execute block like jupyter to execute block
			      (define-key map (kbd "C-<return>") 'org-ctrl-c-ctrl-c)
			      (define-key map (kbd "S-<return>") 'scimax-execute-and-add-new-block)
			      (define-key map (kbd "H-=") 'scimax-insert-src-block)
			      (define-key map (kbd "H-/") 'ob-ipython-inspect)
			      map)))


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
      (add-to-list 'org-babel-default-header-args:ipython
		   (cons :session session-name)))

    ;; (add-hook 'kill-buffer-hook #'ob-ipython-kill-kernel t t)
    )

  (ob-ipython--clear-output-buffer)
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
	 (display (when display-params (mapcar 'intern-soft (list display-params)))))
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
	 (display (when display-params (mapcar 'intern-soft (list display-params)))))
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
     output
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
	       value))
        (png (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)
		(format "#+BEGIN_EXPORT html\n%s\n#+END_EXPORT" value)))
	(latex (lambda (value)
		 (format "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT" value)))
        (txt (lambda (value)
               (let ((lines (s-lines value)))
                 (if (cdr lines)
                     (->> lines
                          (-map 's-trim)
                          (s-join "\n  ")
                          (s-concat "  ")
                          (format "#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE"))
                   (s-concat ": " (car lines)))))))
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
			      (forward-line ,(+ 1 line-number))))))))

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
