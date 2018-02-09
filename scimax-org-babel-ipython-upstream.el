;;; scimax-org-babel-ipython-upstream.el --- Modifications to the upstream ob-ipython module

;;; Commentary:
;;

(require 'scimax-ob)

(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer."
  :group 'ob-ipython)

(add-to-list 'org-structure-template-alist
	     '("ip" "#+BEGIN_SRC ipython\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))

(add-to-list 'org-structure-template-alist
	     '("ipv" "#+BEGIN_SRC ipython :results value\n?\n#+END_SRC"
	       "<src lang=\"python\">\n?\n</src>"))


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


(defun scimax-ob-ipython-close ()
  "Cleanup function for when buffer closes to kill the kernel."
  ;; first we kill the kernel
  (let ((bf (format "*ob-ipython-kernel-%s*"
		    (org-babel-get-session))))
    (when (get-buffer bf)
      (kill-buffer bf))))

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

    (add-hook 'kill-buffer-hook #'scimax-ob-ipython-close t t))

  (ob-ipython--clear-output-buffer)
  (if (assoc :async params)
      (ob-ipython--execute-async body params)
    (ob-ipython--execute-sync body params)))


;; ** Fine tune the output of blocks
;; This gives me the output I want.
(defun ob-ipython--process-response (ret file result-type)
  (let* ((result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 (value (cdr (assoc :value result)))
	 (display (cdr (assoc :display result))))
    (s-concat
     (format "# Out[%d]:\n" (cdr (assoc :exec-count ret)))
     output
     (s-join "\n" (loop for (type . value) in (append value display)
			do
			(message "Rendering %s" (cons type (cond
							    ((eq type 'image/png)
							     "<img data>")
							    (t value))))
			collect
			(ob-ipython--render file (list (cons type value))))))))

;; I added html and latex to this
(defun ob-ipython--render (file-or-nil values)
  (let ((org (lambda (value) (format "org: %s" value)))
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
