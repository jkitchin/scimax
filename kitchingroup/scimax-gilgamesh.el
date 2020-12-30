;;; scimax-gilgamesh.el --- use scimax with jupyter on gilgamesh

;;; Commentary:
;;
;; You need to have passwordless ssh setup with gilgamesh.
;; Setup ~/.ssh/config like this:
;; HOST gilgamesh
;;     HostName gilgamesh.cheme.cmu.edu
;;     User {userid}
;;
;; Then if you don't have ~/.ssh/id_rsa.pub run this command
;; > ssh-keygen -t rsa
;;
;; Finally, copy the pub file to gilgamesh. This will require a password
;; > cat .ssh/id_rsa.pub | ssh gilgamesh 'cat >> .ssh/authorized_keys'
;;
;; Now you should be able to run
;; > ssh gilgamesh
;; and login without a password.
;;
;; Use this in your ipython header:
;; #+BEGIN_SRC ipython :session (scimax-gilgamesh-kernel)
;;
;; #+END_SRC
;;
;; All the ssh connections will be made for you, and should be closed when you
;; kill the buffer.
;; Limitations:
;; 1. You can only run one kernel at a time.
;; 2. This uses 6 ssh connections, there may be limitations on the
;; number you can have on gilgamesh.


(require 's)

(defvar scimax-gilgamesh-ssh-sockets '()
  "List of ssh connections we need to clean up.
Each entry has a (label :socket socket :buffer buffer :port port).
socket is the ssh socket.")


(defun scimax-gilgamesh-kill-kernel ()
  "Kill all the processes associated with ipython on gilgamesh."
  (interactive)
  (let (entry socket buffer kernel-file label port)
    (while (setq entry (pop scimax-gilgamesh-ssh-sockets))
      (setq
       label (car entry)
       socket (plist-get (cdr entry) :socket)
       buffer (plist-get (cdr entry) :buffer)
       kernel-file (plist-get (cdr entry) :kernel-file))
      ;; kill remote process
      (when (eq label 'control_port)
	(setq port (plist-get (cdr entry) :port))
	;; find process of port listening, and then kill it.
	(let ((s (car (last (split-string
			     (shell-command-to-string
			      (format
			       "ssh gilgamesh \"/usr/sbin/lsof -i tcp:%s\"" port))
			     "\n" t))))
	      pid)
	  (when (s-starts-with? "ipython" s)
	    (setq pid (second (split-string s " " t)))
	    (shell-command-to-string
	     (format
	      "ssh gilgamesh \"kill %s\"" pid)))))
      ;; clean up kernel file
      (when (and kernel-file (file-exists-p kernel-file))
	(delete-file kernel-file))
      (message "running %S" (format "ssh -S %s -O exit gilgamesh" socket))
      (shell-command (format "ssh -S %s -O exit gilgamesh" socket))
      (when (get-buffer buffer)
	(kill-buffer buffer)))))


(defun scimax-gilgamesh-kernel ()
  "Open a remote kernel, make a local setup, and return the name of the kernel."
  (unless (eq major-mode 'org-mode)
    (error "You can only start a gilgamesh kernel in an org-file."))

  (let* ((cb (current-buffer))
	 (cw (current-window-configuration))
	 (gilgamesh-buf (concat "*gilgamesh-" (buffer-name) "*"))
	 (kernel-file)
	 (kernel-data))
    ;; We check if we have a kernel running remotely
    (if-let ((kernel-file (plist-get (cdr (assoc 'gilgamesh scimax-gilgamesh-ssh-sockets)) :kernel-file)))
	kernel-file
      ;; we don't have one, so we make one.
      (message "Starting remote kernel in %s" gilgamesh-buf)
      (async-shell-command
       (format	"ssh -t -M -S ~/gilgamesh-socket gilgamesh \"source ~/.bashrc; ipython kernel\"")
       gilgamesh-buf)
      (setq kernel-file
	    (catch 'ready
	      (while t
		(with-current-buffer gilgamesh-buf
		  (goto-char (point-min))
		  (when (re-search-forward
			 "--existing \\(kernel-[0-9]+.json\\)" nil t)
		    (throw 'ready (match-string 1)))
		  ;; little delay to not loop so fast
		  (sleep-for 0.1)))))
      (push (list 'gilgamesh
		  :socket "~/gilgamesh-socket"
		  :buffer gilgamesh-buf
		  :kernel-file kernel-file)
	    scimax-gilgamesh-ssh-sockets)

      (shell-command (format
		      "scp gilgamesh:~/.local/share/jupyter/runtime/%s ."
		      kernel-file))
      (message "copied remote run file to local.")

      ;; Now, we manually set up the tunnels. I don't need this on my Mac, but
      ;; on Windows, it doesn't seem to work if we don't.
      (setq kernel-data (json-read-file kernel-file))
      (cl-loop for (key . val) in kernel-data
	       when (s-contains? "_port" (symbol-name key))
	       do
	       (async-shell-command
		(format
		 "ssh -M -S ~/gilgamesh-port-%s -N -f -L localhost:%s:localhost:%s gilgamesh"
		 val val val)
		(format "*gilgamesh-port-%s*" val))
	       (message "*gilgamesh-port-%s* is setup." val)
	       (push (list key
			   :port val
			   :socket (format "~/gilgamesh-port-%s" val)
			   :buffer (format "*gilgamesh-port-%s*" val))
		     scimax-gilgamesh-ssh-sockets))

      (message "Ready to go with %s" kernel-file)
      (with-current-buffer cb
	(setq header-line-format
	      (format "Running on gilgamesh %s. Click to end." kernel-file))
	(local-set-key [header-line down-mouse-1]
		       `(lambda ()
			  (interactive)
			  (scimax-gilgamesh-kill-kernel)
			  (setq header-line-format nil)
			  (redraw-frame)))

	(add-hook 'kill-buffer-hook `(lambda ()
				       (interactive)
				       (scimax-gilgamesh-kill-kernel)
				       (setq header-line-format nil))
		  nil t))
      (set-window-configuration cw)
      kernel-file)))

(defun scimax-gilgamesh-jupyter ()
  "Open a browser running a jupyter notebook on gilgamesh."
  (interactive)

  (let* ((gilgamesh-buf "*gilgamesh-jupyter*")
	 (local-buf "*local-jupyter*")
	 port
	 token)

    ;; This launches the remote notebook

    ;; Check when it is ready
    (unless (and (get-buffer gilgamesh-buf)
		 (with-current-buffer (get-buffer gilgamesh-buf)
		   (goto-char (point-min))
		   (re-search-forward "http://localhost:\\([0-9]\\{4\\}\\)/\\?token=\\(.*\\)" nil t)))
      ;; we need a remote kernel
      (message "Starting remote notebook in %s" gilgamesh-buf)
      (async-shell-command
       "ssh gilgamesh \"source ~/.bashrc; jupyter notebook --no-browser\""
       gilgamesh-buf)

      (catch 'ready
	(while t
	  (with-current-buffer gilgamesh-buf
	    (goto-char (point-min))
	    (when (re-search-forward "http://localhost:\\([0-9]\\{4\\}\\)/\\?token=\\(.*\\)" nil t)
	      (setq port (match-string 1)
		    token (match-string 2))
	      (throw 'ready t))
	    ;; little delay to not loop so fast
	    (sleep-for 0.1))))

      ;; Now we create the tunnel
      (async-shell-command (format "ssh -t -L localhost:%s:localhost:%s gilgamesh"
				   port port)
			   local-buf)

      (with-current-buffer local-buf
	(setq header-line-format
	      (format "Running on gilgamesh. Kill this buffer to stop it."))
	(add-hook 'kill-buffer-hook
		  `(lambda ()
		     ;; kill gilgamesh buffer
		     (kill-buffer ,gilgamesh-buf)
		     ;; kill the remote notebook
		     (shell-command (format "ssh gilgamesh \"/usr/sbin/lsof -ti:%s | xargs kill\"" ,port)))
		  nil t))

      (pop-to-buffer local-buf)

      ;; Finally, open the notebook
      (browse-url (format "http://localhost:%s/?token=%s" port token)))))


(provide 'scimax-gilgamesh)

;;; scimax-gilgamesh.el ends here
