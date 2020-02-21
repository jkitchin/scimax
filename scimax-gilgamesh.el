;;; scimax-gilgamesh.el --- use scimax with jupyter on gilgamesh

;;; Commentary:
;;
;; HOST gilgamesh
;;     HostName gilgamesh.cheme.cmu.edu
;;     User jkitchin
;;
;; :session ./gilgamesh-ssh.json

(require 's)


(when (require 'jupyter nil 'noerror)
  (require 'scimax-jupyter))



(defcustom scimax-gilgamesh-username user-login-name
  "User ID for gilgamesh. Defaults to `user-login-name'")


(defun scimax-gilgamesh-kernel ()
  (unless (eq major-mode 'org-mode)
    (error "You can only start a gilgamesh kernel in an org-file."))

  (let* ((cb (current-buffer))
	 (gilgamesh-buf (concat "gilgamesh-" (buffer-name)))
	 (local-buf (concat "local-" (buffer-name)))
	 (buf (get-buffer gilgamesh-buf))
	 (cw (current-window-configuration)))

    ;; This launches the remote kernel.
    ;; Check when it is ready
    (unless (and (get-buffer gilgamesh-buf)
		 (with-current-buffer (get-buffer gilgamesh-buf)
		   (goto-char (point-min))
		   (re-search-forward "--existing gilgamesh.json" nil t)))
      ;; we need a remote kernel
      (message "Starting remote kernerl in %s" gilgamesh-buf)
      (async-shell-command
       "ssh -t gilgamesh \"source ~/.bashrc; ipython kernel -f gilgamesh.json\""
       gilgamesh-buf)

      (catch 'ready
	(while t
	  (with-current-buffer gilgamesh-buf
	    (goto-char (point-min))
	    (when (re-search-forward "--existing gilgamesh.json" nil t)
	      (throw 'ready t))
	    ;; little delay to not loop so fast
	    (sleep-for 0.1))))

      ;; Then we copy the run file here.
      (shell-command "scp gilgamesh:~/.local/share/jupyter/runtime/gilgamesh.json .")
      (message "copied remote run file to local."))

    ;; Now the local setup
    (unless (and (get-buffer local-buf)
		 (with-current-buffer (get-buffer local-buf)
		   (goto-char (point-min))
		   (re-search-forward "--existing gilgamesh-ssh.json" nil t)))
      (message "Starting local connection in %s" local-buf)
      ;; This starts the local kernel we connect to
      (async-shell-command "ipython console --existing ./gilgamesh.json --ssh gilgamesh"
			   local-buf)

      (catch 'ready
	(while t
	  (with-current-buffer local-buf
	    (goto-char (point-min))
	    (when (re-search-forward "--existing gilgamesh-ssh.json" nil t)
	      (throw 'ready t))
	    ;; little delay to not loop so fast
	    (sleep-for 0.1)))))

    (message "Ready for action.")

    ;; make cleanup functions
    (with-current-buffer cb
      (setq header-line-format
	    (format "Running on gilgamesh. Click to close."))
      (local-set-key [header-line down-mouse-1]
		     `(lambda ()
			(kill-buffer ,gilgamesh-buf)
			(kill-buffer ,local-buf)
			(delete-file "gilgamesh.json")
			;; kill repl buffer

			(setq header-line-format nil)))


      (add-hook 'kill-buffer-hook `(lambda ()
				     (kill-buffer ,gilgamesh-buf)
				     (kill-buffer ,local-buf)
				     (delete-file "gilgamesh.json")
				     ;; kill repl buffer

				     (setq header-line-format nil))
		nil t)))
  ;; return session name used. It is a constant
  "./gilgamesh-ssh.json")

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
