;;; scimax-nersc.el --- Functions for working with scimax at NERSC

;;; Commentary:
;; This library helps us use scimax locally on NERSC.

;; You should define a kernel in your NERSC account that you want to use and an environment.
;; This is roughly what I did while logged in at NERSC
;; > conda create -n myenv python=3 numpy
;; > pip install ipykernel
;; > python -m ipykernel install --user --name myenv --display-name MyEnv

;; It assumes you can login to NERSC without a password. For example you should
;; be able to run this command in a shell without getting a password prompt
;;
;; ssh nersc "jupyter kernel --kernel=myenv"

;; To do that, you need to follow the directions at
;; https://docs.nersc.gov/connect/mfa/#sshproxy to get the sshproxy.sh command
;; and then add something like this to ~/.ssh/config
;;
;; HOST nersc
;;     HostName cori.nersc.gov
;;     User jkitchin
;;     IdentityFile ~/.ssh/nersc
;;
;; You use sshproxy.sh to login once, and then you should be password free for
;; one day.
;;
;; At this time, you can only run one remote kernel per buffer. I don't see a good
;; reason to make it easy to do more than one remote kernel per buffer at this time.
;;
;; This should be well-behaved, and not leave kernels running after you close
;; the org-buffer. If it does you can always clean up your you may need to run
;; this command, but it will kill them all. ssh nersc "pkill -9 -f
;; jupyter-kernel" to kill these. To fix this, I need to have the pid I think,
;; so I can kill it.

(require 'scimax-jupyter)


(defcustom scimax-nersc-kernel "myenv"
  "Name of kernel to start at NERSC")


(defun scimax-nersc-kernel ()
  "Get a jupyter kernel at nersc.
This looks for an existing kernel in a nersc-buffer and reuses it if there.
If not, it tries to start one.
Returns the string you need to put in the :session parameter of a src block."
  (unless (eq major-mode 'org-mode)
    (error "You can only start a nersc kernel in an org-file."))
  (let* ((cb (current-buffer))
	 (nersc-buf (concat "nersc-"(buffer-name)))
	 (buf (get-buffer nersc-buf))
	 (cw (current-window-configuration))
	 kernel-file)
    (if buf
	(with-current-buffer buf
	  (goto-char (point-min))
	  (re-search-forward "\\[KernelApp\\] Connection file: \\(.*.json\\)")
	  (format "/ssh:nersc:%s" (match-string-no-properties 1)))
      (message "No kernel found. Starting one. Be patient. You may need to login.")
      (pop-to-buffer nersc-buf)
      (async-shell-command
       ;; Note: -t makes sure the kernel dies when you close the buffer.
       (format "ssh -t nersc \"jupyter kernel --kernel=%s\""
	       scimax-nersc-kernel)
       nersc-buf)
      ;; The output is slow, so we loop until we find the relevant text then throw it back.
      (catch 'kernel
	(while t
	  (goto-char (point-min))
	  (when (re-search-forward "\\[KernelApp\\] Connection file: \\(.*.json\\)" nil t)
	    (setq kernel-file (match-string-no-properties 1)
		  header-line-format "press q to kill kernel.")
	    (local-set-key "q"
			   `(lambda ()
			      (interactive)
			      (kill-buffer ,nersc-buf)))
	    (with-current-buffer cb
	      (setq header-line-format
		    (format "Running on nersc %s. Click to close."
			    kernel-file))
	      (local-set-key [header-line down-mouse-1]
			     `(lambda ()
				(interactive)
				(save-window-excursion
				  (kill-buffer ,nersc-buf))
				(setq header-line-format nil)))

	      ;; This local hook should also kill the remote kernel when you
	      ;; kill this buffer
	      (add-hook 'kill-buffer-hook `(lambda () (kill-buffer ,nersc-buf)) nil t))
	    (set-window-configuration cw)
	    (message "%s running. Waiting 1 second for it to start up remotely."
		     kernel-file)
	    (sleep-for 1)
	    (throw 'kernel (format "/ssh:nersc:%s" kernel-file)))
	  ;; a little delay so we don't loop too fast
	  (sleep-for 0.1))))))


(defun scimax-nersc-kill-kernel ()
  "Kill the kernel in the current buffer."
  (interactive)
  (let ((nersc-buf (concat "nersc-"(buffer-name))))
    (when (get-buffer nersc-buf) (kill-buffer nersc-buf)))
  (setq header-line-format nil))


(defun scimax-nersc-kill-all-kernels ()
  "Kill all jupyter kernels at nersc"
  (interactive)
  (when
      (y-or-n-p "Really kill all your kernels?")
    (shell-command "ssh nersc \"pkill -9 -f jupyter-kernel\"")))


(provide 'scimax-nersc)

;;; scimax-nersc.el ends here
