;;; scimax-jupyter.el --- scimax customization for emacs-jupyter

;;; Commentary:
;; This provides some fine-tuning of key bindings, and loads jupyter for scimax.

(require 'scimax-ob)
(require 'jupyter)

;; * automatic buffer kernels

;; (defvar scimax-jupyter-buffer-specific-kernel-p t
;;   "If non-nil, use buffer-specific kernels.")


;; (defvar scimax-jupyter-kill-kernel-on-exit t
;;   "If non-nil kill local kernels.")


;; (defun scimax-jupyter-initiate-session-advice (orig-func &optional session params)
;;   "Initialize a Jupyter SESSION according to PARAMS.
;; Also saves current session for killing later."
;;   (when (equal session "none")
;;     (setq session (if-let ((fname (buffer-file-name)))
;; 		      (md5 fname)
;; 		    "jupyter")))
;;   ;; jrk added to store current client. This assumes only one session per file,
;;   ;; which is not required, you can have more than one, but then they will not
;;   ;; all be killed. you would have to manually name these though.
;;   (setq header-line-format (format "session - %s" session))
;;   (prog1
;;       (setq jupyter-current-client
;; 	    (org-babel-jupyter-initiate-session-by-key session params))
;;     (when scimax-jupyter-kill-kernel-on-exit
;;       (add-hook 'kill-buffer-hook 'scimax-jupyter-kill-kernel-hook nil t))))


;; (defun scimax-jupyter-session-key-advice (orig-func params)
;;   "Return a string that is the concatenation of the :session and :kernel PARAMS.
;; PARAMS is the arguments alist as returned by
;; `org-babel-get-src-block-info'.  The returned string can then be
;; used to identify unique Jupyter Org babel sessions."
;;   (let ((session (alist-get :session params))
;;         (kernel (alist-get :kernel params)))
;;     (when (equal session "none")
;;       (setq session (if-let ((fname (buffer-file-name)))
;; 			(md5 fname)
;; 		      "jupyter")))
;;     (unless (and session kernel
;;                  (not (equal session "none")))
;;       (error "Need a valid session and a kernel to form a key"))
;;     (concat session "-" kernel)))


;; (when scimax-jupyter-buffer-specific-kernel-p
;;   (advice-add 'org-babel-jupyter-initiate-session :around 'scimax-jupyter-initiate-session-advice)
;;   (advice-add 'org-babel-jupyter-session-key :around 'scimax-jupyter-session-key-advice))


;; (defun scimax-jupyter-kill-kernel-hook ()
;;   "Hook function to kill kernel when a buffer is killed."
;;   (when jupyter-current-client
;;     (kill-buffer jupyter-current-client)
;;     (setq jupyter-current-client nil)))



;; * scimax jupyter header defaults

(setq org-babel-default-header-args:jupyter-python
      '((:results . "value")
	(:session . "jupyter")
	(:kernel . "python3")
	(:pandoc . t)
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))

(setq org-babel-default-header-args:jupyter-julia
      '((:results . "value")
	(:session . "jupyter")
	(:kernel . "julia-1.6")
	(:pandoc . t)
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))


(add-to-list 'org-babel-load-languages '(jupyter . t) t)

;; https://datatofish.com/add-julia-to-jupyter/
;;  for setup
(add-to-list 'org-babel-load-languages '(julia . t) t)
(setq inferior-julia-program-name "/usr/local/bin/julia")

(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;; * syntax highlighting
(add-to-list 'org-src-lang-modes '("jupyter-python" . python))
(add-to-list 'org-src-lang-modes '("jupyter-julia" . julia))


;; see [[nb:scimax::scimax-org-babel-ipython-upstream.el::c3312]]
;; for more definitions.
;; (cl-loop for (key . def) in '(("C-<return>" . org-ctrl-c-ctrl-c)
;; 			      ("M-<return>" . (lambda ()
;; 						(interactive)
;; 						(scimax-ob-execute-and-next-block t)))
;; 			      ("S-<return>" . scimax-ob-execute-and-next-block)

;; 			      ("H-w" . org-babel-previous-src-block)
;; 			      ("H-s" . org-babel-next-src-block)
;; 			      ("H-j" . scimax-ob-jump-to-block)

;; 			      ("H--" . scimax-ob-split-src-block)
;; 			      ("H-m" . scimax-ob-merge-blocks)
;; 			      ("H-l" . org-babel-remove-result)
;; 			      ("H-L" . scimax-ob-clear-all-results))
;; 	 do (jupyter-org-define-key (kbd key) def))

;; * make old ipython blocks work with jupyter python

(defalias 'org-babel-execute:ipython 'org-babel-execute:jupyter-python)
(setq org-babel-default-header-args:ipython org-babel-default-header-args:jupyter-python)
(add-to-list 'org-src-lang-modes '("ipython" . python))

(provide 'scimax-jupyter)

;;; scimax-jupyter.el ends here
