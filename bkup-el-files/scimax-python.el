;;; scimax-python.el --- Scimax python utilities

;;; Commentary:
;; 

;; utilities
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
      '("-r no "		 ; no reports
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
	(n)				; for line number
	(cn)				; column number
	(content)			; error on line
	(pb "*pylint*")
	(link)
	(tempfile))

    (unless (executable-find "pylint")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pylint'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['pylint'])\"")))

    ;; rm buffer if it exists
    (when (get-buffer pb) (kill-buffer pb))

    ;; only run if in a python code-block. this doesn't work right for
    ;; jupyter-python. you would have to tangle all the blocks out
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
	(org-cycle '(64))		; open everything
	;; make read-only and press q to quit
	(setq buffer-read-only t)
	(use-local-map (copy-keymap org-mode-map))
	(local-set-key "q" #'(lambda () (interactive) (kill-buffer)))
	(switch-to-buffer-other-window cb))
      ;; final cleanup and delete file
      (delete-file tempfile))))

(provide 'scimax-python)

;;; scimax-python.el ends here
