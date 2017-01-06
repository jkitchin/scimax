;;; scimax-latex.el --- Utilities to check the LaTeX setup


;;; Commentary:
;; 

;;; Code:

(defvar tlmgr-installed-packages nil
  "Cached list of installed LaTeX packages.")


(defun tlmgr-installed (&optional refresh)
  "Get a list of installed LaTeX packages. Uses a cached value if
possible unless REFRESH is non-nil."
  (unless (or tlmgr-installed-packages refresh)
    (setq tlmgr-installed-packages
	  (mapcar (lambda (s)
		    (split-string (substring s 2) ":" t))
		  (split-string
		   (shell-command-to-string "tlmgr list --only-installed") "\n" t))))
  tlmgr-installed-packages)


(defun texdoc (package)
  "Run texdoc on the PACKAGE."
  (interactive (list (completing-read "Package: " (tlmgr-installed))))
  (shell-command (format "texdoc %s" package)))


(defun kpsewhich (symbol)
  "Run kpsewhich on SYMBOL."
  (interactive "sSymbol: ")
  (message (shell-command-to-string (format "kpsewhich %s" symbol))))


(defun scimax-latex-setup ()
  "Display buffer with LaTeX setup information."
  (interactive)
  (message "Please wait while I gather some information. This can take a while.")
  (with-current-buffer (get-buffer-create "*scimax-latex-setup*")
    (erase-buffer)
    (org-mode)
    (insert (s-format "#+TITLE: LaTeX setup

This file describes how LaTeX is setup on your computer.

* Executables

latex: ${(executable-find \"latex\")}
pdflatex: ${(executable-find \"pdflatex\")}
bibtex: ${(executable-find \"bibtex\")}
biber: ${(executable-find \"biber\")}

tlmgr: ${(executable-find \"tlmgr\")}
kpsewhich: ${(executable-find \"kpsewhich\")}
texdoc: ${(executable-find \"texdoc\")}

Configuration:
${(shell-command-to-string \"tlmgr conf texmf\")}

* Latex classes org-mode knows about

Here are some relevant variables
help:org-format-latex-header
help:org-latex-default-packages-alist
help:org-latex-packages-alist
help:org-latex-pdf-process

Note: Not every class has a corresponding style file. Click on the texdoc link to learn more about the class.

Missing files should be installed in the TEXMFHOME directory listed above. See https://en.wikibooks.org/wiki/LaTeX/Installing_Extra_Packages for help.

"
		      (lambda (arg &optional extra)
			(eval (read arg)))))
    (loop for (org-name header-string cls) in
	  (-uniq (loop for latex-class in org-latex-classes
		       collect
		       (list (car latex-class)
			     (nth 1 latex-class)
			     (let ((header-string (nth 1 latex-class)))
			       (when (string-match "documentclass.*?{\\(.*?\\)}" header-string)
				 (match-string 1 header-string))))))
	  do
	  (let ((cls-path (s-trim (shell-command-to-string (format "kpsewhich %s.cls" cls))))
		(sty-path (s-trim (shell-command-to-string (format "kpsewhich %s.sty" cls)))))
	    (insert (s-format "
** ${org-name} creates documents with this LaTeX documentclass: ${cls}
This is the header that is expanded.

${header-string}

LaTeX path for class: [[${cls-path}]]

 [[elisp:(shell-command \"texdoc ${cls}\"][texdoc ${cls}]]

Latex style path: [[${sty-path}]]
 
" 
			      (lambda (arg &optional extra)
				(eval (read arg)))))))

    (insert "* org-mode default latex packages\n\n")
    (loop for (options package snippet compilers) in org-latex-default-packages-alist
	  do
	  (insert (s-format "- ${package} (options=${options}) [[elisp:(shell-command \"texdoc ${package}\"][texdoc ${package}]]\n"
			    (lambda (arg &optional extra)
			      (eval (read arg))))))

    (insert "\n* org-mode defined latex packages\n\n")
    (loop for (options package snippet compilers) in org-latex-packages-alist
	  do
	  (insert (s-format "- ${package} [${options}] [[elisp:(shell-command \"texdoc ${package}\"][texdoc ${package}]]\n"
			    (lambda (arg &optional extra)
			      (eval (read arg))))))

    (insert "\n\n* org-mode LaTeX compiling setup\n\n")
    (insert (format "org-latex-pdf-process = \"%s\"\n" org-latex-pdf-process))
    (if (functionp org-latex-pdf-process)
	(insert "%s" (describe-function org-latex-pdf-process))))

  (switch-to-buffer "*scimax-latex-setup*")
  (goto-char (point-min)))

(provide 'scimax-latex)

;;; scimax-latex.el ends here
