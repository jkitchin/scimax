;;; scimax-build.el --- Build capability for org-mode

;;; Commentary:
;; The idea here is to allow an easy build process for org-files. When you run
;; `scimax-build' it will look in the current org-file for a block named "build"
;; and run it. If that is not found, then it looks for a makefile in the current
;; directory and runs the default target.
;;
;; This is a WIP. [2020-02-12 Wed]


(defun scimax-build ()
  "Run build commands for the current document.

1. Run a codeblock named build in the document
2. Run the emacs compile command?
"
  (interactive)
  (save-buffer)
  (save-excursion
    (cond
     ((not (stringp (org-babel-goto-named-src-block "build")))
      (org-babel-execute-src-block))
     ((or (file-exists-p "Makefile")
	  (file-exists-p "makefile"))
      (compile "make"))
     (t
      (message "I don't know how to build this. Please create a Makefile, or a src-block named \"build\".")))))

(provide 'scimax-build)

;;; scimax-build.el ends here
