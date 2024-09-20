;;; scimax-org-eldoc.el --- Generate org-mode elisp documentation


;;; Commentary:
;;

;;; Code:

(defun scimax-org-eldoc-org-escape (s)
  "Escape some org syntax in S.
Headlines and # in the first position."
  (if (stringp s)
      (progn
	(setq s (replace-regexp-in-string "^*" ",*" s))
	(setq s (replace-regexp-in-string "^#" ",#" s))
	s)
    (format "%s" s)))


(defun scimax-org-eldoc (library)
  "Generate documentation for LIBRARY in an org buffer.
LIBRARY must be loaded before running this function."
  (interactive
   (list (completing-read
	  "Library to generate org-doc for: "
	  (-flatten
	   (delq nil
		 (mapcar
		  (lambda (x)
		    (delq nil
			  (mapcar
			   (lambda (y)
			     (when (and (consp y)
					(eq (car y) 'provide))
			       (cdr y)))
			   x)))
		  load-history))))))

  (let* ((lib-file (locate-library library))
	 ;; these are the things defined in the library
	 (elements (cdr
		    (assoc
		     (locate-library library)
		     load-history)))
	 ;; variables
	 (vars (-filter 'symbolp elements))
	 ;; things the library requires
	 (requires
	  (mapcar 'cdr
		  (-filter (lambda (x)
			     (and (consp x)
				  (eq 'require (car x))))
			   elements)))
	 ;; functions defined in the library
	 (funcs (mapcar
		 'cdr
		 (-filter (lambda (x)
			    (and (consp x)
				 (eq 'defun (car x))))
			  elements))))

    (switch-to-buffer "*org-doc*")
    (erase-buffer)
    (insert (format "#+TITLE: Documentation for %s
#+OPTIONS: toc:nil
\\maketitle
\\tableofcontents

%s

" library  (cond
	    ;; regular lisp file
	    ((string= "el" (file-name-extension lib-file))
	     (format "Source code: [[file:%s][%s]]" lib-file library))
	    ;; compiled file. these are not easy to read so we try plain el file
	    ((and (string= "elc" (file-name-extension lib-file))
		  (file-exists-p
		   (concat (file-name-sans-extension lib-file) ".el")))
	     (format "Source code: [[file:%s][%s]]"
		     (concat (file-name-sans-extension lib-file) ".el")
		     library))
	    ;; catch anything we cannot figure out
	    (t
	     (format "Source code: file:%s" lib-file)))))


    (insert "* Requires\n\n")
    ;; insert link to generate a scimax-org-eldoc buffer for each require
    (dolist (req requires)
      (insert (format "- [[elisp:(scimax-org-eldoc \"%s\")][%s]]\n" req req)))

    (insert "* Custom Variables\n\n")
    (dolist (var (sort (-filter 'custom-variable-p vars) 'string-lessp))
      (insert (format "** %s
Documentation: %s

Value:
%S\n\n"
		      var
		      (scimax-org-eldoc-org-escape
		       (documentation-property var 'variable-documentation))
		      (scimax-org-eldoc-org-escape (symbol-value var)))))

    (insert "* Regular Variables\n\n")
    (dolist (var (sort (-filter (lambda (x)
				  (not (custom-variable-p x)))
				vars)
		       'string-lessp))
      (insert (format "** %s\n" var))
      (insert (scimax-org-eldoc-org-escape
	       (format "Documentation: %s

Value:
%s\n\n"
		       (documentation-property var 'variable-documentation)
		       (symbol-value var)))))

    (insert "* Interactive Functions\n\n")

    (dolist (func (sort (-filter 'commandp funcs) 'string-lessp))
      (insert (format "** %s %s
Documentation: %s

Code:
#+BEGIN_SRC emacs-lisp
%s
#+END_SRC

"
		      func
		      (or (help-function-arglist func) "")

		      (scimax-org-eldoc-org-escape (documentation func))
		      ;; code defining the function
		      (let ((code (save-window-excursion
				    ;; we do not have c-source, so check if func
				    ;; is defined in a c file here.
				    (if (and (stringp (find-lisp-object-file-name
						       func
						       (symbol-function func)))
					     (string= "c"
						      (file-name-extension
						       (find-lisp-object-file-name
							func
							(symbol-function func)))))
					(symbol-function func)
				      ;;else
				      (condition-case nil
					  (let ((bp (find-function-noselect
						     func t)))
					    (set-buffer (car bp))
					    (goto-char (cdr bp))
					    (when (sexp-at-point)
					      (mark-sexp)
					      (buffer-substring (point) (mark))))
					(error func))))))
			(scimax-org-eldoc-org-escape code)))))

    (insert "* Non-interactive Functions\n\n")

    (dolist (func (sort (-filter (lambda (x) (not (commandp x)))
				 funcs)
			'string-lessp))
      (insert (format "** %s %s
Documentation: %s

Code:
#+BEGIN_SRC emacs-lisp
%s
#+END_SRC

"
		      func
		      (or (help-function-arglist func) "")
		      ;; escape some org-syntax
		      (scimax-org-eldoc-org-escape (documentation func))
		      ;; code defining the function
		      (let ((code (save-window-excursion
				    ;; we do not have c-source, so check if func
				    ;; is defined in a c file here.
				    (if
					(and (stringp (find-lisp-object-file-name
						       func
						       (symbol-function func)))
					     (string= "c"
						      (file-name-extension
						       (find-lisp-object-file-name
							func
							(symbol-function func)))))
					(symbol-function func)
				      ;;else
				      (condition-case nil
					  (let ((bp (find-function-noselect func t)))
					    (set-buffer (car bp))
					    (goto-char (cdr bp))
					    (when (sexp-at-point)
					      (mark-sexp)
					      (buffer-substring (point) (mark))))
					(error func))))))
			;; escape org syntax
			(scimax-org-eldoc-org-escape code)))))
    (org-mode)

    ;; replace `' with links to describe function or variable, unless
    ;; they are in a code block, then leave them alone.
    (goto-char (point-min))
    (while (re-search-forward "`\\([^' ]*\\)'" nil t)
      (let ((result (match-string 1))
	    (bg (match-beginning 1))
	    (end (match-end 1)))
	;; checking for code block changes match data, so
	;; we save it here.
	(unless (save-match-data
		  (eq 'src-block (car (org-element-at-point))))
	  (cond
	   ;; known function
	   ((fboundp (intern result))
	    (cl--set-buffer-substring bg end (format "[[elisp:(describe-function '%s)][%s]]"
						     result result)))
	   ;; known variable
	   ((boundp (intern result))
	    (cl--set-buffer-substring bg end (format "[[elisp:(describe-variable '%s)][%s]]"
						     result result)))
	   ;; unknown quoted thing, just return it back
	   (t
	    result)))))
    ;; finally jump to Requires section
    (org-link-open-from-string "[[*Requires]]")))


(provide 'scimax-org-eldoc)

;;; scimax-org-eldoc.el ends here
