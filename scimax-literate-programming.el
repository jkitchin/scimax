;;; scimax-literate-programming.el --- Literate programming tools for org-mode

;;; Commentary:
;; This library adds code navigation via M-. and M-, in literate program
;; org-files. It works by creating and visiting a TAGS file for the src-blocks
;; in the file.
;;
;; This works by creating a temporary version of the org-file with everything
;; but the relevant code stripped out, and then running etags on this temporary
;; version of the file. Only blocks that should be tangled are included in this
;; file.
;;
;; You have to generate a TAGS file first with `scimax-lp-generate-tags' which will
;; work on all the org-files in the current directory, including recursive
;; descent into all subdirectories. The languages that are supported are defined
;; in `scimax-lp-etags-language-map'. The TAGS are not automatically updated, if you
;; add new definitions, you should rerun `scimax-lp-generate-tags'. After this, when
;; you are on a symbol defined in the current org-file M-. should jump to the
;; definition, and M-, should jump back.
;;
;; `scimax-lp-signature-doc' can be used to try getting a signature/docstring for the
;; symbol at point. In emacs-lisp blocks this works on functions and variables.
;; In other languages it will just return the line where the symbol is defined.
;;
;;; Code:

(require 'f)

;; languages supported in etags
;; ada        .ads .adb .ada
;; asm        .a .asm .def .inc .ins .s .sa .S .src
;; c          .c .h
;; c++        .C .c++ .cc .cpp .cxx .H .h++ .hh .hpp .hxx .M .pdb
;; c*         .cs .hs
;; cobol      .COB .cob
;; erlang     .erl .hrl
;; forth      .fth .tok
;; fortran    .F .f .f90 .for
;; go         .go
;; html       .htm .html .shtml
;; java       .java
;; lisp       .cl .clisp .el .l .lisp .LSP .lsp .ml
;; lua        .lua .LUA
;; makefile   Makefile makefile GNUMakefile Makefile.in Makefile.am
;; objc       .lm .m
;; pascal     .p .pas
;; perl       .pl .pm
;; php        .php .php3 .php4
;; postscript .ps .psw
;; proc       .pc
;; prolog     .prolog
;; python     .py
;; ruby       Rakefile Thorfile .rb .ru .rbw
;; scheme     .oak .sch .scheme .SCM .scm .SM .sm .ss .t
;; tex        .bib .clo .cls .ltx .sty .TeX .tex
;; texinfo    .texi .texinfo .txi
;; yacc       .y .y++ .ym .yxx .yy
;; auto
;; none



(defvar scimax-lp-etags-language-map
  '(("emacs-lisp"  . "lisp")
    ("fortran" . "fortran")
    ("python" . "python")
    ("ipython" . "python"))
  "An a-list of babel languages to language for etags.
Each cons cell is (src-block lang . etags language)")


(defun scimax-lp-tangle-p ()
  "Return t if the block should be tangled.
That means :tangle is not no."
  (and (org-in-src-block-p)
       (not (string= "no"
		     (cdr (assq :tangle
				(nth 2 (org-babel-get-src-block-info 'light))))))))


(defun scimax-lp-update-lang-tags (org-file lang)
  "Run etags on a stripped version of the ORG-FILE in LANG mode.
This should run etags on a version of the org-file where all
content that is not a src-block in LANG that is supposed to be
tangled has been stripped out. This is done dangerously; the
current buffer is erased and replaced with the stripped content
so that etags believes it is the right file, then the content is
replaced back. This is done inside an `atomic-change-group' which
should make this a safe operation."
  (interactive "f\nsLang:")
  (message "Updating %s tags in %s" lang org-file)
  (when (cdr (assoc lang scimax-lp-etags-language-map))
    (let ((open (find-buffer-visiting org-file)))
      (with-current-buffer (find-file-noselect org-file)
	(save-buffer)
	(let* ((content (buffer-string)))
	  ;; This has potential for disaster since it deletes the buffer! I think
	  ;; this is pretty safe, but you should be prepared for disaster. If
	  ;; there is any error in this, I think it undoes the buffer damage.
	  (atomic-change-group
	    (goto-char (point-min))
	    (while (and (not (eobp)))
	      (if (and (org-in-src-block-p)
		       (string= lang (car (org-babel-get-src-block-info 'light)))
		       (scimax-lp-tangle-p))
		  (let* ((src (org-element-context))
			 (end (org-element-property :end src))
			 (len (length (buffer-substring
				       (line-beginning-position)
				       (line-end-position))))
			 newend)
		    (setf (buffer-substring
			   (line-beginning-position)
			   (line-end-position))
			  "")
		    ;; Now skip to end, and go back to then src delimiter and
		    ;; eliminate that line.
		    (goto-char (- end len))
		    (forward-line (- (* -1 (org-element-property :post-blank src)) 1))
		    (setf (buffer-substring
			   (line-beginning-position)
			   (line-end-position)) ""))
		(setf (buffer-substring
		       (line-beginning-position)
		       (line-end-position)) ""))
	      (forward-line 1))
	    (save-buffer)
	    (shell-command
	     (format "etags %s --declarations --language=%s %s"
		     (if (file-exists-p "TAGS") "-a" "")
		     (cdr (assoc lang scimax-lp-etags-language-map))
		     org-file))
	    ;; now replace the content back
	    (erase-buffer)
	    (insert content)
	    (save-buffer))))
      ;; close buffer if it wasn't already open.
      (unless open (kill-buffer (find-buffer-visiting org-file))))))


(defun scimax-lp-generate-tags ()
  "Generate a list of tags from org-files and visit the tag-file.
This will attempt to get tags for every language defined in
`scimax-lp-etags-language-map'."
  (interactive)
  (save-buffer)
  (when (file-exists-p "TAGS") (delete-file "TAGS"))
  (let* ((current-point (point))
	 (org-files (f-entries
		     "."
		     (lambda (f) (f-ext? f "org")) t))
	 langs)
    (loop for org-file in org-files do
	  (setq langs '())
	  (org-babel-map-src-blocks org-file
	    (pushnew lang langs :test 'string=))
	  (loop for lang in langs do
		(scimax-lp-update-lang-tags org-file lang)))
    (goto-char current-point)
    (let ((tag-buffer (or (find-buffer-visiting "TAGS")
			  (find-file-noselect "TAGS"))))
      (with-current-buffer tag-buffer
	(revert-buffer :ignore-auto :noconfirm)
	(visit-tags-table "TAGS")))))


(defun scimax-lp-signature-doc ()
  "Get signature and docstring for thing at point.
For emacs-lisp this should work for defun and defvar. For other
languages you will get see the definition line."
  (interactive)
  (when (org-in-src-block-p)
    ;; This is a weird issue. It seems like read moves the point inside the
    ;; save-window-excursion, so I save the point here to move back later.
    (let ((current-point (point)))
      (save-window-excursion
	(let* ((fname (symbol-name (symbol-at-point)))
	       (p (xref-find-definitions fname)))
	  (cond
	   ((string= (get-char-property (point) 'lang) 'emacs-lisp)
	    (cond
	     ((looking-at "(defun")
	      (let* ((def (read (current-buffer)))
		     (args (nth 2 def))
		     (n3 (nth 3 def))
		     (docstring (if (stringp n3) n3 "")))
		(message "%s: (%s) \"%s\"" fname args docstring)))
	     ((looking-at "(defvar")
	      (let* ((def (read (current-buffer)))
		     (var (nth 1 def))
		     (val (nth 2 def))
		     (ds (nth 3 def)))
		(message "%s=%s \"%s\"" var val ds)))))
	   (t
	    ;; This assumes that xref-find-definitions moved the point. I don't
	    ;; know any better way to get reasonable information about the
	    ;; definition. You need parsing to get it in general.
	    (message (buffer-substring
		      (line-beginning-position)
		      (line-end-position)))))))
      (goto-char current-point))))


(provide 'scimax-literate-programming)

;;; scimax-literate-programming.el ends here
