;;; scimax-literate-programming.el --- Literate programming tools for org-mode

;;; Commentary:
;; This library adds code navigation via M-. and M-, in literate program
;; org-files. This assumes you are using an org-file to create a program file
;; that will be tangled to a source file. It works by creating and visiting a
;; TAGS file for the tangled src-blocks in the file.
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
;; This is largely a proof of concept. It works, but it turns out I don't use
;; literate programming as much as I thought I would. This library indeed makes
;; it nicer, but still many debugging tools don't interface will with the
;; org-file. It is a little annoying you have to run `scimax-lp-generate-tags'
;; to regenerate tags. This might be an expensive operation though, and although
;; you could run it in a save-buffer hook, that is not done by default here.
;;
;; Probably I would consider rewriting this code to use something like lsp or
;; tree-sitter when that is a feature in Emacs 29. I wrote this code several
;; years ago when that was not obvious.
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

(defvar scimax-lp-consider-all nil
  "If non-nil consider all src blocks when making tags.")


(defvar scimax-lp-etags-language-map
  '(("emacs-lisp"  . "lisp")
    ("fortran" . "fortran")
    ("python" . "python")
    ("jupyter-python" . "python")
    ("ipython" . "python"))
  "An a-list of babel languages to language for etags.
Each cons cell is (src-block lang . etags language)")


(defun scimax-lp-tangle-p ()
  "Return absolute tangle filename if the block should be tangled.
That means :tangle is not no."
  (when (org-in-src-block-p)
    (let ((tangle (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light))))))
      ;; Note this might be a "yes"
      (when (not (string= "no" tangle))
	(expand-file-name tangle)))))


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

	(let* ((content (buffer-string))
	       (inhibit-read-only t))
	  ;; This has potential for disaster since it deletes the buffer! I think
	  ;; this is pretty safe, but you should be prepared for disaster. If
	  ;; there is any error in this, I think it undoes the buffer damage.
	  (atomic-change-group
	    (goto-char (point-min))
	    (while (and (not (eobp)))
	      (if (and (org-in-src-block-p)
		       (string= lang (car (org-babel-get-src-block-info 'light)))
		       (or (scimax-lp-tangle-p) scimax-lp-consider-all))
		  (let* ((src (org-element-context))
			 (end (org-element-property :end src))
			 (len (length (buffer-substring
				       (line-beginning-position)
				       (line-end-position))))
			 newend)
		    (cl--set-buffer-substring (line-beginning-position)
					      (line-end-position)
					      "")
		    
		    ;; Now skip to end, and go back to then src delimiter and
		    ;; eliminate that line.
		    (goto-char (- end len))
		    (forward-line (- (* -1 (org-element-property :post-blank src)) 1))
		    (cl--set-buffer-substring (line-beginning-position)
					      (line-end-position)
					      ""))
		(cl--set-buffer-substring (line-beginning-position)
					  (line-end-position)
					  ""))
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


(defvar scimax-lp-update-tags-always t
  "If non-nil, update TAGS file whenever the org file is newer.")


(defun scimax-lp-generate-tags (&optional refresh)
  "Generate a list of tags from org-files and visit the tag-file.
This will attempt to get tags for every language defined in
`scimax-lp-etags-language-map'."
  (interactive "P")
  (when (and (eq major-mode 'org-mode)
	     (or scimax-lp-update-tags-always refresh))
    (save-buffer)
    ;; (when (file-exists-p "TAGS") (delete-file "TAGS"))
    (let* ((current-point (point))
	   (org-files (f-entries
		       "."
		       (lambda (f) (f-ext? f "org")) t))
	   langs)
      (cl-loop for org-file in org-files do
	       (setq langs '())
	       (org-babel-map-src-blocks org-file
		 (pushnew lang langs :test 'string=))
	       (clloop for lang in langs do
		       (scimax-lp-update-lang-tags org-file lang)))
      (goto-char current-point)))
  (let ((tag-buffer (or (find-buffer-visiting "TAGS")
			(find-file-noselect "TAGS"))))
    (when tag-buffer
      (with-current-buffer tag-buffer
	(revert-buffer :ignore-auto :noconfirm)
	(visit-tags-table "TAGS")))))


;; I had to make this small function for a reason I don't understand. I could
;; not use the `scimax-lp-generate-tags' function directly without an error
;; related to number of arguments.
(defun scimax-lp-xref-advice (arg)
  ":before advice for xref-find-definitions to automatically make tags."
  (scimax-lp-generate-tags))

(advice-add 'xref-find-definitions :before #'scimax-lp-xref-advice)

(defun scimax-lp-signature-doc ()
  "Get signature and docstring for thing at point.
For emacs-lisp this should work for defun and defvar. For other
languages you will get see the definition line."
  (interactive)
  (when (org-in-src-block-p)
    ;; This is a weird issue. It seems like read moves the point inside the
    ;; save-window-excursion and doesn't restore it, so I save the point here to
    ;; move back later.
    (let ((current-point (point)))
      (save-window-excursion
	(let* ((sname (symbol-name (symbol-at-point)))
	       (p (condition-case nil (xref-find-definitions sname)
		    (error nil))))
	  (cond
	   ((string= (get-char-property (point) 'lang) 'emacs-lisp)
	    (cond
	     ((looking-at "(defun")
	      (let* ((def (read (current-buffer)))
		     (args (nth 2 def))
		     (n3 (nth 3 def))
		     (docstring (if (stringp n3) n3 "")))
		(message "%s: (%s) \"%s\"" sname args docstring)))
	     ((looking-at "(defvar")
	      (let* ((def (read (current-buffer)))
		     (var (nth 1 def))
		     (val (nth 2 def))
		     (ds (nth 3 def)))
		(message "%s=%s \"%s\"" var val ds)))))
	   (t
	    ;; if p is nil it means nothing was found, so we try searching
	    ;; instead. this is not a very sophisticated search yet, we should
	    ;; search until we are in the right kind of code block. This will
	    ;; fail on things not defined in the current file, e.g. variable
	    ;; names that are imported.
	    (unless p
	      (goto-char (point-min))
	      (re-search-forward sname))
	    ;; Then, we show the context
	    (message (buffer-substring
		      (line-beginning-position)
		      (line-end-position)))))))
      (goto-char current-point))))

;; * Advice on org-babel-load-file

;; The idea here is to replace all definitions of the tangled files with the
;; org-file in `load-history' so that describe-function/variable points to them
;; instead.

(defun scimax-lp-modify-load-history (&rest args)
  "Modify the load-history to point all tangled files to compile."
  (interactive)
  (let* ((file (nth 0 args))
	 (compile (nth 1 args))
	 (open (find-buffer-visiting file))
	 tf
	 (tangle-files '()))
    (org-babel-map-src-blocks file
      ;; I am not sure if it matters if the
      (setq tf (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
      (cond
       ((string= "yes" tf)
	(setq tf (concat (file-name-sans-extension (buffer-file-name)) ".el")))
       ((not (string= "no" tf))
	(setq tf (expand-file-name tf)))
       (t
	(setq tf nil)))
      (when tf
	(setq tf (concat tf (if compile "c" "")))
	(pushnew (expand-file-name tf) tangle-files :test #'string=)))
    ;; now modify the load-history
    (mapc (lambda (tf)
	    (when (car (assoc tf load-history))
	      (setf (car (assoc tf load-history)) (expand-file-name file))))
	  tangle-files)
    (unless open (kill-buffer open))))


(defun scimax-lp-toggle-modify-load-history ()
  "Toggle `scimax-lp-modify-load-history' advice."
  (interactive)
  (if (not (get 'scimax-lp-toggle-modify-load-history 'enabled))
      (progn
	(advice-add 'org-babel-load-file :after #'scimax-lp-modify-load-history)
	(put 'scimax-lp-toggle-modify-load-history 'enabled t)
	(message "scimax-lp-toggle-modify-load-history advice enabled."))
    (advice-remove 'org-babel-load-file #'scimax-lp-modify-load-history)
    (put 'scimax-lp-toggle-modify-load-history 'enabled nil)
    (message "scimax-lp-toggle-modify-load-history advice disabled.")))

(scimax-lp-toggle-modify-load-history)

(provide 'scimax-literate-programming)

;;; scimax-literate-programming.el ends here
