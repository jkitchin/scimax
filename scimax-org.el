;;; scimax-org.el --- org-mode configuration for scimax	-*- lexical-binding:t ; -*-

;;; Commentary:


;;; Code:
(require 'org)
(require 'org-inlinetask)
(require 'org-mouse)
(require 'org-ref)
(require 'org-agenda)
(require 'dash)

;; * Configuration of org-mode
;; Make editing invisible regions smart
(setq org-catch-invisible-edits 'smart)

;; allow lists with letters in them.
(setq org-list-allow-alphabetical t)

;; setup archive location in archive directory in current folder
(setq org-archive-location "archive/%s_archive::")

(defcustom scimax-use-org-bullets nil
  "Whether to use org-bullets-mode or not."
  :group 'scimax)

(when scimax-use-org-bullets
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq org-src-tab-acts-natively t)

;; * Speed commands
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))


(setq org-use-speed-commands t)

(add-to-list 'org-speed-commands-user (cons "P" 'org-set-property))
(add-to-list 'org-speed-commands-user (cons "d" 'org-deadline))

;; Mark a subtree
(add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))

;; Widen
(add-to-list 'org-speed-commands-user (cons "S" 'widen))

;; kill a subtree
(add-to-list 'org-speed-commands-user (cons "k" (lambda ()
						  (org-mark-subtree)
						  (kill-region
						   (region-beginning)
						   (region-end)))))

;; Jump to headline
(add-to-list 'org-speed-commands-user
	     (cons "q" (lambda ()
			 (avy-with avy-goto-line
			   (avy--generic-jump "^\\*+" nil avy-style)))))


(defun org-teleport (&optional arg)
  "Teleport the current heading to after a headline selected with avy.
With a prefix ARG move the headline to before the selected
headline. With a numeric prefix, set the headline level. If ARG
is positive, move after, and if negative, move before."
  (interactive "P")
  ;; Kill current headline
  (org-mark-subtree)
  (kill-region (region-beginning) (region-end))
  ;; Jump to a visible headline
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style))
  (cond
   ;; Move before  and change headline level
   ((and (numberp arg) (> 0 arg))
    (save-excursion
      (yank))
    ;; arg is what we want, second is what we have
    ;; if n is positive, we need to demote (increase level)
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
	       do
	       (if (> 0 n)
		   (org-promote-subtree)
		 (org-demote-subtree)))))
   ;; Move after and change level
   ((and (numberp arg) (< 0 arg))
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))
    ;; n is what we want and second is what we have
    ;; if n is positive, we need to demote
    (let ((n (- (abs arg) (car (org-heading-components)))))
      (cl-loop for i from 1 to (abs n)
	       do
	       (if (> 0 n) (org-promote-subtree)
		 (org-demote-subtree)))))

   ;; move to before selection
   ((equal arg '(4))
    (save-excursion
      (yank)))
   ;; move to after selection
   (t
    (org-mark-subtree)
    (goto-char (region-end))
    (when (eobp) (insert "\n"))
    (save-excursion
      (yank))))
  (outline-hide-leaves))

(add-to-list 'org-speed-commands-user (cons "T" 'org-teleport))

;; * Org-id

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-link-search-must-match-exact-headline 'query-to-create)
(setq org-id-locations-file
      (expand-file-name "user/.org-id-locations" scimax-dir))
(require 'org-id)

;; * Agenda setup
;; record time I finished a task when I change it to DONE
(setq org-log-done 'time)

;; I don't want to see things that are done. turn that off here.
;; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-todo-ignore-scheduled t)
(setq org-agenda-todo-ignore-deadlines t)
(setq org-agenda-todo-ignore-timestamp t)
(setq org-agenda-todo-ignore-with-date t)
(setq org-agenda-start-on-weekday nil) ;; start on current day

(setq org-upcoming-deadline '(:foreground "blue" :weight bold))

;; use timestamps in date-trees. for the journal
(setq org-datetree-add-timestamp 'active)

(add-to-list
 'org-agenda-custom-commands
 '("w" "Weekly Review"
   ( ;; deadlines
    (tags-todo "+DEADLINE<=\"<today>\""
	       ((org-agenda-overriding-header "Late Deadlines")))
    ;; scheduled  past due
    (tags-todo "+SCHEDULED<=\"<today>\""
	       ((org-agenda-overriding-header "Late Scheduled")))

    ;; now the agenda
    (agenda ""
	    ((org-agenda-overriding-header "weekly agenda")
	     (org-agenda-ndays 7)
	     (org-agenda-tags-todo-honor-ignore-options t)
	     (org-agenda-todo-ignore-scheduled nil)
	     (org-agenda-todo-ignore-deadlines nil)
	     (org-deadline-warning-days 0)))
    ;; and last a global todo list
    (todo "TODO"))))


;; * Babel settings
;; enable prompt-free code running
(setq org-confirm-babel-evaluate nil
      org-confirm-elisp-link-function nil
      org-confirm-shell-link-function nil)

;; register languages in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)
   (shell . t)
   (matlab . t)
   (sqlite . t)
   (ruby . t)
   (perl . t)
   (org . t)
   (dot . t)
   (plantuml . t)
   (R . t)
   (fortran . t)
   (C . t)))

;; no extra indentation in the source blocks
(setq org-src-preserve-indentation t)

;; use syntax highlighting in org-file code blocks
(setq org-src-fontify-natively t)

(setq org-babel-default-header-args:python
      '((:results . "output replace")
	(:session . "none")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))

;; ** jupyter ipython blocks

(require 'scimax-org-babel-ipython-upstream)

;; ** jupyter-hy blocks
;; make src blocks open in the right mode
(add-to-list 'org-src-lang-modes '("jupyter-hy" . hy))
(add-to-list 'org-latex-minted-langs '(jupyter-hy  "hylang"))

;; set default headers for convenience
(setq org-babel-default-header-args:jupyter-hy
      '((:results . "output replace")
	(:session . "hy")
	(:kernel . "calysto_hy")
	(:exports . "both")
	(:eval . "never-export")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")))

(defalias 'org-babel-execute:jupyter-hy 'org-babel-execute:ipython)
(defalias 'org-babel-prep-session:jupyter-hy 'org-babel-prep-session:ipython)
(defalias 'org-babel-jupyter-hy-initiate-session 'org-babel-ipython-initiate-session)

;; (when (version< (org-version) "9.2")
;;   (add-to-list 'org-structure-template-alist
;; 	       '("hy" "#+BEGIN_SRC jupyter-hy\n?\n#+END_SRC"
;; 		 "<src lang=\"hy\">\n?\n</src>")))

;; (when (version<= "9.2" (org-version))
;;   (add-to-list 'org-structure-template-alist
;; 	       '("hy" . "src jupyter-hy")))

;; ** Fortran
(defalias 'org-babel-execute:f90 'org-babel-execute:fortran)

;; * Images in org-mode

;; default with images open
(setq org-startup-with-inline-images "inlineimages")

;; default width
(setq org-image-actual-width nil)

;; redisplay figures when you run a block so they are always current.
(add-hook 'org-babel-after-execute-hook
	  'org-display-inline-images)

;; This automatically aligns tables, which is nice if you use code to generate
;; tables.
(defun scimax-align-result-table ()
  "Align tables in the subtree."
  (save-restriction
    (save-excursion
      (unless (org-before-first-heading-p) (org-narrow-to-subtree))
      (org-element-map (org-element-parse-buffer) 'table
	(lambda (tbl)
	  (goto-char (org-element-property :post-affiliated tbl))
	  (org-table-align))))))

(add-hook 'org-babel-after-execute-hook
	  'scimax-align-result-table)




;; * Markup commands for org-mode


(defun org-markup-region-or-point (type beginning-marker end-marker)
  "Apply the markup TYPE with BEGINNING-MARKER and END-MARKER to region, word or point.
This is a generic function used to apply markups. It is mostly
the same for the markups, but there are some special cases for
subscripts and superscripts."
  (cond
   ;; We have an active region we want to apply
   ((region-active-p)
    (let* ((bounds (list (region-beginning) (region-end)))
	   (start (apply 'min bounds))
	   (end (apply 'max bounds))
	   (lines))
      (unless (memq type '(subscript superscript))
	(save-excursion
	  (goto-char start)
	  (unless (looking-at " \\|\\<")
	    (backward-word)
	    (setq start (point)))
	  (goto-char end)
	  (unless (or (looking-at " \\|\\>")
		      (looking-back "\\>" 1))
	    (forward-word)
	    (setq end (point)))))
      (setq lines
	    (s-join "\n" (mapcar
			  (lambda (s)
			    (if (not (string= (s-trim s) ""))
				(concat beginning-marker
					(s-trim s)
					end-marker)
			      s))
			  (split-string
			   (buffer-substring start end) "\n"))))
      (setf (buffer-substring start end) lines)
      (forward-char (length lines))))
   ;; We are on a word with no region selected
   ((thing-at-point 'word)
    (cond
     ;; beginning of a word
     ((looking-back " " 1)
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))
     ;; end of a word
     ((looking-back "\\>" 1)
      (insert (concat beginning-marker end-marker))
      (backward-char (length end-marker)))
     ;; not at start or end so we just sub/sup the character at point
     ((memq type '(subscript superscript))
      (insert beginning-marker)
      (forward-char (- (length beginning-marker) 1))
      (insert end-marker))
     ;; somewhere else in a word and handled sub/sup. mark up the
     ;; whole word.
     (t
      (re-search-backward "\\<")
      (insert beginning-marker)
      (re-search-forward "\\>")
      (insert end-marker))))
   ;; not at a word or region insert markers and put point between
   ;; them.
   (t
    (insert (concat beginning-marker end-marker))
    (backward-char (length end-marker)))))


(defun org-italics-region-or-point ()
  "Italicize the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'italics "/" "/"))


(defun org-bold-region-or-point ()
  "Bold the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'bold "*" "*"))


(defun org-underline-region-or-point ()
  "Underline the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "_" "_"))


(defun org-code-region-or-point ()
  "Mark the region, word or character at point as code.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "~" "~"))


(defun org-verbatim-region-or-point ()
  "Mark the region, word or character at point as verbatim.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'underline "=" "="))


(defun org-strikethrough-region-or-point ()
  "Mark the region, word or character at point as strikethrough.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'strikethrough "+" "+"))


(defun org-subscript-region-or-point ()
  "Mark the region, word or character at point as a subscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'subscript "_{" "}"))


(defun org-superscript-region-or-point ()
  "Mark the region, word or character at point as superscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup."
  (interactive)
  (org-markup-region-or-point 'superscript "^{" "}"))


(defun org-latex-math-region-or-point (&optional arg)
  "Wrap the selected region in latex math markup.
\(\) or $$ (with prefix ARG) or @@latex:@@ with double prefix.
With no region selected, insert those and put point in the middle
to add an equation. Finally, if you are between these markers
then exit them."
  (interactive "P")
  (if (memq 'org-latex-and-related (get-char-property (point) 'face))
      ;; in a fragment, let's get out.
      (goto-char (or (next-single-property-change (point) 'face) (line-end-position)))
    (let ((chars
	   (cond
	    ((null arg)
	     '("\\(" . "\\)"))
	    ((equal arg '(4))
	     '("$" . "$"))
	    ((equal arg '(16))
	     '("@@latex:" . "@@")))))
      (if (region-active-p)
	  ;; wrap region
	  (progn
	    (goto-char (region-end))
	    (insert (cdr chars))
	    (goto-char (region-beginning))
	    (insert (car chars)))
	(cond
	 ((thing-at-point 'word)
	  (save-excursion
	    (end-of-thing 'word)
	    (insert (cdr chars)))
	  (save-excursion
	    (beginning-of-thing 'word)
	    (insert (car chars)))
	  (forward-char (length (car chars))))
	 (t
	  (insert (concat  (car chars) (cdr chars)))
	  (backward-char (length (cdr chars)))))))))


(defun helm-insert-org-entity ()
  "Helm interface to insert an entity from `org-entities'.
F1 inserts utf-8 character
F2 inserts entity code
F3 inserts LaTeX code (does not wrap in math-mode)
F4 inserts HTML code
F5 inserts the entity code."
  (interactive)
  (helm :sources
	(reverse
	 (let ((sources '())
	       toplevel
	       secondlevel)
	   (dolist (element (append
			     '("* User" "** User entities")
			     org-entities-user org-entities))
	     (when (and (stringp element)
			(s-starts-with? "* " element))
	       (setq toplevel element))
	     (when (and (stringp element)
			(s-starts-with? "** " element))
	       (setq secondlevel element)
	       (add-to-list
		'sources
		`((name . ,(concat
			    toplevel
			    (replace-regexp-in-string
			     "\\*\\*" " - " secondlevel)))
		  (candidates . nil)
		  (action . (("insert utf-8 char" . (lambda (x)
						      (mapc (lambda (candidate)
							      (insert (nth 6 candidate)))
							    (helm-marked-candidates))))
			     ("insert org entity" . (lambda (x)
						      (mapc (lambda (candidate)
							      (insert
							       (concat "\\" (car candidate))))
							    (helm-marked-candidates))))
			     ("insert latex" . (lambda (x)
						 (mapc (lambda (candidate)
							 (insert (nth 1 candidate)))
						       (helm-marked-candidates))))
			     ("insert html" . (lambda (x)
						(mapc (lambda (candidate)
							(insert (nth 3 candidate)))
						      (helm-marked-candidates))))
			     ("insert code" . (lambda (x)
						(mapc (lambda (candidate)
							(insert (format "%S" candidate)))
						      (helm-marked-candidates)))))))))
	     (when (and element (listp element))
	       (setf (cdr (assoc 'candidates (car sources)))
		     (append
		      (cdr (assoc 'candidates (car sources)))
		      (list (cons
			     (format "%10s %s" (nth 6 element) element)
			     element))))))
	   sources))))


(defun ivy-insert-org-entity ()
  "Insert an org-entity using ivy."
  (interactive)
  (ivy-read "Entity: " (cl-loop for element in (append org-entities org-entities-user)
				when (not (stringp element))
				collect
				(cons
				 (format "%20s | %20s | %20s | %s"
					 (cl-first element) ;name
					 (cl-second element) ; latex
					 (cl-fourth element) ; html
					 (cl-seventh element)) ;utf-8
				 element))
	    :require-match t
	    :action '(1
		      ("u" (lambda (candidate)
			     (insert (cl-seventh (cdr candidate)))) "utf-8")
		      ("o" (lambda (candidate)
			     (insert "\\" (cl-first (cdr candidate)))) "org-entity")
		      ("l" (lambda (candidate)
			     (insert (cl-second (cdr candidate)))) "latex")
		      ("h" (lambda (candidate)
			     (insert (cl-fourth (cdr candidate)))) "html")
		      ("a" (lambda (candidate)
			     (insert (cl-fifth (cdr candidate)))) "ascii")
		      ("L" (lambda (candidate)
			     (insert (cl-sixth (cdr candidate))) "Latin-1")))))



;; * New org links

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "pydoc"
     :follow (lambda (path)
	       (pydoc path))
     :export (lambda (path desc format)
	       "Generate a url"
	       (let (url)
		 (setq url (cond
			    ((s-starts-with? "scipy" path)
			     (format
			      "https://docs.scipy.org/doc/scipy/reference/generated/%s.html"
			      path))
			    ((s-starts-with? "numpy" path)
			     (format
			      "https://docs.scipy.org/doc/numpy/reference/generated/%s.html"
			      path))
			    (t
			     (format
			      "https://www.google.com/#safe=off&q=%s"
			      path))))


		 (cond
		  ((eq format 'md)
		   (format "[%s](%s)" (or desc path) url))))))
  (org-add-link-type
   "pydoc"
   (lambda (path)
     (pydoc path))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "attachfile"
     :follow (lambda (link-string) (org-open-file link-string))
     :export (lambda (keyword desc format)
	       (cond
		((eq format 'html) (format ""))	; no output for html
		((eq format 'latex)
		 ;; write out the latex command
		 (format "\\attachfile{%s}" keyword)))))

  (org-add-link-type
   "attachfile"
   (lambda (link-string) (org-open-file link-string))
   ;; formatting
   (lambda (keyword desc format)
     (cond
      ((eq format 'html) (format ""))	; no output for html
      ((eq format 'latex)
       ;; write out the latex command
       (format "\\attachfile{%s}" keyword))))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "altmetric"
     :follow (lambda (doi)
	       (browse-url (format  "http://dx.doi.org/%s" doi)))
     :export (lambda (keyword desc format)
	       (cond
		((eq format 'html)
		 (format "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" keyword))
		((eq format 'latex)
		 ""))))

  (org-add-link-type
   "altmetric"
   (lambda (doi)
     (browse-url (format  "http://dx.doi.org/%s" doi)))
   (lambda (keyword desc format)
     (cond
      ((eq format 'html)
       (format "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>
<div data-badge-type='medium-donut' class='altmetric-embed' data-badge-details='right' data-doi='%s'></div>" keyword))
      ((eq format 'latex)
       "")))))


(defun org-man-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    (let* ((page (save-excursion
		   (goto-char (point-min))
		   (re-search-forward " ")
		   (buffer-substring (point-min) (point))))
	   (link (concat "man:" page))
	   (description (format "Manpage for %s" page)))
      (org-store-link-props
       :type "man"
       :link link
       :description description))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "man"
     :follow (lambda (path)
	       (man path))
     :store 'org-man-store-link))


;; * ivy navigation
(defun ivy-org-jump-to-visible-headline ()
  "Jump to visible headline in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump "^\\*+" nil avy-style)))


(defun ivy-jump-to-visible-sentence ()
  "Jump to visible sentence in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy--generic-jump (sentence-end) nil avy-style))
  (forward-sentence))


(defun ivy-org-jump-to-heading ()
  "Jump to heading in the current buffer."
  (interactive)
  (let ((headlines '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      ;; this matches org headings in elisp too.
	      "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"  nil t)
	(cl-pushnew (list
		     (format "%-80s"
			     (match-string 0))
		     (cons 'position (match-beginning 0)))
		    headlines)))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (cdr (assoc 'position candidate)))
			(outline-show-entry)))))


(defun ivy-org-jump-to-agenda-heading ()
  "Jump to a heading in an agenda file."
  (interactive)
  (let ((headlines '()))
    ;; these files should be open already since they are agenda files.
    (loop for file in (org-agenda-files) do
	  (with-current-buffer (find-file-noselect file)
	    (save-excursion
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(cl-pushnew (list
			     (format "%-80s (%s)"
				     (match-string 0)
				     (file-name-nondirectory file))
			     :file file
			     :position (match-beginning 0))
			    headlines)))))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(find-file (plist-get (cdr candidate) :file))
			(goto-char (plist-get (cdr candidate) :position))
			(outline-show-entry)))))


(defun ivy-org-jump-to-heading-in-files (files &optional fontify)
  "Jump to org heading in FILES.
Optional FONTIFY colors the headlines. It might slow things down
a lot with large numbers of org-files or long org-files. This
function does not open the files."
  (let ((headlines '()))
    (loop for file in files do
	  (when (file-exists-p file)
	    (with-temp-buffer
	      (insert-file-contents file)
	      (when fontify
		(org-mode)
		(font-lock-fontify-buffer))
	      (goto-char (point-min))
	      (while (re-search-forward org-heading-regexp nil t)
		(cl-pushnew (list
			     (format "%-80s (%s)"
				     (match-string 0)
				     (file-name-nondirectory file))
			     :file file
			     :position (match-beginning 0))
			    headlines)))))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(find-file (plist-get (cdr candidate) :file))
			(goto-char (plist-get (cdr candidate) :position))
			(outline-show-entry)))))


(defun ivy-org-jump-to-heading-in-directory (&optional recursive)
  "Jump to heading in an org file in the current directory.
Use a prefix arg to make it RECURSIVE.
Use a double prefix to make it recursive and fontified."
  (interactive "P")
  (let ((fontify nil))
    (when (equal recursive '(16))
      (setq fontify t))
    (ivy-org-jump-to-heading-in-files
     (f-entries "."
		(lambda (f)
		  (and
		   (f-ext? f "org")
		   (not (s-contains? "#" f))))
		recursive)
     fontify)))


(defun ivy-org-jump-to-project-headline (&optional fontify)
  "Jump to a headline in an org-file in the current project.
The project is defined by projectile. Use a prefix arg FONTIFY
for colored headlines."
  (interactive "P")
  (ivy-org-jump-to-heading-in-files
   (mapcar
    (lambda (f) (expand-file-name f (projectile-project-root)))
    (-filter (lambda (f)
	       (and
		(f-ext? f "org")
		(not (s-contains? "#" f))))
	     (projectile-current-project-files)))
   fontify))


(defun ivy-org-jump-to-open-headline (&optional fontify)
  "Jump to a headline in an open org-file.
Use a prefix arg FONTIFY for colored headlines."
  (interactive "P")
  (ivy-org-jump-to-heading-in-files
   (mapcar 'buffer-file-name
	   (-filter (lambda (b)
		      (-when-let (f (buffer-file-name b))
			(f-ext? f "org")))
		    (buffer-list)))
   fontify))


(defun ivy-org-jump-to-recent-headline (&optional fontify)
  "Jump to a headline in an org-file in `recentf-list'."
  (interactive)
  (ivy-org-jump-to-heading-in-files
   (-filter (lambda (f) (f-ext? f "org")) recentf-list)
   fontify))


(require 'scimax-org-babel-python)


;; * Enable pdf and eps images in org-mode
;; Suggested on the org-mode maillist by Julian Burgos
(add-to-list 'image-file-name-extensions "pdf")
(add-to-list 'image-file-name-extensions "eps")

(add-to-list 'image-type-file-name-regexps '("\\.eps\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "eps")
(add-to-list 'image-type-file-name-regexps '("\\.pdf\\'" . imagemagick))
(add-to-list 'image-file-name-extensions "pdf")

(setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))

;; * A better return

(defun scimax/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return-indent))

     ;; Open links like usual, unless point is at the end of a line.
     ;; and if at beginning of line, just press enter.
     ((or (and (eq 'link (car (org-element-context))) (not (eolp)))
	  (bolp))
      (org-return))

     ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
     ;; Johansson!
     ((org-inlinetask-in-task-p)
      (org-return))

     ;; checkboxes - add new or delete empty
     ((org-at-item-checkbox-p)
      (cond
       ;; at the end of a line.
       ((and (eolp)
	     (not (eq 'item (car (org-element-context)))))
	(org-insert-todo-heading nil))
       ;; no content, delete
       ((and (eolp) (eq 'item (car (org-element-context))))
	(setf (buffer-substring (line-beginning-position) (point)) ""))
       ((eq 'paragraph (car (org-element-context)))
	(goto-char (org-element-property :end (org-element-context)))
	(org-insert-todo-heading nil))
       (t
	(org-return))))

     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((org-in-item-p)
      (cond
       ;; empty definition list
       ((and (looking-at " ::")
	     (looking-back "- " 3))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; empty item
       ((and (looking-at "$")
	     (looking-back "- " 3))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; numbered list
       ((and (looking-at "$")
	     (looking-back "[0-9]+. " (line-beginning-position)))
	(beginning-of-line)
	(delete-region (line-beginning-position) (line-end-position)))
       ;; insert new item
       (t
	(end-of-line)
	(org-insert-item))))

     ;; org-heading
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
	  (progn
	    ;; Go to end of subtree suggested by Pablo GG on Disqus post.
	    (org-end-of-subtree)
	    (org-insert-heading-respect-content)
	    (outline-show-entry))
	;; The heading was empty, so we delete it
	(beginning-of-line)
	(setf (buffer-substring
	       (line-beginning-position) (line-end-position)) "")))

     ;; tables
     ((org-at-table-p)
      (if (-any?
	   (lambda (x) (not (string= "" x)))
	   (nth
	    (- (org-table-current-dline) 1)
	    (remove 'hline (org-table-to-lisp))))
	  (org-return)
	;; empty row
	(beginning-of-line)
	(setf (buffer-substring
	       (line-beginning-position) (line-end-position)) "")
	(org-return)))

     ;; fall-through case
     (t
      (org-return)))))


(defcustom scimax-return-dwim t
  "When t redefine the Ret behavior to add items, headings and table rows."
  :group 'scimax)


(when scimax-return-dwim
  (define-key org-mode-map (kbd "RET")
    'scimax/org-return))

;;* org-numbered headings
(defun scimax-overlay-numbered-headings ()
  "Put numbered overlays on the headings."
  (interactive)
  (loop for (p lv) in (let ((counters (copy-list '(0 0 0 0 0 0 0 0 0 0)))
			    (current-level 1)
			    last-level)
			(mapcar (lambda (x)
				  (list (car x)
					;; trim trailing zeros
					(let ((v (nth 1 x)))
					  (while (= 0 (car (last v)))
					    (setq v (butlast v)))
					  v)))
				(org-map-entries
				 (lambda ()
				   (let* ((hl (org-element-context))
					  (level (org-element-property :level hl)))
				     (setq last-level current-level
					   current-level level)
				     (cond
				      ;; no level change or increase, increment level counter
				      ((or (= last-level current-level)
					   (> current-level last-level))
				       (incf (nth current-level counters)))

				      ;; decrease in level
				      (t
				       (loop for i from (+ 1 current-level) below (length counters)
					     do
					     (setf (nth i counters) 0))
				       (incf (nth current-level counters))))

				     (list (point) (-slice counters 1)))))))
	do
	(let ((ov (make-overlay p p)))
	  (overlay-put ov 'before-string (concat (mapconcat 'number-to-string lv ".") ". "))
	  (overlay-put ov 'numbered-heading t))))


(define-minor-mode scimax-numbered-org-mode
  "Minor mode to number org headings."
  :init-value nil
  (cl-labels ((fl-noh (limit) (save-restriction
				(widen)
				(ov-clear 'numbered-heading)
				(scimax-overlay-numbered-headings))))

    (if scimax-numbered-org-mode
	(progn
	  (font-lock-add-keywords
	   nil
	   `((fl-noh 0 nil)))
	  (font-lock-fontify-buffer))
      (ov-clear 'numbered-heading)
      (font-lock-remove-keywords
       nil
       `((fl-noh 0 nil))))))


(use-package scimax-org-radio-checkbox
  :ensure nil
  :load-path scimax-dir)


(use-package scimax-org-latex
  :load-path scimax-dir
  :ensure nil
  :config
  (scimax-toggle-org-latex-fragment-tooltip)
  (scimax-toggle-latex-fragment-justification)
  (scimax-toggle-latex-equation-numbering)
  (scimax-toggle-inject-latex))


(use-package scimax-org-images
  :ensure nil
  :load-path scimax-dir)

(use-package scimax-org-src-blocks
  :ensure nil
  :load-path scimax-dir
  :config (scimax-org-toggle-colored-src-blocks))


;; * The end
(provide 'scimax-org)

;;; scimax-org.el ends here
