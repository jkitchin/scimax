;;; scimax-org.el --- org-mode configuration for scimax	-*- lexical-binding:t ; -*-

;;; Commentary:


;;; Code:
(require 'org)
(require 'org-inlinetask)
(require 'org-mouse)
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
  :group 'scimax
  :type 'boolean)

(when scimax-use-org-bullets
  (use-package org-bullets)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")))


;; * heading manipulation

(defun scimax-org-headline-to-inlinetask ()
  "Convert the heading at point to an inlinetask."
  (interactive)
  (let* ((hl (org-element-context))
	 (body (buffer-substring-no-properties (org-element-property :contents-begin hl)
					       (org-element-property :contents-end hl)))
	 (title (org-element-property :title hl))
	 (tags (nth 5 (org-heading-components)))
	 (beg (org-element-property :begin hl))
	 (end (org-element-property :end hl))
	 (inlinetask (with-temp-buffer
		       (org-mode)
		       (org-inlinetask-insert-task) 
		       (insert title "  " tags "\n" body)
		       (concat (buffer-string) "\n\n"))))
    (cl--set-buffer-substring beg end inlinetask)))

;; * Speed commands
;; These are single letter commands at headings
(setq org-use-speed-commands t)


;; I use these for convenience
;; kill a subtree
(defun scimax-org-kill-subtree ()
  "Kill current subtree."
  (interactive)
  (org-mark-subtree)
  (kill-region
   (region-beginning)
   (region-end)))


;; Jump to headline
(defun scimax-avy-org-headline ()
  "Jump to an org headline with avy."
  (interactive)
  (avy-with avy-goto-line
    (avy-jump org-heading-regexp)))


(defun org-teleport (&optional arg)
  "Teleport the current heading to a visible headline selected with avy.
With a prefix ARG move the headline to before the selected
headline. With a numeric prefix, set the headline level. If ARG
is positive, move after, and if negative, move before."
  (interactive "P")

  ;; if your heading is at the last line, we have to add a \n so that when we
  ;; move it, there is a next line to separate it.
  (save-excursion
    (goto-char (line-end-position))
    (when (eobp)
      (insert "\n")))
  
  (org-mark-subtree) 
  (kill-region (region-beginning) (region-end))
  ;; Jump to a visible headline
  (avy-with avy-goto-line (avy-jump org-heading-regexp))
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

;; [2021-05-24 Mon] See https://github.com/jkitchin/scimax/issues/416#issuecomment-836802234
;; [2024-02-05 Mon] This may not work for org-versions less than 9.5. I think that is ok.
(setq org-speed-commands
      (append org-speed-commands
	      '(("P" . org-set-property)
		("d" . org-deadline)
		("m"  . org-mark-subtree)
		("S" . widen)
		("k" . scimax-org-kill-subtree)
		("K" . scimax-org-headline-to-inlinetask)
		("q" . scimax-avy-org-headline)
		("T" . org-teleport))))


;; * Org-id

(setq org-id-link-to-org-use-id 'create-if-interactive)
(setq org-link-search-must-match-exact-headline 'query-to-create)
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
      ;; set some bounds here, unless it is a subscript/superscript
      ;; Those start at point or region
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
      (cl--set-buffer-substring start end lines) 
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

     ;; looking back at closing char
     ((and (memq type '(subscript superscript))
	   (looking-back end-marker 1))
      (delete-char -1)
      (forward-char)
      (insert end-marker))

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
   ;; looking back at end marker, slurp next word in
   ((looking-back end-marker (length end-marker))
    (delete-char (* -1 (length end-marker)))
    (forward-word)
    (insert end-marker))
   ;; not at a word or region insert markers and put point between
   ;; them.
   (t
    (insert (concat beginning-marker end-marker))
    (backward-char (length end-marker)))))


(defun org-double-quote-region-or-point ()
  "Double quote the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "\"" "\""))


(defun org-single-quote-region-or-point ()
  "Single quote the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "'" "'"))


(defun org-italics-region-or-point ()
  "Italicize the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'italics "/" "/"))


(defun org-bold-region-or-point ()
  "Bold the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'bold "*" "*"))


(defun org-underline-region-or-point ()
  "Underline the region, word or character at point.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "_" "_"))


(defun org-code-region-or-point ()
  "Mark the region, word or character at point as code.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "~" "~"))


(defun org-verbatim-region-or-point ()
  "Mark the region, word or character at point as verbatim.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'underline "=" "="))


(defun org-strikethrough-region-or-point ()
  "Mark the region, word or character at point as strikethrough.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'strikethrough "+" "+"))


(defun org-subscript-region-or-point ()
  "Mark the region, word or character at point as a subscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
  (interactive)
  (org-markup-region-or-point 'subscript "_{" "}"))


(defun org-superscript-region-or-point ()
  "Mark the region, word or character at point as superscript.
This function tries to do what you mean:
1. If you select a region, markup the region.
2. If in a word, markup the word.
3. Otherwise wrap the character at point in the markup.
Repeated use of the function slurps the next word into the markup."
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
	 ;; slurp next word if you call it again
	 ((and (not (equal arg '(16))) (looking-back (regexp-quote (cdr chars)) (length (cdr chars))))
	  (delete-char (* -1 (length (cdr chars))))
	  (forward-word)
	  (insert (cdr chars)))
	 (t
	  (insert (concat  (car chars) (cdr chars)))
	  (backward-char (length (cdr chars)))))))))


(defun ivy-insert-org-entity ()
  "Insert an org-entity using ivy."
  (interactive)
  (ivy-read "Entity: " (cl-loop for element in (append org-entities org-entities-user)
				when (not (stringp element))
				collect
				(cons
				 (format "%20s | %20s | %20s | %s"
					 (cl-first element)    ;name
					 (cl-second element)   ; latex
					 (cl-fourth element)   ; html
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

(org-link-set-parameters
 "attachfile"
 :follow (lambda (link-string) (org-open-file link-string))
 :export (lambda (keyword desc format)
	   (cond
	    ((eq format 'html) (format ""))	; no output for html
	    ((eq format 'latex)
	     ;; write out the latex command
	     (format "\\attachfile{%s}" keyword)))))

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


(defun org-man-store-link ()
  "Store a link to a man page."
  (when (memq major-mode '(Man-mode woman-mode))
    (let* ((page (save-excursion
		   (goto-char (point-min))
		   (re-search-forward " ")
		   (buffer-substring (point-min) (point))))
	   (link (concat "man:" page))
	   (description (format "Manpage for %s" page)))
      (org-link-store-props
       :type "man"
       :link link
       :description description))))

(org-link-set-parameters
 "man"
 :follow (lambda (path)
	   (man path))
 :store 'org-man-store-link)


;; * ivy navigation
(defun avy-org-jump-to-visible-headline ()
  "Jump to visible headline in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy-jump org-heading-regexp nil)))


(defun avy-jump-to-visible-sentence ()
  "Jump to visible sentence in the buffer."
  (interactive)
  (org-mark-ring-push)
  (avy-with avy-goto-line (avy-jump (sentence-end) nil))
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
    (cl-loop for file in (org-agenda-files) do
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
    (cl-loop for file in files do
	  (when (file-exists-p file)
	    (with-temp-buffer
	      (insert-file-contents file)
	      (when fontify
		(org-mode)
		(font-lock-ensure))
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


(defcustom scimax-ivy-jump-functions
  '((heading . ivy-org-jump-to-heading)
    (visible . avy-org-jump-to-visible-headline)
    (sentence . avy-jump-to-visible-sentence)
    (recent-org ivy-org-jump-to-recent-headline)
    (directory . ivy-org-jump-to-heading-in-directory)
    (project . ivy-org-jump-to-project-headline )
    (agenda ivy-org-jump-to-agenda-heading))
  "alist of jump functions. The first one is the default."
  :group 'scimax
  :type '(alist :key-type symbol ::value-type function))


(defun ivy-org-jump (&optional arg)
  "Jump to a location in org file. The default is the first entry
in `scimax-ivy-jump-functions'. With a prefix arg, you can choose
the scope."
  (interactive "P")
  (let ((jumpfn (if arg (cdr (assoc (intern-soft
				     (ivy-read "Scope: " scimax-ivy-jump-functions))
				    scimax-ivy-jump-functions))
		  ;; the default choice.
		  (cdr (car scimax-ivy-jump-functions)))))
    (funcall jumpfn)))


;; * A better return

(defun scimax/org-return (&optional arg)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET.
A prefix arg of 4 opens link in new window.
A prefix arg of 5 opens link in new frame."
  (interactive "P")
  (cond
   ;; single prefix arg, no fancy stuff, just org-return
   ((and arg (listp arg) (equal arg '(4))) 
    (org-return))

   ((null arg)
    (cond

     ((eq 'line-break (car (org-element-context)))
      (org-return t))

     ;; Open links like usual, unless point is at the end of a line.
     ((and (eq 'link (car (org-element-context))) (not (eolp)))
      (org-return))

     ((looking-at org-heading-regexp)
      (org-return))
     

     ;; when you are here
     ;; * headline...
     ;;              ^
     ;; this rule is activated
     ((and (bolp)
	   ;; This avoids the case where you are at the beginning of a line that is not folded
	   (save-excursion
	     (let ((p (point))) 
	       (org-beginning-of-line) 
	       (not (= p (point)))))
	   ;; This is a heuristic device where I found C-a C-e does not return
	   ;; to the same place. I feel like this is new behavior since org
	   ;; 9.5ish, but am not sure
	   (save-excursion
	     (let ((p (point))) 
	       (org-beginning-of-line)
	       (org-end-of-line)
	       (not (= p (point))))))
      (org-show-entry)
      (org-insert-heading))

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
	(delete-region (line-beginning-position) (point)))
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
       ((and (looking-at "$")
	     (looking-at "^"))
	(org-return))
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
	(delete-region (line-beginning-position) (line-end-position))))

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
	(delete-region (line-beginning-position) (line-end-position)) 
	(org-return)))
     ;; fall-through
     (t
      (org-return))))
   
   ;; other window, 
   ((= arg 4)
    (clone-indirect-buffer-other-window (buffer-name) t)
    (org-return))
   
   ;; other frame
   ((= arg 5)
    (clone-frame)
    (org-return))

   ;; fall-through case
   (t
    (org-return))))


(defcustom scimax-return-dwim t
  "When t redefine the Ret behavior to add items, headings and table rows."
  :group 'scimax
  :type 'boolean)


(when scimax-return-dwim
  (define-key org-mode-map (kbd "RET")
	      'scimax/org-return))


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
  :load-path scimax-dir)


(defun scimax-get-file-keyword (KEYWORD)
  "Get the value for the file KEYWORD from a line like this
#+KEYWORD: value
in a file. The search for KEYWORD is not case-sensitive."
  (interactive)
  (let ((case-fold-search t)
        (re (format "^#\\+%s:[ \t]+\\([^\t\n]+\\)" KEYWORD)))
    (if (not (save-excursion
               (or (re-search-forward re nil t)
                   (re-search-backward re nil t))))
        (error (format "No line containing #+%s: value found" KEYWORD)))
    (match-string-no-properties 1)))


;; * tooltips on footnotes
;; inspired by Juan Manual Macías on the org-mode mailing list
(defun scimax-footnote-reference-tooltip (_win _obj position)
  "Get footnote contents"
  (save-excursion
    (goto-char position)
    (or
     (nth 3 (org-footnote-get-definition
	     (org-element-property :label (org-element-context))))
     "No footnote content found.")))


(defun scimax-footnote-tooltip (limit)
  "Add text properties for footnotes.
This is used as :override advice on `org-activate-footnote-links'."
  (let ((fn (org-footnote-next-reference-or-definition limit)))
    (when fn
      (let* ((beg (nth 1 fn))
	     (end (nth 2 fn))
	     (label (car fn))
	     (referencep (/= (line-beginning-position) beg)))
	(when (and referencep (nth 3 fn))
	  (save-excursion
	    (goto-char beg)
	    (search-forward (or label "fn:"))
	    (org-remove-flyspell-overlays-in beg (match-end 0))))
	(add-text-properties beg end
			     (list 'mouse-face 'highlight
				   'keymap org-mouse-map
				   'help-echo
				   ;; this is the modification to get the tooltips to show
				   (if referencep #'scimax-footnote-reference-tooltip
				     ;; I don't know what would make sense here,
				     ;; so we leave a string
				     "Footnote definition")
				   'font-lock-fontified t
				   'font-lock-multiline t
				   'face 'org-footnote))))))

(advice-add 'org-activate-footnote-links :override 'scimax-footnote-tooltip)

;; * The end
(provide 'scimax-org)

;;; scimax-org.el ends here
