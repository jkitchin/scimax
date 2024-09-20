;;; ore.el --- Org-mode explorer

;;; Commentary:
;; This module provides one command `ore' that provides information about the
;; org element at point.

;;; Code:

(require 'emacs-keybinding-command-tooltip-mode)

(defvar ore-user-directory "~/.emacs.d/ore/"
  "Directory to store user additions to the notes.")


(defun ore-user-documentation (type)
  "Return user documentation for org element TYPE if it exists.
Notes are returned as plain text, and will be rendered in `help-mode'."
  (let ((fname (expand-file-name (format "%s.org" type) ore-user-directory)))
    (concat
     "User documentation:\n"
     (if (file-exists-p fname)
	 (with-temp-buffer
           (insert "\n")
	   (insert-file-contents fname)
	   (indent-rigidly (point-min) (point-max) 5)
	   (buffer-string))
       "None defined.")
     (format  "\n\nEdit [[file:%s]]" fname))))


(defun ore-latex (element)
  "`ore' documentation for latex fragment."
  (concat
   (substitute-command-keys "You are on a LaTeX fragment or environment.

\\[org-toggle-latex-overlays] or `org-toggle-latex-overlays' to
toggle LaTeX images on it.

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'latex)))


(defun ore-link (element)
  "`ore' documentation for org links."
  (let* ((link (org-element-context))
	 (type (org-element-property :type link)) 
	 (follow-func (org-link-get-parameter type :follow))
	 (export-func (org-link-get-parameter type :export)))
    (concat
     (format
      (substitute-command-keys "You are on a %s link.

Link path: %s
%s

Clicking on the link will run `%s'.

This link uses this function for export: `%s'

If you are on an image link, you can toggle it with
\\[org-toggle-inline-images] or `org-toggle-inline-images'.

You can toggle the link display with `org-toggle-link-display'.

See Info node `(org) Hyperlinks'.

%s

%s\n\n")
      type
      (org-element-property :path link)
      (format "Whole link: %s" (buffer-substring
				(org-element-property :begin link)
				(org-element-property :end link)))
      (pp-to-string follow-func)
      (pp-to-string export-func)
      (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
      (ore-user-documentation 'link)))))


(defun ore-src-block-header-p (element)
  "Return whether point is in a src-block header."
  (and (eq 'src-block (car element))
       (save-excursion
	 (let ((cp (point))
	       (lp (line-number-at-pos)))
	   (goto-char (org-element-property :begin element))
	   (= lp (line-number-at-pos))))))


(defun ore-src-block-header (element)
  "`ore' documentation for src-block header."
  (concat
   "You are in a src-block header.

This line tells org-mode that
it is a src-block, and language of the src-block. There are
also optional header arguments. See Info node `(org) Header arguments'

"
   (format "The default headers are described here: `org-babel-default-header-args:%s'

" (org-element-property :language element))
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'src-block-header)))


(defun ore-src-block (element)
  "`ore' documentation for a src-block."
  (concat
   (substitute-command-keys "You are in a src-block.

C-c C-c to execute this block.
\\[org-babel-tangle]  org-babel-tangle

You can edit the block with \\[org-edit-special] or `org-edit-special'.

See Info node `(org) Working with source code' for more details.\n\n")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'src-block)))


(defun ore-headline (element)
  "`ore' documentation for a headline."
  (concat
   (when (bolp)
     (format  "You are at the beginning of a headline.

\\[org-cycle] to cycle Info node `(org) Global and local cycling'.

Check `org-use-speed-commands'\n\n"))

   ;; in a headline
   (substitute-command-keys
    "You are in a headline. You can change:

 Visibility with \\[org-cycle]

 TODO state \\[org-shiftleft] and \\[org-shiftright] or
 `org-todo'.

 Your current todo sets can be found in `org-todo-sets'.

 Priority \\[org-shiftup] (`org-priority-up') and
 \\[org-shiftdown] (`org-priority-down')

 Tags  \\[org-ctrl-c-ctrl-c] or `org-set-tags'

 Set a property with \\[org-set-property] `org-set-property'.

 Delete a property with \\[org-delete-property] or `org-delete-property'.

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'headline)))


;; timestamp
(defun ore-timestamp (element)
  "`ore' documentation for timestamps."
  (concat
   (substitute-command-keys "You are on a timestamp.

If you click on it, you will see the date in the agenda. With the
cursor on the <> or [] \\[org-shiftup] and \\[org-shiftdown] will
switch from active to inactive timestamps.

You can change the date by putting the cursor on a date part and
using \\[org-shiftup] and \\[org-shiftdown] or \\[org-shiftleft]
and \\[org-shiftright]

See Info node `(org) Dates and times'.

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'timestamp)))


;; table table-row table-cell
(defun ore-table (element)
  (concat
   "You are in a table.

Move cell to cell with \\[org-cycle]. When you are in the last
cell, adds a new row.

Move rows up and down with \\[org-metaup] and \\[org-metadown].
Move columns left and right with \\[org-metaleft] and \\[org-metaright].

Insert a row with `org-table-insert-row'.
Delete a row with `org-table-kill-row'.

Insert a column with `org-table-insert-column'.
Delete a column with `org-table-delete-column'.

Sort a column with \\[org-sort] `org-sort'. You can sort
alphabetically, numerically, or by time. Use upper case letter to
reverse the sort order.

`C-c -     (`org-table-insert-hline')'
     Insert a horizontal line below current row.  With a prefix
     argument, the line is created above the current line.

`C-c <RET>     (`org-table-hline-and-move')'
     Insert a horizontal line below current row, and move the cursor
     into the row below that line.

You can transpose a table with `org-table-transpose-table-at-point'.

Info node `(org) Tables'.

"
   (format "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'table)))


(defun ore-plain-list (element)
  "`ore' documentation for plain lists."
  (concat
   (substitute-command-keys
    "You are on a plain list.
See Info node `(org) Plain lists'.

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'plain-list)))


(defun ore-item (element)
  "`ore' documentation for items in a list"
  (concat
   (substitute-command-keys
    "You are on an item in a list.

You can move items up and down with \\[org-metaup] or
`org-metaup' and \\[org-meta-down] or `org-metadown'.

You can add a new item with \\[org-meta-return] or
`org-meta-return'.

You can change the indentation of an item with \\[org-metaleft]
or `org-metaleft' and \\[org-meta-right] or `org-meta-right'.

You can change the bullet of the item with \\[org-shiftleft] or
`org-shiftleft' and \\[org-shiftright] or `org-shiftright'.

See Info node `(org) Plain lists' for other things like sorting,
cycling, checkboxes, etc...

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'item)))


(defun ore-keyword (element)
  "`ore' documentation for a keyword."
  (concat
   (format "You are on a keyword.
%s: %s\n" (org-element-property :key element) (org-element-property :value element))
   (substitute-command-keys
    "
You may need to run \\[org-ctrl-c-ctrl-c] or `org-ctrl-c-ctrl-c'
to refresh its value if you change it.

You can move keywords up and down with \\[org-metaup] or
`org-metaup' and \\[org-metadown] or `org-metadown'.

")
   (format "These keywords change export behavior:
%s\n\n" (mapconcat 'identity (org-get-export-keywords) " "))
   (format "These keywords change options:
%s\n\n" (mapconcat 'identity org-options-keywords " "))

   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'keyword)))


(defun ore-paragraph (element)
  "`ore' documentation for a paragraph."
  (concat
   (substitute-command-keys
    "You are in a paragraph.

You can move a paragraph up with \\[org-metaup] or `org-metaup'.

You can move a paragraph up with \\[org-metadown] or `org-metadown'.

You can mark the paragraph with `mark-paragraph'.

Are you looking for general Info node `(org)' help?
Perhaps Info node `(org) Tables'?

")
   (format  "\nClick for details on the face [[face:%s]]\n" (face-at-point))
   (ore-user-documentation 'paragraph)))


;;;###autoload
(defun ore ()
  "Help function for the org-mode element at point."
  (interactive)
  (let* ((oeap (org-element-context))
	 (ore-func (intern (format "ore-%s" (car oeap))))
	 (s (if (fboundp ore-func)
		(funcall ore-func oeap)
	      (format
	       "No documentation found for %s.

%s"
	       (car oeap)
	       (ore-user-documentation (car oeap))))))
    ;; There are some special cases.
    (cond
     ((and  (eq 'src-block (car oeap))
	    (ore-src-block-header-p oeap))
      (setq s (ore-src-block-header oeap)))

     ((or (eq 'table (car oeap))
	  (eq 'table-row (car oeap))
	  (eq 'table-cell (car oeap)))
      (setq s (ore-table oeap)))

     ((or (eq 'latex-fragment (car oeap))
	  (eq 'latex-environment (car oeap)))
      (setq s (ore-latex oeap))))

    (with-help-window
	(help-buffer)
      

      (princ s)
      (princ "\n\nHere is how org-mode sees the element.\n\n")
      (pp oeap)
      (emacs-keybinding-command-tooltip-mode)))
  )


(defun match-next-ore-file (&optional limit)
  "Font-lock function to make file links clickable in help-mode."
  (when  (re-search-forward "\\[\\[file:\\([^]]*\\)\\]\\]" limit t)
    (let* ((fname (expand-file-name
		   (match-string 1)
		   ore-user-directory))
	   (beg (match-beginning 0))
	   (end (match-end 0))
	   (find-func `(lambda ()
			 (interactive)
			 (find-file ,fname))))

      (add-text-properties
       beg
       end
       `(mouse-face
	 highlight
         display "User documentation"
	 local-map ,(let ((map (copy-keymap help-mode-map)))
		      (define-key map [mouse-1] find-func)
		      map)
	 help-echo (format
		    "Click to edit User documentation.\n%s"
		    fname))))))

(defun match-next-ore-face (&optional limit)
  "Font-lock function to make face links clickable in help-mode."
  (when  (re-search-forward "\\[\\[face:\\([^]]*\\)\\]\\]" limit t)
    (let* ((face (match-string 1))
	   (beg (match-beginning 0))
	   (end (match-end 0))
	   (func `(lambda ()
		    (interactive)
		    (describe-face ,face))))

      (add-text-properties
       beg
       end
       `(mouse-face
	 highlight
	 local-map ,(let ((map (copy-keymap help-mode-map)))
		      (define-key map [mouse-1] func)
		      map)
	 display ,face
	 help-echo (format
		    "Click to show face information.\n%s"
		    face))))))

(add-hook 'help-mode-hook
	  (lambda ()
	    (font-lock-add-keywords
	     nil
	     '((match-next-ore-file . font-lock-keyword-face)
	       (match-next-ore-face . font-lock-keyword-face)))))


;; Let's add to the org menu for "Help at point"
(add-hook 'org-mode-hook
	  (lambda ()
	    (easy-menu-change
	     '("Org")
	     "Help"
	     '(["Help at point" ore])
	     "Show/Hide")))

(provide 'ore)

;;; ore.el ends here
