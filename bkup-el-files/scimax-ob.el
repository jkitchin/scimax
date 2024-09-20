;;; scimax-ob.el --- New org-babel functions for src blocks

;;; Commentary:
;; Inspired by some editing commands in Jupyter notebook I created these
;; functions to make regular src blocks easier to work with.

;;; Code:

(require 's)
(require 'dash)
(require 'avy)

;; * create/modify blocks

(defun scimax-ob-insert-src-block (&optional below)
  "Insert a src block above the current point.
With prefix arg BELOW, insert it below the current point.

If point is in a block, copy the header to the new block"
  (interactive "P")
  (if (org-in-src-block-p)
      (let* ((src (org-element-context))
	     (start (org-element-property :begin src))
	     (end (org-element-property :end src))
	     (lang (org-element-property :language src))
	     (switches (or (org-element-property :switches src) ""))
	     (parameters (or (org-element-property :parameters src) ""))
	     location)
	(if below
	    (progn
	      (goto-char start)
	      (setq location (org-babel-where-is-src-block-result nil nil))
	      (if (not  location)
		  (goto-char end)
		(goto-char location)
		(goto-char (org-element-property :end (org-element-context))))
	      (insert (format "\n#+BEGIN_SRC %s %s %s

#+END_SRC\n\n" lang switches parameters))
	      (forward-line -3))
	  ;; after current block
	  (goto-char (org-element-property :begin (org-element-context)))
	  (insert (format "\n#+BEGIN_SRC %s %s %s

#+END_SRC\n\n" lang switches parameters))
	  (forward-line -3)))

    ;; Not in a src block, just insert a block
    (beginning-of-line)
    (insert (format "\n#+BEGIN_SRC %s
#+END_SRC\n" (completing-read "Language: " (mapcar 'car org-babel-load-languages))))
    (forward-line -1)))


(defun scimax-ob-split-src-block (&optional below)
  "Split the current src block with point in upper block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
	 (p (point))
	 (language (org-element-property :language el))
	 (switches (org-element-property :switches el))
	 (parameters (org-element-property :parameters el)))

    (beginning-of-line)
    (insert (format "#+END_SRC

#+BEGIN_SRC %s %s %s\n" language (or switches "") (or parameters "")))
    (unless below
      (beginning-of-line)
      (forward-line -3)
      (forward-char -1))))

;; * Navigating in block

(defun scimax-ob-edit-up ()
  "Move to previous line, unless at the top.
In that case first move to beginning of line, and then move to
previous cell."
  (interactive)
  (let ((first-code-line-p (save-excursion
			     (forward-line -1)
			     (beginning-of-line)
			     (looking-at "#\\+BEGIN_SRC")))
	(lang (org-element-property :language (org-element-context))))
    (cond
     ((and (bolp) first-code-line-p)
      (ignore-errors
	(catch 'block
	  (while (org-babel-previous-src-block)
	    (when (string= lang
			   (org-element-property :language (org-element-context)))
	      (throw 'block t))))))
     (first-code-line-p
      (beginning-of-line))
     (t
      (forward-line -1)))))


(defun scimax-ob-edit-down ()
  "Move to next line, unless at the bottom.
In that case first move to beginning of line, and then move to
next cell."
  (interactive)
  (let ((last-code-line-p (save-excursion
			    (forward-line 1)
			    (beginning-of-line)
			    (looking-at "#\\+END_SRC")))
	(lang (org-element-property :language (org-element-context))))
    (cond
     ((and (eolp) last-code-line-p)
      (ignore-errors
	(catch 'block
	  (while (org-babel-next-src-block)
	    (when (string= (org-element-property :language (org-element-context))
			   lang)
	      (throw 'block t))))))
     (last-code-line-p
      (end-of-line))
     (t
      (forward-line)))))

;; * Executing blocks

(defun scimax-ob-execute-and-next-block (&optional new)
  "Execute this block and either jump to the next block with the
same language, or add a new one.
With prefix arg NEW, always insert new cell."
  (interactive "P")
  (org-babel-execute-src-block)
  ;; we ignore-errors here because when there is no next block it is a
  ;; user-error, not nil.
  (let* ((lang (car (org-babel-get-src-block-info t)))
	 (next-block (ignore-errors
		       (save-excursion
			 (catch 'block
			   (while (setq next-block (org-babel-next-src-block))
			     (when (string= lang (org-element-property :language (org-element-context)))
			       (throw 'block next-block))))))))
    (if (or new (not next-block))
	(scimax-ob-insert-src-block t)
      (goto-char (match-beginning 0)))))


(defun scimax-ob-execute-to-point ()
  "Execute all the src blocks that start before point."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
	(org-babel-execute-src-block)))))

;; * Jumping to blocks

(defun scimax-ob-jump-to-visible-src-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (org-mark-ring-push)
  (avy-with scimax-ob-jump-to-block
    (avy-jump "#\\+BEGIN_SRC"  :window-flip nil :beg (point-min) :end (point-max))))


(defun scimax-ob-jump-to-src-block (&optional N)
  "Jump to a block in the buffer.
If narrowing is in effect, only a block in the narrowed region.
Use a numeric prefix N to specify how many lines of context to use.
Defaults to 3."
  (interactive "p")
  (let ((p '()))
    (when (= 1 N) (setq N 3))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
	(push (list (format "line %s:\n%s"
			    (line-number-at-pos (match-beginning 0))
			    (save-excursion
			      (goto-char (match-beginning 0))
			      (let ((s (point)))
				(forward-line N)
				(buffer-substring s (point)))))
		    (line-number-at-pos (match-beginning 0)))
	      p)))
    (ivy-read "block: " (reverse p)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (point-min))
			(forward-line (1- (second candidate)))
			(outline-show-entry)
			(recenter)))))


(defun scimax-ob-jump-to-inline-src ()
  "Jump to an inline src element in the buffer."
  (interactive)
  (let ((p '()))
    (org-element-map (org-element-parse-buffer) 'inline-src-block
      (lambda (isrc)
	(push (list  (buffer-substring (org-element-property :begin isrc) (org-element-property :end isrc))
		     (org-element-property :begin isrc))
	      p)))
    (ivy-read "inline: " (reverse p)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (second candidate))
			(recenter)))))

;; * Jump to positions in blocks

(defun scimax-ob-jump-to-header ()
  "Jump to src header."
  (interactive)
  (org-mark-ring-push)
  (let* ((src-info (org-babel-get-src-block-info 'light))
	 (header-start (sixth src-info)))
    (goto-char header-start)))


(defun scimax-ob-jump-to-first-line ()
  "Move point to start of first line in the src block."
  (interactive)
  (org-mark-ring-push)
  (org-edit-special)
  (goto-char (point-min))
  (org-edit-src-exit))


(defun scimax-ob-jump-to-end-line ()
  "Move point to end of last line in the src block."
  (interactive)
  (org-mark-ring-push)
  (org-edit-special)
  (goto-char (point-max))
  (org-edit-src-exit))


(defun scimax-ob-jump-to-end ()
  "Jump to src block end."
  (interactive)
  (org-mark-ring-push)
  (let* ((src (org-element-context))
	 (nlines (org-element-property :post-blank src)))

    (goto-char (org-element-property :end src))
    (when (numberp nlines)
      (forward-line (* -1 (cl-incf nlines))))
    (goto-char (line-end-position))))

;; * kill/copy/clone

;; kill block
(defun scimax-ob-kill-block-and-results ()
  "Kill the block and its results."
  (interactive)
  (let ((src (org-element-context))
	(result-start (org-babel-where-is-src-block-result))
	end)
    (if result-start
	(save-excursion
	  (goto-char result-start)
	  (setq end (org-babel-result-end)))
      (setq end (org-element-property :end src)))
    (kill-region
     (org-element-property :begin src)
     end)))


;; copy block
(defun scimax-ob-copy-block-and-results ()
  "Copy the block and its results."
  (interactive)
  (let ((src (org-element-context))
	(result-start (org-babel-where-is-src-block-result))
	end)
    (if result-start
	(save-excursion
	  (goto-char result-start)
	  (setq end (org-babel-result-end)))
      (setq end (org-element-property :end src)))

    (kill-new
     (buffer-substring
      (org-element-property :begin src)
      end))))


;; clone block
(defun scimax-ob-clone-block (&optional below)
  "Clone the block."
  (interactive "P")
  (let* ((src (org-element-context))
	 (code (org-element-property :value src)))
    (scimax-ob-insert-src-block (not below))
    (delete-char 1)
    (insert code)
    ;; jump back to start of new block
    (org-babel-previous-src-block)
    (org-babel-next-src-block)))


;; * Move blocks

(defun scimax-ob-move-src-block-up ()
  "Move block before previous one."
  (interactive)
  (let ((src (org-element-context)))
    (kill-region
     (org-element-property :begin src)
     (org-element-property :end src)))
  (org-babel-previous-src-block)
  (org-yank))


(defun scimax-ob-move-src-block-down ()
  "Move block down."
  (interactive)
  (let ((src (org-element-context)))
    (kill-region
     (org-element-property :begin src)
     (org-element-property :end src)))
  (org-babel-next-src-block)
  (goto-char (org-element-property :end (org-element-context)))
  (forward-line)
  (org-yank))


;; * Merging blocks

(defun scimax-ob-create-header-string ()
  "Build up an org-babel header argument string with completion and return it."
  (interactive)
  (unless (org-in-src-block-p) (user-error "Not in src-block"))
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (header-vals (org-babel-combine-header-arg-lists
		       org-babel-common-header-args-w-values
		       (when (boundp lang-headers) (eval lang-headers t))))
	 (headers (mapcar 'car header-vals))
	 (header (completing-read
		  "Header (C-M-j to finish): "
		  headers nil nil "^"))
	 (vals (cdr (assoc (intern-soft header) header-vals)))
	 (header-string (concat ":" header " ")))
    (cond
     ((null vals)
      nil)
     ((eq vals :any)
      (setq header-string (concat header-string " " (read-string "Value: "))))
     ((and (listp vals) (not (listp (car vals))))
      (setq header-string (concat header-string
				  (let ((s (ivy-read
					    "choose (C-M-j for none): " vals
					    :initial-input "^")))
				    (if (string= "^" s)
					""
				      s)))))
     ;; list of lists
     (t
      (setq header-string (concat header-string
				  (s-join " " (-filter
					       (lambda (s)
						 (not
						  (s-blank? s)))
					       (cl-loop for lst in vals
							collect
							(let ((s (ivy-read
								  "choose (C-M-j for none): "
								  lst
								  :initial-input "^")))
							  (if (string= "^" s)
							      ""
							    s)))))))))

    header-string))


(defun scimax-ob-merge-previous ()
  "Merge current block with previous one."
  (interactive)
  (let ((p (point))
	(lang (org-element-property :language (org-element-context))))
    (org-babel-previous-src-block)
    (when (string= lang (org-element-property :language (org-element-context)))
      (scimax-ob-merge-blocks (point) p))))


(defun scimax-ob-merge-next ()
  "Merge current block with next one."
  (interactive)
  (let ((p (point))
	(lang (org-element-property :language (org-element-context))))
    (org-babel-next-src-block)
    (when (string= lang (org-element-property :language (org-element-context)))
      (scimax-ob-merge-blocks p (point)))))

;; * convenience functions

(defun scimax-ob-edit-header ()
  "Edit the src-block header in the minibuffer."
  (interactive)
  (let* ((src-info (org-babel-get-src-block-info 'light))
	 (header-start (sixth src-info))
	 (header-end (save-excursion (goto-char header-start)
				     (line-end-position))))
    (cl--set-buffer-substring
     header-start header-end
     (read-string "Header: "
		  (buffer-substring-no-properties header-start header-end)))))


(defun scimax-ob-clear-all-results ()
  "Clear all results in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-babel-next-src-block)
      (org-babel-remove-result))))


(defun scimax-ob-toggle-output ()
  "Toggle folded state of results if there are some."
  (interactive)
  (when-let (loc (org-babel-where-is-src-block-result))
    (save-excursion
      (goto-char loc)
      (org-cycle))))


(defun scimax-ob-mark-code ()
  "Mark the code in the block."
  (interactive)
  (org-edit-special)
  (let ((p0 (point-min))
	(p1 (point-max)))
    (goto-char p0)
    (org-edit-src-exit)
    (set-mark (point))
    (goto-char (+ (point) (- p1 2)))))


;; * src keys for specific languages

(defvar scimax-ob-src-keys '()
  "Keep a list of key definitions for each language.")


(defmacro scimax-ob-define-src-key (language key def)
  "For LANGUAGE (symbol) src blocks, define key sequence KEY as DEF.
KEY should be a string sequence that will be used in a `kbd' sequence.
This is like `define-key', except the definition only applies in
src blocks for a specific LANGUAGE.

If language is nil apply to all src-blocks.

Adapted from
http://endlessparentheses.com/define-context-aware-keys-in-emacs.html"
  (declare (indent 3)
	   (debug (form form form &rest sexp)))
  ;; store the key in scimax-src-keys
  (unless (cdr (assoc language scimax-ob-src-keys))
    (cl-pushnew (list language '()) scimax-ob-src-keys))

  (cl-pushnew (cons key def) (cdr (assoc language scimax-ob-src-keys)))

  `(define-key org-mode-map ,(kbd key)
     '(menu-item
       ,(format "maybe-%s" (or (car (cdr-safe def)) def))
       nil
       :filter (lambda (&optional _)
		 ,(if language
		      `(when (and (org-in-src-block-p)
				  (string= ,(symbol-name language)
					   (car (org-babel-get-src-block-info t))))
			 ,def)
		    `(when (org-in-src-block-p)
		       ,def))))))

(defcustom scimax-ob-src-key-bindings
  '(("<return>" . #'newline-and-indent)
    ("C-<return>" . #'org-ctrl-c-ctrl-c)
    ("S-<return>" . #'scimax-ob-execute-and-next-block)
    ("M-<return>" . (lambda ()
		      (interactive)
		      (scimax-ob-execute-and-next-block t)))
    ("M-S-<return>" . #'scimax-ob-execute-to-point)
    ("C-M-<return>" . #'org-babel-execute-buffer)
    ("s-." . #'scimax-ob/body))
  "Keybindings for src-blocks."
  :type '(alist :key-type string :value-type function)
  :group 'scimax)


(defun scimax-ob-src-key-bindings ()
  "Function to define key-bindings.
Usually called in a hook function."
  ;; These should work in every src-block IMO.
  (cl-loop for (key . cmd) in scimax-ob-src-key-bindings
	   do
	   (eval `(scimax-ob-define-src-key nil ,key ,cmd))))

;; I think this hook needs to be run at the end.
(add-hook 'org-mode-hook 'scimax-ob-src-key-bindings t)

(defun scimax-ob-show-src-keys (language)
  "Show a reminder of the keys bound for LANGUAGE blocks."
  (interactive (list (completing-read "Language: " (mapcar 'car scimax-ob-src-keys))))
  (let* ((s (cl-loop for (key . function) in  (cdr (assoc (if (stringp language)
							      (intern-soft language)
							    language)
							  scimax-ob-src-keys))
		     collect
		     (format "%10s  %40s" key function)))
	 (n (length s))
	 (m (floor (/ n 2))))
    (message "%s" (cl-loop for i to m concat
			   (s-join " | "
				   (append (-slice s (* i 3) (* 3 (+ i 1)))
					   '("\n")))))))



;; * line numbers
(defvar scimax-ob-number-line-overlays '()
  "List of overlays for line numbers.")

(make-variable-buffer-local 'scimax-ob-number-line-overlays)

(defun scimax-ob-toggle-line-numbers ()
  (interactive)
  (if scimax-ob-number-line-overlays
      (scimax-ob-remove-line-numbers)
    (scimax-ob-add-line-numbers)))


(defun scimax-ob-remove-line-numbers ()
  "Remove line numbers from "
  (interactive)
  (mapc 'delete-overlay
	scimax-ob-number-line-overlays)
  (setq-local scimax-ob-number-line-overlays '())
  (remove-hook 'post-command-hook 'scimax-ob-add-line-numbers t))


(defun scimax-ob-add-line-numbers ()
  "Add line numbers to an org src-block."
  (interactive)
  (save-excursion
    (let* ((src-block (org-element-context))
	   (nlines (- (length
		       (s-split
			"\n"
			(org-element-property :value src-block)))
		      1)))
      ;; clear any existing overlays
      (scimax-ob-remove-line-numbers)

      (goto-char (org-element-property :begin src-block))
      ;; the beginning may be header, so we move forward to get the #+BEGIN
      ;; line. Then jump one more to get in the code block
      (while (not (looking-at "#\\+BEGIN"))
	(forward-line))
      (forward-line)
      (cl-loop for i from 1 to nlines
	       do
	       (beginning-of-line)
	       (let (ov)
		 (setq ov (make-overlay (point)(point)))
		 (overlay-put
		  ov
		  'before-string (propertize
				  (format "%03s " (number-to-string i))
				  'font-lock-face '(:foreground "black" :background "gray80")))
		 (push ov scimax-ob-number-line-overlays))
	       (forward-line))))
  ;; This allows you to update the numbers if you change the block, e.g. add/remove lines
  (add-hook 'post-command-hook 'scimax-ob-add-line-numbers nil 'local))

;; * Header editing

(defun scimax-ob-avy-jump-to-header ()
  "Jump to a position in the header using avy."
  (interactive)
  (unless (org-in-src-block-p) (user-error "Not in src-block"))
  (let* ((src (org-element-context))
	 (begin (org-element-property :begin src))
	 (header-end (save-excursion
		       (goto-char (org-element-property :post-affiliated src))
		       (line-end-position)))
	 (posns '()))
    (save-excursion
      (goto-char begin)
      (while (re-search-forward " " header-end t)
	(push (1+ (match-beginning 0)) posns))
      ;; put last point in too so we can add new args
      (push (line-end-position) posns))
    (org-mark-ring-push)
    (avy-with ob-header
      (avy-process (reverse posns) (avy--style-fn avy-style)))))


(defun scimax-ob-create-header-string ()
  "Build up an org-babel header argument string with completion and return it."
  (interactive)
  (unless (org-in-src-block-p) (user-error "Not in src-block"))
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (header-vals (org-babel-combine-header-arg-lists
		       org-babel-common-header-args-w-values
		       (when (boundp lang-headers) (eval lang-headers t))))
	 (headers (mapcar 'car header-vals))
	 (header (completing-read
		  "Header (C-M-j to finish): "
		  headers nil nil "^"))
	 (vals (cdr (assoc (intern-soft header) header-vals)))
	 (header-string (concat ":" header " ")))
    (cond
     ((null vals)
      nil)
     ((eq vals :any)
      (setq header-string (concat header-string " " (read-string "Value: "))))
     ((and (listp vals) (not (listp (car vals))))
      (setq header-string (concat header-string
				  (let ((s (ivy-read
					    "choose (C-M-j for none): " vals
					    :initial-input "^")))
				    (if (string= "^" s)
					""
				      s)))))
     ;; list of lists
     (t
      (setq header-string (concat header-string
				  (s-join " " (-filter
					       (lambda (s)
						 (not
						  (s-blank? s)))
					       (cl-loop for lst in vals
							collect
							(let ((s (ivy-read
								  "choose (C-M-j for none): "
								  lst
								  :initial-input "^")))
							  (if (string= "^" s)
							      ""
							    s)))))))))

    header-string))


(defun scimax-ob-replace-header-item-with-completion (&optional delete)
  "Jump to a position in the header with avy, then replace it with completion.
With a prefix arg, delete the thing you jumped to."
  (interactive "P")
  (unless (org-in-src-block-p) (user-error "Not in src-block"))
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (headers (org-babel-combine-header-arg-lists
		   org-babel-common-header-args-w-values
		   (when (boundp lang-headers) (eval lang-headers t))))
	 (begin (sixth info))
	 (posns '())
	 header
	 (point-choice)
	 new-header
	 (choice (save-excursion
		   (goto-char begin)
		   (re-search-forward lang)
		   (while (re-search-forward " [a-zA-Z0-9]" (line-end-position) t)
		     (push (1+ (match-beginning 0)) posns))
		   (push (line-end-position) posns)
		   (avy-with ob-header
		     (avy-process (reverse posns) (avy--style-fn avy-style)))
		   (when (eolp) (setq new-header t))
		   (setq point-choice (point))
		   ;; get the header we are in
		   (save-excursion
		     (re-search-backward ":\\(.*\\) ")
		     (setq header (intern-soft (match-string 1))))
		   ;; now get the value we are on
		   (let ((p (point)))
		     (re-search-forward ":" (line-end-position) 'mv)
		     (s-trim
		      (buffer-substring-no-properties p (if (looking-back ":" 1)
							    (1- (point))
							  (point)))))))
	 ;; these are the possible values for the header
	 (header-vals (cdr (assoc header headers)))
	 new-value)

    ;; now we have the symbol for the header and the current value. We have to
    ;; get a new value. There are three types of vals possible, :any, an item in
    ;; a list, or a list of lists
    (unless delete
      (setq new-value
	    (if new-header
		(scimax-ob-create-header-string)
	      (cond
	       ;; any thing is ok
	       ((and (stringp header-vals) (string= ":any" header-vals))
		(read-string "any: " choice))
	       ;; a list of values (although sometimes :any is in the
	       ((and (listp header-vals)
		     (not (listp (car header-vals))))
		(ivy-read "Value: " header-vals))
	       ;; this probably means it is a list of lists
	       (t
		;; get the list that
		(ivy-read "Value: " (catch 'collection
				      (cl-loop for lst in header-vals
					       do (message "%s" lst)
					       (when (-contains? lst (intern-soft choice))
						 (throw 'collection lst))))))))))
    (save-excursion
      (goto-char point-choice)
      (when (eolp)
	(skip-chars-backward " ")
	(delete-region (point) (line-end-position)))
      (unless (looking-back " " 1) (insert " "))
      (cl--set-buffer-substring (point) (or (re-search-forward " " (line-end-position) 'mv)
					    (line-end-position))
				(if delete ""
				  (concat new-value " "))))))




(defun scimax-ob-cycle-header-1 (&optional arg)
  "Cycle the header string through the list of headers.
The strings are defined in SRC-HEADERS file tags.
With a prefix arg cycle backwards."
  (interactive "P")
  (let* ((lang (car (org-babel-get-src-block-info t)))
	 (headers (org-element-map (org-element-parse-buffer) 'keyword
		    (lambda (key)
		      (when (string= (org-element-property :key key) "SRC-HEADERS")
			(org-element-property :value key)))))
	 header index)
    (save-excursion
      (org-babel-goto-src-block-head)
      (re-search-forward lang)
      (setq header (buffer-substring-no-properties (point) (line-end-position))
	    index (-find-index (lambda (s) (string= (s-trim s) (s-trim header))) headers))
      (delete-region (point) (line-end-position))
      (insert " " (if index
		      (if arg
			  (nth (mod (1- index) (length headers)) headers)
			(nth (mod (1+ index) (length headers)) headers))
		    (car headers))))))


(defhydra scimax-ob-cycle-header-strings (:color red)
  "cycle header args"
  ("<left>" (scimax-ob-cycle-header-1 t))
  ("<right>" (scimax-ob-cycle-header-1))
  ("q" nil))


;; * a hydra for src blocks

(defhydra scimax-ob (:color red :hint nil)
  "
	Execute                   Navigate                 Edit             Misc
-----------------------------------------------------------------------------------------------------------------------------
    _<return>_: current           _i_: previous src        _w_: move up       ^ ^                         _<up>_:
  _S-<return>_: current and next  _k_: next src            _s_: move down     _l_: clear result  _<left>_:           _<right>_:
  _M-<return>_: current and new   _q_: visible src         _x_: kill          _L_: clear all              _<down>_:
_S-M-<return>_: to point          _Q_: any src             _n_: copy          _o_: toggle result folding
_C-M-<return>_: buffer       _C-<up>_: goto src start      _c_: clone         _N_: toggle line numbers
	   ^ ^             _C-<down>_: goto src end        _mm_: merge region
	   ^ ^             _C-<left>_: word left           _mp_: merge prev
	   ^ ^            _C-<right>_: word right          _mn_: merge next
	   ^ ^                  _C-<_: src begin           _-_: split
	   ^ ^                  _C->_: src end             _+_: insert above
	   ^ ^                    _R_: results             _=_: insert below
	   ^ ^                    ^ ^                      _h_: header
_;_: dwim comment  _z_: undo  _y_: redo _r_: Goto repl

"
  ("o" scimax-ob-toggle-output :color red)
  ("<up>" scimax-ob-edit-up :color red)
  ("<down>" scimax-ob-edit-down :color red)
  ("<left>" left-char :color red)
  ("<right>" right-char :color red)
  ("C-<up>" scimax-ob-jump-to-first-line :color red)
  ("C-<down>" scimax-ob-jump-to-end-line :color red)
  ("C-<left>" left-word :color red)
  ("C-<right>" right-word :color red)

  ("z" undo-tree-undo :color red)
  ("y" undo-tree-redo :color red)

  ("<return>" org-ctrl-c-ctrl-c :color blue)
  ("S-<return>" scimax-ob-execute-and-next-block :color red)
  ("M-<return>" (lambda ()
		  "Execute and insert new block."
		  (interactive)
		  (scimax-ob-execute-and-next-block t)
		  (font-lock-fontify-block)) :color red)
  ("S-M-<return>" scimax-ob-execute-to-point :color blue)
  ("C-M-<return>" org-babel-execute-buffer :color blue)
  ("r" org-babel-switch-to-session)
  ("N" scimax-ob-toggle-line-numbers)

  ("i" org-babel-previous-src-block :color red)
  ("k" org-babel-next-src-block :color red)
  ("q" scimax-ob-jump-to-visible-block)
  ("Q" scimax-ob-jump-to-block)
  ("C-<" scimax-ob-jump-to-first-line)
  ("C->" scimax-ob-jump-to-end-line)
  ("R" (goto-char (org-babel-where-is-src-block-result)))

  ("w" scimax-ob-move-src-block-up :color red)
  ("s" scimax-ob-move-src-block-down :color red)
  ("x" scimax-ob-kill-block-and-results)
  ("n" scimax-ob-copy-block-and-results)
  ("c" scimax-ob-clone-block)
  ("mm" scimax-ob-merge-blocks)
  ("mp" scimax-ob-merge-previous)
  ("mn" scimax-ob-merge-next)
  ("-" scimax-ob-split-src-block )
  ("+" scimax-ob-insert-src-block)
  ("=" (scimax-ob-insert-src-block t))
  ("l" org-babel-remove-result)
  ("L" scimax-ob-clear-all-results)
  ("h" scimax-ob-edit-header)
  (";" org-comment-dwim :color red))


(provide 'scimax-ob)

;;; scimax-ob.el ends here
