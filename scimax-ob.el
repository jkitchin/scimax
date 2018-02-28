;;; scimax-ob.el --- New org-babel functions

;;; Commentary:
;;

;;* Commands like the jupyter notebook has

;;; Code:

(defun scimax-insert-src-block (&optional below)
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


(defun scimax-split-src-block (&optional below)
  "Split the current src block with point in upper block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
	 (p (point))
	 (language (org-element-property :language el))
	 (parameters (org-element-property :parameters el)))

    (beginning-of-line)
    (insert (format "#+END_SRC

#+BEGIN_SRC %s %s\n" language (or parameters "")))
    (unless below
      (beginning-of-line)
      (forward-line -3)
      (forward-char -1))))


(defun scimax-execute-and-next-block (&optional new)
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
	(scimax-insert-src-block t)
      (goto-char (match-beginning 0)))))


(defun scimax-execute-to-point ()
  "Execute all the src blocks that start before point."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
	(org-babel-execute-src-block)))))


(defun scimax-jump-to-visible-block ()
  "Jump to a visible src block with avy."
  (interactive)
  (avy-with scimax-jump-to-block
    (avy--generic-jump "#\\+BEGIN_SRC"  nil avy-style (point-min) (point-max))))


(defun scimax-jump-to-block (&optional N)
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
			(goto-char (point-min))
			(forward-line (1- (second candidate)))
			(outline-show-entry)
			(recenter)))))


(defun scimax-ob-edit-header ()
  "Edit the src-block header in the minibuffer."
  (interactive)
  (let* ((src-info (org-babel-get-src-block-info 'light))
	 (header-start (sixth src-info))
	 (header-end (save-excursion (goto-char header-start)
				     (line-end-position))))
    (setf (buffer-substring header-start header-end)
	  (read-input "Header: "
		      (buffer-substring-no-properties header-start header-end)))))


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
    (scimax-insert-src-block (not below))
    (delete-char 1)
    (insert code)
    ;; jump back to start of new block
    (org-babel-previous-src-block)
    (org-babel-next-src-block)))

;; Move blocks
(defun scimax-ob-move-block-up ()
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

(defun scimax-ob-clear-all-results ()
  "Clear all results in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (org-babel-next-src-block)
      (org-babel-remove-result))))

;; * src keys

(defvar scimax-src-keys '()
  "Keep a list of key definitions for each language.")

(defmacro scimax-define-src-key (language key def)
  "For LANGUAGE (symbol) src blocks, define key sequence KEY as DEF.
KEY should be a string sequence that will be used in a `kbd' sequence.
This is like `define-key', except the definition only applies in
src blocks for a specific LANGUAGE. Adapted from
http://endlessparentheses.com/define-context-aware-keys-in-emacs.html"
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  ;; store the key in scimax-src-keys
  (unless (cdr (assoc language scimax-src-keys))
    (cl-pushnew (list language '()) scimax-src-keys))
  (cl-pushnew (cons key def) (cdr (assoc language scimax-src-keys)))
  `(define-key org-mode-map ,(kbd key)
     '(menu-item
       ,(format "maybe-%s" (or (car (cdr-safe def)) def))
       nil
       :filter (lambda (&optional _)
                 (when (and (org-in-src-block-p)
			    (string= ,(symbol-name language)
				     (car (org-babel-get-src-block-info t))))
		   ,def)))))


(defun scimax-show-src-keys (language)
  "Show a reminder of the keys bound for LANGUAGE blocks."
  (interactive (list (completing-read "Language: " (mapcar 'car scimax-src-keys))))
  (let* ((s (loop for (key . function) in  (cdr (assoc (if (stringp language)
							   (intern-soft language)
							 language)
						       scimax-src-keys))
		  collect
		  (format "%10s  %40s" key function)))
	 (n (length s))
	 (m (floor (/ n 2))))
    (message "%s" (loop for i to m concat
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
      (loop for i from 1 to nlines
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
	    (next-line))))
  ;; This allows you to update the numbers if you change the block, e.g. add/remove lines
  (add-hook 'post-command-hook 'scimax-ob-add-line-numbers nil 'local))

(provide 'scimax-ob)

;;; scimax-ob.el ends here
