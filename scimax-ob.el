;;; scimax-ob.el --- New org-babel functions

;;; Commentary:
;;

;;* Commands like the jupyter notebook has

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
	      (forward-line -2))
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


(defun scimax-execute-and-next-block ()
  "Execute this block and either jump to the next one, or add a new one."
  (interactive)
  (org-babel-execute-src-block)
  (let ((next-block (save-excursion (org-babel-next-src-block))))
    (if next-block
	(goto-char (match-beginning 0))
      (scimax-insert-src-block t)))
  (recenter 10))


(defun scimax-execute-to-point ()
  "Execute all the src blocks that start before point."
  (interactive)
  (let ((p (point)))
    (save-excursion
      (goto-char (point-min))
      (while (and (org-babel-next-src-block) (< (point) p))
	(org-babel-execute-src-block)))))


(defun scimax-jump-to-block (&optional N)
  "Jump to a block in the buffer.
If narrowing is in effect, only a block in the narrowed region.
Use a numeric prefix to specify how many lines of context to use.
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

;; * src keys

(defmacro scimax-define-src-key (language key def)
  "For LANGUAGE (symbol) src blocks, define key sequence KEY as DEF.
This is like `define-key', except the definition only applies in
src blocks for a specific LANGUAGE. Adapted from
http://endlessparentheses.com/define-context-aware-keys-in-emacs.html"
  (declare (indent 3)
           (debug (form form form &rest sexp)))
  `(define-key org-mode-map ,key
     '(menu-item
       ,(format "maybe-%s" (or (car (cdr-safe def)) def))
       nil
       :filter (lambda (&optional _)
                 (when (and (org-in-src-block-p)
			    (string= ,(symbol-name language)
				     (car (org-babel-get-src-block-info t))))
		   ,def)))))


(provide 'scimax-ob)

;;; scimax-ob.el ends here
