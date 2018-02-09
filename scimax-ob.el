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
	     (switches (org-element-property :switches src))
	     (parameters (org-element-property :parameters src))
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
    ;; Not in a src block
    (beginning-of-line)
    (insert (format "\n#+BEGIN_SRC %s %s %s

#+END_SRC\n" (completing-read "Language: " (mapcar 'car org-babel-load-languages))))
    (forward-line -2)))


(defun scimax-split-src-block (&optional below)
  "Split the current src block.
With a prefix BELOW move point to lower block."
  (interactive "P")
  (let* ((el (org-element-context))
	 (language (org-element-property :language el))
	 (parameters (org-element-property :parameters el)))

    (beginning-of-line)
    (insert (format "#+END_SRC

#+BEGIN_SRC %s %s\n" language (or parameters "")))
    (beginning-of-line)
    (when (not below)
      (org-babel-previous-src-block))))


(defun scimax-execute-and-next-block ()
  "Execute this block and either jump to the next one, or add a new one."
  (interactive)
  (org-babel-execute-src-block)
  (let ((next-block (save-excursion (re-search-forward org-babel-src-block-regexp nil t))))
    (if next-block
	(goto-char (match-beginning 0))
      (scimax-insert-src-block t))))




;; * Src block keymaps
(defcustom scimax-src-block-keymaps
  "alist of keymaps for different languages."
  '()
  :group 'scimax)

(defun scimax-add-keymap-to-src-blocks (limit)
  "Add keymaps to src-blocks defined in `scimax-src-block-keymaps'."
  (let ((case-fold-search t)
        lang)
    (while (re-search-forward org-babel-src-block-regexp limit t)
      (let ((lang (match-string 2))
            (beg (match-beginning 0))
            (end (match-end 0)))
        (if (assoc (org-no-properties lang) scimax-src-block-keymaps)
            (progn
              (add-text-properties
               beg end `(local-map ,(cdr (assoc
                                          (org-no-properties lang)
                                          scimax-src-block-keymaps))))
              (add-text-properties
               beg end `(cursor-sensor-functions
                         ((lambda (win prev-pos sym)
                            ;; This simulates a mouse click and makes a menu change
                            (org-mouse-down-mouse nil)))))))))))


(defun scimax-spoof-mode (orig-func &rest args)
  "Advice function to spoof commands in org-mode src blocks.
It is for commands that depend on the major mode. One example is
`lispy--eval'."
  (if (org-in-src-block-p)
      (let ((major-mode (intern (format "%s-mode" (first (org-babel-get-src-block-info))))))
        (apply orig-func args))
    (apply orig-func args)))


(define-minor-mode scimax-src-keymap-mode
  "Minor mode to add mode keymaps to src-blocks."
  :init-value nil
  (if scimax-src-keymap-mode
      (progn
        (add-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks t)
        (add-to-list 'font-lock-extra-managed-props 'local-map)
        (add-to-list 'font-lock-extra-managed-props 'cursor-sensor-functions)
        (advice-add 'lispy--eval :around 'scimax-spoof-mode)
        (cursor-sensor-mode +1))
    (remove-hook 'org-font-lock-hook #'scimax-add-keymap-to-src-blocks)
    (advice-remove 'lispy--eval 'scimax-spoof-mode)
    (cursor-sensor-mode -1))
  (font-lock-fontify-buffer))

(add-hook 'org-mode-hook (lambda ()
                           (scimax-src-keymap-mode +1)))



(provide 'scimax-ob)

;;; scimax-ob.el ends here
