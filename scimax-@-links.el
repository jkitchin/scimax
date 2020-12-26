;;; scimax-@-links.el --- Scimax support for links

;;; Commentary:
;;

(require 'org-db)
(require 'org-ref)

(defcustom scimax-links-candidate-functions
  '(@-links-this-buffer
    @-links-org-buffers
    @-links-stored-links
    org-db-contacts-candidates
    )
  "List of functions that generate candidates")

(defcustom scimax-insert-link-functions
  `(counsel-find-file
    counsel-recentf
    counsel-projectile-switch-project
    ,org-ref-insert-cite-function
    org-db-contacts)
  "Other commands with link actions.")

;; * Augment some projectile functions with links
(defun scimax-projectile-insert-file-link (f)
  "Insert a file link to F in a project."
  (let ((fname (expand-file-name f (projectile-project-root))))
    (when (file-exists-p fname)
      (with-ivy-window
	(insert (format "[[file:%s][%s]]"
			fname
			f))))))


(ivy-add-actions
 'counsel-projectile-find-file
 '(("l" scimax-projectile-insert-file-link "Insert link")))

(ivy-add-actions
 'counsel-projectile-find-dir
 '(("l" scimax-projectile-insert-file-link "Insert link")))

(ivy-add-actions
 'counsel-projectile-switch-project
 '(("l" scimax-projectile-insert-file-link "Insert link")))

(ivy-add-actions
 'counsel-projectile
 '(("l" scimax-projectile-insert-file-link "Insert link")))

(ivy-add-actions
 'counsel-recentf
 '(("l" scimax-projectile-insert-file-link "Insert link")))


;; * Candidate generation functions
(defun @-links-this-buffer ()
  "Return list of candidates in the current buffer."
  (let ((candidates '()))
    ;; Links to labels (figures and tables) in this buffer
    (setq candidates (cl-loop for label in (org-ref-get-labels)
			      collect
			      (list (format "ref:%s" label) #'insert)))

    ;; headings in this buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(let ((title (s-trim (match-string 2)))
	      (id (org-entry-get (point) "ID"))
	      (custom-id (org-entry-get (point) "CUSTOM_ID")))

	  (cl-pushnew (list
		       (format "[[*%s][*%s]]"
			       title
			       title)
		       #'insert)
		      candidates)
	  ;; ID
	  (when id
	    (cl-pushnew (list
			 (format "[[id:%s][*%s]]"
				 id
				 title)
			 #'insert)
			candidates))
	  ;; CUSTOM_ID
	  (when custom-id
	    (cl-pushnew (list
			 (format "[[#%s][%s]]"
				 custom-id
				 title)
			 #'insert)
			candidates)))))
    candidates))

(defun @-links-org-buffers ()
  (let ((candidates '()))
    ;; Links to headings in open org-files
    (setq candidates (append
		      candidates
		      (let* ((org-files (mapcar
					 'buffer-file-name
					 (-filter (lambda (b)
						    (-when-let (f (buffer-file-name b))
						      (f-ext? f "org")))
						  (buffer-list))))
			     (headings '()))
			(cl-loop for file in org-files do
				 (when (file-exists-p file)
				   (with-temp-buffer
				     (insert-file-contents file)
				     (goto-char (point-min))
				     (while (re-search-forward org-heading-regexp nil t)
				       (cl-pushnew (list
						    (format "[[%s::%s][%s]]"
							    file
							    (line-number-at-pos)
							    (match-string 0))
						    #'insert)
						   headings)
				       (cl-pushnew (list
						    (format "[[%s::*%s][%s]]"
							    file
							    (s-trim (match-string 2))
							    (match-string 2))
						    #'insert)
						   headings)))))
			headings)))
    candidates))

(defun @-links-stored-links ()
  (mapcar (lambda (lnk)
	    (list (car lnk) #'insert))
	  org-stored-links))


(defun @-link-candidates ()
  (append '(("@" 'insert))
	  (cl-loop for func in scimax-links-candidate-functions
		   append
		   (funcall func))
	  scimax-insert-link-functions))

(defun @-insert-link ()
  "Insert a link or call a function from `scimax-insert-link-functions'."
  (interactive)
  (let ((candidates (@-link-candidates)))
    (ivy-read "Link: " candidates
	      :action #'(lambda (x)
			  (unless smex-initialized-p
			    (smex-initialize))
			  (cond
			   ((and (symbolp x)
				 (-contains? smex-ido-cache
					     (symbol-name x)))
			    (command-execute (intern-soft x) 'record))
			   ((and (listp x)
				 (functionp (second x)))
			    (funcall (second x) (car x)))
			   ((plist-get (cdr x) :email)
			    (org-db-insert-contact-link x))
			   (t
			    (error "err: %S" x)))))))


;; * @ key-binding

(define-key org-mode-map "@"
  '(menu-item "maybe-@" nil
	      :filter (lambda (&optional _)
                        (unless (looking-back "[a-zA-Z0-9]" 2)
			  #'@-insert-link))))

(provide 'scimax-@-links)

;;; scimax-@-links.el ends here
