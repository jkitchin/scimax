;;; scimax-@-links.el --- Scimax support for links

;;; Commentary:
;; This is some idea to bind @ to insert links to convenient things.
;; - buffer headings
;; - open headings
;; - recentf
;; - projects
;; - org-db-headings?
;; - contacts
;;
;; it is an experiment inspired by a feature in Google docs that prompts you
;; with a dropdown list of things to insert with @.
;;
;; The main function is `@-insert-link' and when you load this library, that is
;; bound to the @ key.

(require 'org-db)
(require 'org-ref)


(defcustom scimax-links-candidate-functions
  '(@-links-this-buffer
    @-links-org-buffers
    @-links-open-files
    @-hashtags
    @-links-stored-links
    @-counsel-recentf-candidates
    @-projectile-relevant-known-projects
    org-db-contacts-candidates)
  "List of functions that generate candidates"
  :group 'scimax-@-link
  :type '(repeat function))


(defcustom scimax-insert-link-functions
  `(counsel-find-file
    counsel-recentf
    counsel-projectile-switch-project
    ,org-ref-insert-cite-function
    org-db-contacts)
  "Other commands with link actions."
  :group 'scimax-@-link
  :type '(repeat function))


;; * Augment some projectile functions with links

(defun scimax-projectile-insert-file-link (f)
  "Insert a file link to F in a project.
F is going to be a string that is a filename or directory."
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
  "Return list of candidates in the current buffer.
The candidates are to:
1. headings
2. headings with ID properties
3. headings with CUSTOM_ID properties

A candidate is a list of (link function)."
  (let ((candidates '()))
    ;; Links to labels (figures and tables) in this buffer
    (setq candidates (cl-loop for label in (org-ref-get-labels)
			      collect
			      (list (format "ref:%s" label) #'insert)))

    ;; headings in this buffer
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
	(let ((title (fifth (org-heading-components)))
	      (id (org-entry-get (point) "ID"))
	      (custom-id (org-entry-get (point) "CUSTOM_ID")))

	  (cl-pushnew (list
		       (format "[[*%s][*%s]]"
			       (org-link-escape  title)
			       (org-link-escape  title))
		       #'insert)
		      candidates)
	  ;; ID
	  (when id
	    (cl-pushnew (list
			 (format "[[id:%s][*%s]]"
				 id
				 (org-link-escape  title))
			 #'insert)
			candidates))
	  ;; CUSTOM_ID
	  (when custom-id
	    (cl-pushnew (list
			 (format "[[#%s][%s]]"
				 custom-id
				 (org-link-escape  title))
			 #'insert)
			candidates)))))
    candidates))


(defun open-org-buffers ()
  "Return a list of buffers to org-files."
  (seq-filter (lambda (b)
		(when-let (f (buffer-file-name b))
		  (string= "org" (file-name-extension f))))
	      (buffer-list)))


(defun @-hashtags ()
  "Get a list of candidate hashtags you have used before."
  (let* ((tip (looking-at-hashtag))
	 (hashtag-data (with-org-db
			(sqlite-select org-db "select
hashtag, file_hashtags.begin, files.filename
from hashtags
left join file_hashtags on hashtags.rowid = file_hashtags.hashtag_id
inner join files on files.rowid = file_hashtags.filename_id"))))
    (-uniq (cl-loop for (hashtag begin fname) in hashtag-data
		    collect (concat "#" hashtag)))))


(defun @-links-org-buffers ()
  "Return candidates in all open org-buffers.
These candidates are to headings by position and by *links."
  (let ((candidates '()))
    ;; Links to headings in open org-files
    (setq candidates (append
		      candidates
		      (let* ((org-bufs (open-org-buffers))
			     title
			     file
			     (headings '()))
			(cl-loop for buf in org-bufs
				 do
				 (setq file (buffer-file-name buf))
				 (when (file-exists-p file)
				   (with-current-buffer buf
				     (save-excursion
				       (goto-char (point-min))
				       (while (re-search-forward org-heading-regexp nil t)
					 (setq title (fifth (org-heading-components)))
					 (cl-pushnew (list
						      (format "[[%s::%s][%s]]"
							      file
							      (line-number-at-pos)
							      (org-link-escape  title))
						      #'insert)
						     headings)
					 (cl-pushnew (list
						      (format "[[%s::*%s][%s]]"
							      file
							      (org-link-escape  title)
							      (org-link-escape  title))
						      #'insert)
						     headings))))))
			headings)))
    candidates))


(defun @-links-open-files ()
  "Return links to all open files."
  (delete nil
	  (mapcar (lambda (buf)
		    (when-let ((fname (buffer-file-name buf)))
		      (format "[[file:%s]]" fname)))
		  (buffer-list))))


(defun @-links-stored-links ()
  "Return list of stored links"
  (mapcar (lambda (lnk)
	    `(,(car lnk) insert))
	  org-stored-links))


(defun @-counsel-recentf-candidates ()
  "Returns list of links to recentf candidates."
  (mapcar (lambda (lnk)
	    (list
	     (format "[[file:%s]]" lnk)
	     'insert))
	  (counsel-recentf-candidates)))


(defun @-projectile-relevant-known-projects ()
  (mapcar (lambda (lnk)
	    (list
	     (format "[[file:%s]]" lnk)
	     'insert))
	  (projectile-relevant-known-projects)))



(defun @-link-candidates ()
  "Return all the candidates"
  (append '(("@" insert))
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
			   ((stringp x)
			    (insert x))
			   ((and (listp x)
				 (functionp (second x)))
			    (funcall (second x) (car x)))
			   ;; a contact
			   ((plist-get (cdr x) :email)
			    (insert (format "[[contact:%s][%s]]"
					    (plist-get (cdr x) :email)
					    (plist-get (cdr x) :title))))
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
