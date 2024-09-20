;;; scimax-org-attachments.el --- scimax enhancements for org-attachments

;;; Commentary:
;;

(setq org-attach-method 'mv
      org-attach-store-link-p 'attached)

;; TODO
;; a hydra or transient menu?

(defun scimax-mac-attach-from-finder ()
  "Attach a file from Finder on MacOS."
  (interactive)
  (let* ((item (as-get-selected-finder-items))
	 (fields (split-string item "::split::"))
	 (file-link (first fields))
	 (file (substring file-link (length "file://"))))
    (if (file-directory-p file)
	(scimax-org-attach-attach-directory-mv file)
      (org-attach-attach file))))


(defun scimax-org-attach-action (x)
  "Attach X to a heading."
  (cond
   ((and (stringp x)
	 (file-exists-p x))
    (org-attach-attach x))
   ((and (listp x)
	 (stringp (first x))
	 (file-exists-p (first x)))
    (org-attach-attach (first x)))
   (t
    (message "I don't know how to attach %S" x))))


(defun scimax-org-attach-attach-advice (&rest file)
  "Add link to attached file in the property"
  (let ((attach (pop org-stored-links)))
    (org-entry-put (point) "ATTACHMENTS"
		   (concat
		    (org-entry-get (point) "ATTACHMENTS")
		    (format " [[%s][%s]]" (first attach) (second attach))))))

(advice-add 'org-attach-attach :after 'scimax-org-attach-attach-advice)


(ivy-add-actions
 'counsel-projectile-find-file
 '(("A" scimax-org-attach-action "Attach to heading")))

(ivy-add-actions
 'counsel-find-file
 '(("A" scimax-org-attach-action "Attach to heading")))

(ivy-add-actions
 'counsel-projectile-find-dir
 '(("A" scimax-org-attach-action "Attach to heading")))

(ivy-add-actions
 'counsel-projectile-switch-project
 '(("A" scimax-org-attach-action "Attach to heading")))

(ivy-add-actions
 'counsel-projectile
 '(("A" scimax-org-attach-action "Attach to heading")))

(ivy-add-actions
 'counsel-recentf
 '(("A" scimax-org-attach-action "Attach to heading")))

(defun scimax-org-attach-attach-directory-cp (dir)
  "Copy DIR as an attachment to the current heading."
  (interactive "DDir: ")
  (copy-directory dir (file-name-as-directory (org-attach-dir 'get-create)))
  (org-attach-sync)
  (org-entry-put (point) "ATTACHMENTS"
		 (concat
		  (org-entry-get (point) "ATTACHMENTS")
		  (format " [[attachment:%s]]" (car (last (f-split dir)))))))


(defun scimax-org-attach-attach-directory-mv (dir)
  "Move DIR as an attachment to the current heading."
  (interactive "DDir: ")
  (scimax-org-attach-attach-directory-cp dir)
  (delete-directory dir t t))


(defun scimax-org-attach (file-or-dir)
  "Use completion to attach a FILE-OR-DIR to the current heading."
  (interactive (list (read-file-name "Attach: ")))
  (unless (eq major-mode 'org-mode)
    (error "You must be in an org file to use this command"))

  (when (org-before-first-heading-p)
    (error "You must be in a heading to attach files"))

  (if (file-directory-p file-or-dir)
      (scimax-org-attach-attach-directory-mv file-or-dir)
    (org-attach-attach file-or-dir)))

(defhydra scimax-org-attach (:color blue)
  "Org attach"
  ("a" scimax-org-attach "Attach file or dir")
  ("d" org-attach-dired-to-subtree "Attach from dired")
  ("f" scimax-mac-attach-from-finder "Attach from Finder")
  ("o" org-attach-open "Open an attachment")
  ("O" (org-attach-open t) "Open attachment in Emacs")
  ("r" org-attach-reveal "Open attachment folder")
  ("z" org-attach-sync "Sync attachments")
  ("q" nil "Abort"))

(define-key org-mode-map (kbd "C-c C-a") 'scimax-org-attach/body)
(define-key dired-mode-map (kbd "C-c C-a") 'scimax-org-attach/body)

(provide 'scimax-org-attachments)

;;; scimax-org-attachments.el ends here
