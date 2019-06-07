;;; scimax-yas.el --- scimax setup for snippets

;;; Commentary:
;; This module provides a scimax customized setup for yasnippets. It was
;; motivated by some recent discussions on the org-mode mailing list about
;; replacing the <template mechanism with something new. I have been meaning to
;; figure out how to use snippets, and this module provides a more useful way
;; (imho) to use them. Notably, you can use `scimax-ivy-yas' to insert a snippet
;; using completion.
;;
;; The variable `scimax-snippet-dir' defines the directory where scimax snippets
;; are stored.

(require 'yasnippet)

(defcustom scimax-snippet-dir (expand-file-name "snippets" scimax-dir)
  "Directory to store snippets in.")

(unless (file-directory-p scimax-snippet-dir)
  (make-directory scimax-snippet-dir t))

(add-to-list 'yas-snippet-dirs scimax-snippet-dir)
(yas-global-mode 1)

(defun scimax-ivy-yas ()
  "Interactively insert a snippet.
Adapted from helm-c-yasnippet.
`ivy-yasnippet' might be a better function."
  (interactive)
  (let ((candidates (mapcar
		     (lambda (template)
		       (cons
			(format "%15s %s %s"
				(yas--template-key template)
				(yas--template-name template)
				(yas--template-get-file template))
			template))
		     (yas--all-templates (yas--get-snippet-tables)))))
    (ivy-read "insert: " candidates
	      :action '(1
			("o"
			 (lambda (candidate)
			   (let ((yas--current-template (cdr candidate))
				 (where (if (region-active-p)
					    (cons (region-beginning) (region-end))
					  (cons (point) (point)))))
			     (yas-expand-snippet (yas--template-content yas--current-template)
						 (car where)
						 (cdr where)
						 (yas--template-expand-env yas--current-template))))
			 "Expand selected template")
			
			("v"
			 (lambda (candidate)
			   (find-file (yas--template-get-file (cdr candidate))))
			 "Visit template file")
			
			("d"
			 (lambda (candidate)
			   (when (y-or-n-p "Delete?")
			     (delete-file (yas--template-get-file (cdr candidate)))
			     (yas-reload-all)))
			 "Delete snippet")
			
			("n"
			 (lambda (candidate)
			   (yas-new-snippet))
			 "new template")))))



(provide 'scimax-yas)

;;; scimax-yas.el ends here
