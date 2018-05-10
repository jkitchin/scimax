;;; scimax-yas.el --- scimax setup for snippets

;;; Commentary:
;;

(require 'yasnippet)

(setq helm-yas-space-match-any-greedy t)

(defcustom scimax-snippet-dir (expand-file-name "snippets" scimax-dir)
  "Directory to store snippets in.")

(unless (file-directory-p scimax-snippet-dir)
  (make-directory scimax-snippet-dir t))

(add-to-list 'yas-snippet-dirs scimax-snippet-dir)
(yas-global-mode 1)

(defun scimax-ivy-yas ()
  "Interactively insert a snippet.
Adapated from helm-c-yasnippet."
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
	      :action (lambda (candidate)
			(let ((yas--current-template (cdr candidate))
			      (where (if (region-active-p)
					 (cons (region-beginning) (region-end))
				       (cons (point) (point)))))
			  (yas-expand-snippet (yas--template-content yas--current-template)
					      (car where)
					      (cdr where)
					      (yas--template-expand-env yas--current-template)))))))


(provide 'scimax-yas)

;;; scimax-yas.el ends here
