;;; scimax-notebook.el ---    -*- lexical-binding: t -*-
;;; Header:

;;; Commentary:

;;; Code:
;; * Setup

(defvar nb-notebook-directory
  "~/vc/projects/"
  "Directory where projects are stored.")

(unless (file-directory-p nb-notebook-directory)
  (make-directory nb-notebook-directory t))

(defvar nb-master-file "README.org"
  "Name of the master file in each project")

;;;###autoload
(defun nb-open ()
  "Switch to a project and open the main file."
  (interactive)
  (let ((projectile-switch-project-action (lambda ()
					    (find-file nb-master-file)))) 
    (projectile-switch-project)))

;;;###autoload
(defun nb-new (name)
  "Create a new project of NAME in `nb-notebook-directory'."
  (interactive "sNew project name: ")
  (let ((dir (file-name-as-directory (expand-file-name name nb-notebook-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (let ((default-directory dir))
	(shell-command "git init")))
    (find-file (expand-file-name nb-master-file dir))
    (projectile-mode)))



;; * Notebook agenda
;;;###autoload
(defun nb-agenda ()
  "Show org-agenda for org-files in the notebook."
  (interactive)
  (let ((org-agenda-files (mapcar
			   (lambda (f) (expand-file-name f (projectile-project-root)))
			   (-filter (lambda (f)
				      (and 
				       (f-ext? f "org")
				       (not (s-contains? "#" f))))
				    (projectile-current-project-files)))))
    (org-agenda)))


;; * Add a menu to scimax


(easy-menu-change
 '("Scimax") "notebook"
 `(["New notebook" nb-new t]
   ["Open notebook" nb-open t]
   ("Projects"))
 "words")

(defun update-scimax-projects-menu ()
  (easy-menu-change
   '("Scimax" "notebook") "Projects" 
   (sort (mapcar (lambda (x)
		   (vector
		    ;; entry
		    (file-name-nondirectory (substring x 0 -1))
		    ;; action
		    `(lambda ()
		       (interactive)
		       (projectile-switch-project-by-name
			,x))
		    ;; visibility
		    t))
		 projectile-known-projects)
	 (lambda (a b) (string< (elt a 0) (elt b 0))))
   "words"))

(add-hook 'menu-bar-update-hook 'update-scimax-projects-menu)





;; * The end

(provide 'scimax-notebook)

;;; scimax-notebook.el ends here
