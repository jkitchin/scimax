;;; scimax-notebook.el ---    -*- lexical-binding: t -*-
;;; Header:

;;; Commentary:
;; This is an experiment in using scimax and org-mode for scientific notebook
;; purposes. The idea is you have a "project" that is a set of org and other
;; files under version control (git). You can use `projectile' to switch between
;; projects easily, or search/find files within a project.
;;
;; This code sets up a notebook agenda command `nb-agenda` to see the TODO items
;; within a project, or do other org-agenda things within the scope of the
;; project, e.g. search by tag/property.

;;; Code:
;; * Setup
(projectile-mode +1)


(defvar nb-notebook-directory
  "~/vc/projects/"
  "Directory where projects are stored.")


(unless (file-directory-p nb-notebook-directory)
  (make-directory nb-notebook-directory t))


(defvar nb-master-file "README.org"
  "Name of the master file in each project.")


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
    (projectile-add-known-project dir)
    (projectile-save-known-projects)
    (find-file (expand-file-name nb-master-file dir))))



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

;; * Archive a notebook
;;;###autoload
(defun nb-archive ()
  "Create an archive of the current notebook.
This uses git archive to create an archive of the current state
of the notebook. The zip file will be timestamped in the root
project directory. If your repo contains untracked files or
uncommitted changes, you will be prompted to continue."
  (let ((output (shell-command-to-string "git status --porcelain")))
    (unless (string= "" output)
      (when
	  (y-or-n-p
	   (format
	    "Your notebook contains uncommitted changes or files:\n%s\n Continue?" output))
	(shell-command
	 (format
	  "git archive --format zip HEAD -o \"%s-%s.zip\""
	  (f-join (projectile-project-root)
		  (car (last (f-split (projectile-project-root)))))
	  (format-time-string "%Y-%m-%d-%H:%M%p")))))))

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
   (mapcar (lambda (x)
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
   ;; (sort (mapcar (lambda (x)
   ;; 		   (vector
   ;; 		    ;; entry
   ;; 		    (file-name-nondirectory (substring x 0 -1))
   ;; 		    ;; action
   ;; 		    `(lambda ()
   ;; 		       (interactive)
   ;; 		       (projectile-switch-project-by-name
   ;; 			,x))
   ;; 		    ;; visibility
   ;; 		    t))
   ;; 		 projectile-known-projects)
   ;; 	 (lambda (a b) (string< (elt a 0) (elt b 0))))
   "words"))

(add-hook 'menu-bar-update-hook 'update-scimax-projects-menu)





;; * The end

(provide 'scimax-notebook)

;;; scimax-notebook.el ends here
