;;; scimax-notebook.el ---    -*- lexical-binding: t -*-

;;; Commentary:
;; This is an experiment in using scimax and org-mode for scientific notebook
;; purposes. The idea is you have a "project" that is a set of org and other
;; files under version control (git). There is a "master" file that is the
;; starting point, e.g. the README.org file. You can use `projectile' to switch
;; between projects easily, or search/find files within a project.
;;
;; `nb-new' is command to create a new project, it is just a thin wrapper that
;; creates the directories, registers them with projectile, and opens the master
;; file.
;;
;; `nb-open' is a command to open an existing project. It is a thin wrapper
;; around the projectile-switch-project command that opens the master file.
;;
;; `nb-agenda' to see the TODO items within a project, or do other org-agenda
;; things within the scope of the project, e.g. search by tag/property.
;;
;; `nb-archive' creates a zip-archive of the project.
;;
;; Note there is a projectile hydra defined: `hydra-projectile/body' that may be
;; useful for scimax-notebooks.

;;; Code:
;; * Setup
(projectile-mode +1)

(use-package ggtags)
(use-package ibuffer-projectile)
(when (executable-find "ag")
  (use-package ag))


(defcustom nb-notebook-directory
  "~/vc/projects/"
  "Directory where projects are stored."
  :group 'scimax-notebook
  :type '(directory))


(unless (file-directory-p nb-notebook-directory)
  (make-directory nb-notebook-directory t))


(defcustom nb-master-file (lambda (&optional name)
			    "Return the master file name for the project."
			    "README.org")
  "A function that returns the master file in each project.
The function must take one optional argument that is a project
name. This function will be run in the root directory of the
project. The function should return a string of the master file
name. See `nb-example-master' for an example of a computed maste
file."
  :group 'scimax-notebook)


(defun nb-example-master (&optional name)
  "Return the master filename for the project of NAME.
NAME is optional, and if it is nil, compute the filename from the
current directory. This master file is an org-file with the name
of the root directory, with a @ prefix so it sorts to the top of
the directory with ls."
  (concat "@"
	  (file-name-base (directory-file-name default-directory))
	  ".org")
  :group 'scimax-notebook)


(defcustom nb-scimax-update-menu-p nil
  "If non-nil, add a hook that updates the menu with known projects.
This can be slow if you have a lot of projects because it runs
when in a menu bar update hook. If nil, just add projects once."
  :group 'scimax-notebook)


(defcustom nb-switch-project-action
  (lambda ()
    (find-file (read-file-name "File: " "." (funcall nb-master-file))))
  "Function to run after switching projects with `nb-open'."
  :group 'scimax-notebook)


;;;###autoload
(defun nb-open ()
  "Switch to a project and open the main file.
This is a thin wrapper on `projectile-switch-project' that opens the master file."
  (interactive)
  (let ((projectile-switch-project-action nb-switch-project-action))
    (projectile-switch-project)))


;;;###autoload
(defun nb-git-clone (url path)
  "Clone a git repo at URL as a project at PATH in `nb-notebook-directory'."
  (interactive "sUrl: \nsPath: ")
  (let ((default-directory nb-notebook-directory))
    (when (file-exists-p path)
      (error "%S already exists" path))
    (make-directory path t)
    (shell-command-to-string (format "git clone %s %s" url path))
    (dired path)))


;;;###autoload
(defun nb-clone ()
  "Create a clone (by a recursive copy) of the current notebook.
This is helpful when you want to keep a copy of the repo, for
example."
  (interactive)
  (let* ((project-root (projectile-project-root))
	 (dir-one-up (file-name-directory (directory-file-name project-root)))
	 (name (file-name-base (directory-file-name project-root)))
	 (clone-base-name (read-directory-name
			   "Clone name: "
			   dir-one-up  nil nil
			   (concat name "-clone"))))
    (let ((default-directory dir-one-up))
      (shell-command (format "cp -R %s %s" name clone-base-name))
      (projectile-add-known-project clone-base-name)
      (projectile-save-known-projects)
      (projectile-switch-project-by-name clone-base-name))))


;;;###autoload
(defun nb-new (name)
  "Create a new project of NAME in `nb-notebook-directory'."
  (interactive (list (read-directory-name "New project name: " nb-notebook-directory)))
  (when (file-directory-p name)
    (user-error "%s already exists." name))
  (let ((dir (file-name-as-directory (expand-file-name name nb-notebook-directory)))
	(nb-master-file-name (funcall nb-master-file name)))
    (unless (file-directory-p dir)
      (make-directory dir t)
      (let ((default-directory dir))
	(shell-command "git init")))
    (projectile-add-known-project dir)
    (projectile-save-known-projects)
    (find-file (expand-file-name nb-master-file-name dir))))


;; * Notebook agenda
;;;###autoload
(defun nb-agenda ()
  "Show org-agenda for org-files in the notebook."
  (interactive)
  (let ((org-agenda-files (mapcar
			   (lambda (f) (expand-file-name
					f (projectile-project-root)))
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
	    "Your notebook contains uncommitted changes or files:\n%s\n Continue? " output))
	(shell-command
	 (format
	  "git archive --format zip HEAD -o \"%s-%s.zip\""
	  (f-join (projectile-project-root)
		  (car (last (f-split (projectile-project-root)))))
	  (format-time-string "%Y-%m-%d-%H:%M%p")))))))



;; * List tags in the notebook
(defun nb-list-tags ()
  "Get a list of tags in the notebook."
  (interactive)
  (let ((tags '())
	(already-open nil)
	(org-files (mapcar
		    (lambda (f) (expand-file-name
				 f (projectile-project-root)))
		    (-filter (lambda (f)
			       (and
				(f-ext? f "org")
				(not (s-contains? "#" f))))
			     (projectile-current-project-files)))))
    (cl-loop for org-file in org-files do
	     (setq already-open (find-buffer-visiting org-file))
	     (with-current-buffer (find-file-noselect org-file)
	       (save-excursion
		 (save-restriction
		   (widen)
		   (goto-char (point-min))
		   (while (re-search-forward org-heading-regexp nil t)
		     (setq tags (append tags (org-get-tags)))))))
	     (unless already-open
	       (y-or-n-p (format "kill %s" already-open))
	       (kill-buffer already-open)
	       (setq already-open nil)))
    (-uniq tags)))


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
   "words"))

(if nb-scimax-update-menu-p
    (add-hook 'menu-bar-update-hook 'update-scimax-projects-menu)
  (update-scimax-projects-menu))


;; * The end

(provide 'scimax-notebook)

;;; scimax-notebook.el ends here
