;;; scimax-dashboard.el --- Dashboard for scimax

;;; Commentary:
;; This module creates a dashboard splash screen for scimax.  It
;; builds off of https://github.com/rakanalh/emacs-dashboard to
;; provide a starting point for you when you first open emacs.
;;
;; `scimax-dashboard' will open the dashboard any time.
;;
;; Most sections are customized to be more functional than default.
;;
;; These key bindings are different than those provided by `dashboard':
;; a - jump to any button and open it
;; i - previous line
;; s-i  previous section
;; k - next line
;; s-k  next section
;; o - open the button at point
;; ? - show the keymap

;;; Code:

(require 'cl)
(require 'avy)
(require 'counsel)
(require 'dashboard)

(defcustom scimax-dashboard-check-git-updates t
  "When non-nil check for updates."
  :group 'scimax-dashboard
  :type 'boolean)


(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :bind (:map dashboard-mode-map
	      ("a" . scimax-dashboard-jump-to-button)
	      ("i" . previous-line)
	      ("k" . forward-line)
	      ("s-i" . dashboard-previous-section)
	      ("j" . dashboard-previous-section)
	      ("s-k" . dashboard-next-section)
	      ("l" . dashboard-next-section)
	      ("o" . widget-button-click)
	      ("?" . scimax-dashboard-describe-keys))
  :config
  (setq dashboard-banner-logo-title "Awesome editing for scientists and engineers")
  (setq dashboard-startup-banner (expand-file-name "scimax.png" scimax-dir))
  (setq dashboard-items '((scimax . t)
			  (scimax-agenda . 5)
			  (scimax-recentf  . 5)
			  (scimax-bookmarks . 5)
			  (scimax-projects . 5)))
  (dashboard-setup-startup-hook))


(defun scimax-dashboard-describe-keys ()
  "Show the keybindings in the dashboard."
  (interactive)
  (describe-keymap dashboard-mode-map))


(defun scimax-dashboard ()
  "Open the scimax dashboard."
  (interactive)
  (dashboard-insert-startupify-lists)
  (dashboard-refresh-buffer))


(defun scimax-dashboard-jump-to-button ()
  "Jump to a link in the dashboard with avy."
  (interactive)
  (let ((marks '())
	(ovs (overlay-lists)))
    (with-current-buffer "*dashboard*"
      (cl-loop for ov in (append (car ovs) (cdr ovs))
	       do
	       (when (overlay-get ov 'button)
		 (push (ov-beg ov) marks))))
    (avy-with scimax-dashboard-link
      (avy--process
       (reverse marks)
       (avy--style-fn avy-style)))))


(defun scimax-internet-p ()
  "Use ping to see if the internet is available."
  (interactive)
  (let* ((exe (executable-find "ping"))
	 (cmd (format "%s github.com"
		      (cond
		       ((string= system-type "windows-nt")
			"ping -n 1 ")
		       ((string= system-type "darwin")
			"ping -c 1 ")
		       (t "ping -c 1")))))
    (if exe
	(if (= 0 (shell-command cmd))
	    t
	  (message-box "%s failed. Check your internet connection." cmd))
      (message "You have no ping executable! I cannot check for internet connectivity.")
      nil)))

;; * scimax section

(defun scimax-dashboard-scimax-section (&rest args)
  "Create the scimax dashboard section."
  (dashboard-insert-heading "scimax")
  (insert "\n    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (scimax-help))
		 :mouse-face 'highlight
		 :help-echo (substitute-command-keys
			     (format "\\[%s]\n"
				     "scimax-help"))
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "scimax help")

  (insert "    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (scimax/body))
		 :mouse-face 'highlight
		 :help-echo (substitute-command-keys
			     (format "\\[%s]\n"
				     "scimax/body"))
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "scimax menu")

  (insert "    ")
  (widget-create
   'push-button
   :action `(lambda (&rest ignore)
	      (browse-url "https://github.com/jkitchin/scimax"))
   :mouse-face 'highlight
   :help-echo "Click to open in browser."
   :follow-link "\C-m"
   :button-prefix ""
   :button-suffix ""
   :format "%[%t%]"
   "scimax (Github)")

  (insert "    ")
  (widget-create
   'push-button
   :action `(lambda (&rest ignore)
	      (scimax-customize-user))
   :mouse-face 'highlight
   :help-echo "Click to open user.el"
   :follow-link "\C-m"
   :button-prefix ""
   :button-suffix ""
   :format "%[%t%]"
   "Customize user.el")

  (insert "    ")
  (widget-create
   'push-button
   :action `(lambda (&rest ignore)
	      (customize-apropos "scimax"))
   :mouse-face 'highlight
   :help-echo "Click to customize scimax related variables."
   :follow-link "\C-m"
   :button-prefix ""
   :button-suffix ""
   :format "%[%t%]"
   "Customize scimax")

  ;; Check if there are updates available
  (when (and scimax-dashboard-check-git-updates (scimax-internet-p))
    (let ((branch (s-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
	  (incoming (string-to-number
		     (s-trim (shell-command-to-string
			      "git rev-list master...origin/master --count")))))
      (when (> incoming 0)
	(insert "  ")
	(widget-create
	 'push-button
	 :action `(lambda (&rest ignore)
		    (scimax-update))
	 :button-face '(:foreground "red" :weight bold :underline t)
	 :help-echo `(format "%s updates are available" ,incoming)
	 :mouse-face 'highlight
	 :follow-link "\C-m"
	 :button-prefix ""
	 :button-suffix ""
	 :format "%[%t%]"
	 "  update scimax")))))

(add-to-list 'dashboard-item-generators  '(scimax . scimax-dashboard-scimax-section))

;; * Agenda

(defun scimax-dashboard-agenda (&rest args)
  "Create our version of recent"
  (dashboard-insert-agenda (cdr (assoc 'scimax-agenda dashboard-items)))
  (insert "\n    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (org-agenda nil "a"))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Agenda"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "Open agenda")
  (insert " ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (org-agenda nil "t"))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Agenda"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "TODOs")

  (insert " Search by: ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (org-agenda nil "m"))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Search by tag/property"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "tag/prop")
  (insert " ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (org-agenda nil "s"))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Search for keywords"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "keyword")
  (insert " ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (org-agenda nil "/"))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "moccur"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "moccur"))

(add-to-list 'dashboard-item-generators  '(scimax-agenda . scimax-dashboard-agenda))

;; * recentf
(defun scimax-dashboard-recentf (&rest args)
  "Create our version of recent"
  (dashboard-insert-recents (cdr (assoc 'scimax-recentf dashboard-items)))
  (insert "\n    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (counsel-recentf))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "counsel-recentf"
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "Open another recent file"))

(add-to-list 'dashboard-item-generators  '(scimax-recentf . scimax-dashboard-recentf))

;; * bookmarks
(defun scimax-dashboard-bookmarks (&rest args)
  "Create our version of bookmarks"
  (dashboard-insert-bookmarks (cdr (assoc 'scimax-bookmarks dashboard-items)))
  (insert "\n    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (counsel-bookmark))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Open another bookmark."
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "Open another bookmark."))

(add-to-list 'dashboard-item-generators  '(scimax-bookmarks . scimax-dashboard-bookmarks))

;; * projects
(defun scimax-dashboard-projects (&rest args)
  "Create our version of projects"
  (dashboard-insert-projects (cdr (assoc 'scimax-projects dashboard-items)))
  (insert "\n    ")
  (widget-create 'push-button
		 :action `(lambda (&rest ignore)
			    (projectile-switch-project))
		 :mouse-face 'highlight
		 :button-face '(:background "Lightgray" :underline t)
		 :help-echo "Open another project."
		 :follow-link "\C-m"
		 :button-prefix ""
		 :button-suffix ""
		 :format "%[%t%]"
		 "Open another project"))

(add-to-list 'dashboard-item-generators  '(scimax-projects . scimax-dashboard-projects))


(provide 'scimax-dashboard)

;;; scimax-dashboard.el ends here
