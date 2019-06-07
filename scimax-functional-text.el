;;; scimax-functional-text.el --- Functional text for scimax

;;; Commentary:
;; This library makes functional links out of text in Emacs. It is similar to
;; the implicit links in hyperbole. These are similar to, but different than
;; org-links. The biggest difference is there is no export for these links, they
;; appear as is. They work in any kind of file, e.g. in emails, code comments,
;; log files, etc.
;;
;; The implicit links defined here should require minimal or no markup to
;; recognize, and the consequences of false matches should be pretty minimal.
;; The text should read naturally.
;;
;; In org-mode, I prefer to use links, since you can control how they export,
;; and they are more powerful and easier to configure IMO. Here I use
;; `button-lock-mode' instead of font-lock directly. This limits you to regexps
;; for implicit links. Font-lock would be more powerful, e.g. using functions
;; for finding links which would let you do validation if desired, but font-lock
;; is more complex, and validation might be slower.

;; Here are some examples of functional text.
;; Email addresses: johnrkitchin@gmail.com
;; Hashtags: #MeToo
;; @Usernames
;;
;; Github issues: issue #153
;; git commits:  commit 05fcea6
;; pull requests: pr #146  pull #146  or pull request #146

(require 'thingatpt)
(require 'button-lock)
;;; Code:

(global-button-lock-mode)

(defmacro scimax-functional-text (regexp action &rest plist)
  "A button-lock button maker.
REGEXP is the regular expression to match. It may have subgroups
in it that are accessible in the ACTION. ACTION is either a list
of sexps that are the body of a function, a lambda function, or a
quoted symbol of a function. You can access the regexp subgroups
in this function. PLIST is the rest of the arguments for
`button-lock-set-button'. I like <return> to be active, so it is
set by default. If you have a help-echo function it is also
wrapped so that it too can have access to the match-data.
"
  (when (and (listp action) (functionp (cadr action)))
    (setq action `((funcall-interactively ,action))))
  ;; wrap the help-echo so match-data is available
  (when (and (plist-get plist :help-echo)
	     (functionp (plist-get plist :help-echo)))
    (setq plist (plist-put plist :help-echo
			   `(lambda (win buf pt)
			      (save-excursion
				;; This is clunky, but with grouping the
				;; properties may not extend to the whole
				;; regexp.
				(goto-char (previous-single-property-change pt
									    'button-lock))
				;; now make sure we see it again to get the match-data 
				(save-match-data
				  (while (not (looking-at ,regexp))
				    (backward-char)) 
				  (funcall ,(plist-get plist :help-echo) win buf pt)))))))
  `(button-lock-register-global-button
    ,regexp
    ;; Suggested in https://github.com/rolandwalker/button-lock/issues/10
    (lambda (event)
      (interactive "e")
      (let ((click-pos (posn-point (event-end event))))
	(save-mark-and-excursion
	 (save-match-data
	   ;; This is clunky, but with grouping the properties may not extend to
	   ;; the whole regexp, e.g. if you set properties on a subgroup.
	   (goto-char (previous-single-property-change click-pos
						       'button-lock))
	   ;; now make sure we see it again to get the match-data
	   (while (not (looking-at ,regexp))
	     (backward-char))
	   ,@action))))
    ;; I like to press return on functional text to activate it.
    :keyboard-binding "RET"
    ;; These are the rest of the properties
    ,@plist))


(defun scimax-flyspell-ignore-buttons (orig-fun &rest args)
  "Ignore flyspell on buttons.
The overlays hijack mouse clicks and functions. This gives
button-lock precedence in org-mode. It is used as advice on
`org-mode-flyspell-verify'."
  (and (apply orig-fun args)
       (not (get-text-property (point) 'button-lock))))

(advice-add 'org-mode-flyspell-verify :around 'scimax-flyspell-ignore-buttons)

;; * Email addresses
;; johnrkitchin@gmail.com

(defhydra mail-address (:color blue :hint nil)
  "
mail address
_c_: Contacts _m_: Mail
"
  ("m" (when-let (email (thing-at-point 'email)) (compose-mail email)))
  ("c" (let ((ivy-initial-inputs-alist `((ivy-contacts . ,(thing-at-point 'email)))))
	 (ivy-contacts))))

(defun sf-activate-emails ()
  (interactive)
  (scimax-functional-text
   thing-at-point-email-regexp
   'mail-address/body
   :face (list 'link)
   :help-echo "Click me to send a message from emacs"))

;; * Username handles
;;
;; These may have many contexts, e.g. Twitter, Github, etc. The action on these
;;  is to launch a hydra menu to pick which action you want.
;;
;; @johnkitchin   (twitter)
;; @jkitchin   (github, reddit)
;; @hematravels (instagram)
;; @tengnie (LinkedIn)
;; @nakhan1 (Facebook)
;; Some username handles are trickier. They can contain punctuation and dashes.
;; The regex is too weak for these. These include Facebook, LinkedIn, and
;; others.

(defvar @username-handle-regexp "\\(^\\|[[:space:]]\\|\\s(\\)\\(?2:@\\(?1:[[:alnum:]]+\\)\\)"
  "Regexp for a username handle.
It looks like @username preceded by a space, an opening bracket
The handle is in group 1.
These are defined by @username. This pattern will not match
usernames with punctuation in them, this is partly by design to
avoid matching emails too.")

(defun @username-open (url-pattern)
  "If point is at an @username, open it in URL-PATTERN.
URL-PATTERN should have one %s in it which is replaced by username."
  (interactive)
  (browse-url (format url-pattern (match-string 1))))


(defhydra @username (:color blue :hint nil)
  "
Open a @username
_b_: Bitbucket  _f_: Facebook
_g_: GitHUB     _i_: Instagram
_G_: GitLab     _l_: LinkedIn _r_: reddit  _t_: Twitter
"
  ("b" (@username-open "https://bitbucket.org/%s/"))
  ("f" (@username-open "https://www.facebook.com/%s"))
  ("g" (@username-open "https://github.com/%s"))
  ("G" (@username-open "https://gitlab.com/%s"))
  ("i" (@username-open "https://www.instagram.com/%s/"))
  ("l" (@username-open "https://www.linkedin.com/in/%s/"))
  ("r" (@username-open "https://www.reddit.com/user/%s/"))
  ("t" (@username-open "https://twitter.com/%s")))

(defun sf-activate-usernames ()
  (interactive)
  (scimax-functional-text
   @username-handle-regexp
   '@username/body
   :grouping 2
   :face (list 'link)
   :help-echo "Click me to open username."))


;; * Hashtags
;; These are defined like #MeToo (Twitter, Facebook)
;; #cabo  (Instagram)
;; #book (org-mode)
;; They also could have different contexts, maybe Twitter, maybe Instagram, or
;; tags in org-mode, etc. so we also define a hydra for this.

(defvar hashtag-regexp "\\(^\\|[[:space:]]\\|\\s(\\)\\(?2:#\\(?1:[[:alnum:]]+\\)\\)"
  "A regexp for a hashtag.
The hashtag is in group 1.")

(defun hashtag-open (url-pattern)
  "If point is at a hashtag, open it in URL-PATTERN.
URL-PATTERN should have one %s in it which is replaced by the hashtag."
  (interactive)
  (browse-url (format url-pattern (match-string 1))))

(defhydra hashtag (:color blue :hint nil)
  "
@hashtag
_f_: Facebook _i_: Instagram  _o_: org-tag  _t_: Twitter"
  ("i" (hashtag-open "https://www.instagram.com/explore/tags/%s/"))
  ("f" (hashtag-open "https://www.facebook.com/hashtag/%s"))
  ("o" (org-tags-view nil (hashtag-at-p)))
  ("t" (hashtag-open "https://twitter.com/hashtag/%s")))

(defun sf-activate-hashtags ()
  (interactive)
  (scimax-functional-text
   hashtag-regexp
   'hashtag/body
   :grouping 2
   :face (list 'link)
   :help-echo "Click me to open the hashtag."))

;; * Github
;; ** Github issues
;; When the link in a git repo, make it open the issue.
;; issue #153 -> https://github.com/jkitchin/scimax/issues/153
(defvar github-issue-regexp "issue\\s-+#\\(?1:[0-9]+\\)"
  "Regexp for a github issue.")

(defhydra github-issue (:color blue :hint nil)
  "
git issue
_g_: Github"
  ("g" (lambda ()
	 (interactive)
	 (when-let (url (github-issue-at-p))
	   (browse-url url)))))

(defun github-issue-at-p ()
  "Return url to the issue if we are at one."
  (save-excursion
    (let* ((project-name
	    ;; assume something like: git@github.com:jkitchin/scimax.git
	    (substring
	     (second
	      (s-split
	       ":"
	       (s-trim (shell-command-to-string "git remote get-url origin"))))
	     nil -4))
	   (issue (match-string-no-properties 1))
	   (url (format "https://github.com/%s/issues/%s"
			project-name issue)))
      url)))

(defun sf-activate-github-issues ()
  (interactive)
  (scimax-functional-text
   github-issue-regexp
   'github-issue/body
   :face (list 'link)
   :help-echo "Click me to open issue at Github."))

;; ** Github pull requests
;; pull request #146
;; pull #146
;; pr #146

(defvar pull-request-regexp "\\(?:pull request\\|pr\\|pull\\)\\s-+#\\(?1:[0-9]+\\)"
  "Regexp for a pull request.")

(defun pull-request-at-p ()
  "Return url to pull request."
  (save-excursion
    (let* ((project-name
	    ;; assume something like: git@github.com:jkitchin/scimax.git
	    (substring
	     (second
	      (s-split
	       ":"
	       (s-trim (shell-command-to-string "git remote get-url origin"))))
	     nil -4))
	   (pull-request (match-string-no-properties 1))
	   (url (format "https://github.com/%s/pull/%s"
			project-name pull-request)))
      url)))

(defhydra github-pull-request (:color blue :hint nil)
  "
git pull-request
_g_: Github"
  ("g" (lambda ()
	 (interactive)
	 (when-let (url (pull-request-at-p))
	   (browse-url url)))))

(defun sf-activate-github-pull-requests ()
  (interactive)
  (scimax-functional-text
   pull-request-regexp
   'github-pull-request/body
   :face (list 'link)
   :help-echo "Click me to open pull request at Github."))

;; ** Github commits
;; Open a commit in a browser.
;; commit 15b8adf1 -> https://github.com/jkitchin/scimax/commit/15b8adf191a252bb6a4c16c327f9c6fccc315a73
;; commit 05fcea6
(defvar github-commit-regexp "\\b\\(commit\\s-+\\)\\([0-9a-z]\\{6,40\\}\\)\\b"
  "Regexp for a github commit.")

(defun github-commit-at-p ()
  "Return (project-name hash full-hash)."
  (save-excursion
    (let* ((project-name
	    ;; assume something like: git@github.com:jkitchin/scimax.git
	    (substring
	     (second
	      (s-split
	       ":"
	       (s-trim (shell-command-to-string "git remote get-url origin"))))
	     nil -4))
	   (hash (match-string-no-properties 2))
	   (full-hash (s-trim (shell-command-to-string (format "git rev-parse %s" hash)))))
      (when hash
	(list project-name hash full-hash)))))

(defhydra git-commit (:color blue :hint nil)
  "
commit
_g_: Github _m_: Magit log
^ ^         _c_: Magit commit"
  ("c" (magit-show-commit (third (github-commit-at-p))))
  ("g" (when-let ((data (github-commit-at-p))
		  (url (format "https://github.com/%s/commit/%s"
			       (first data) (third full-hash))))
	 (browse-url url)))
  ("m" (magit-log (list (third (github-commit-at-p))))))

(defun sf-activate-git-commits ()
  (interactive)
  (scimax-functional-text
   github-commit-regexp
   'git-commit/body
   :face (list 'link)
   :help-echo "Click me to open the commit on Github."))

(provide 'scimax-functional-text)

;;; scimax-functional-links.el ends here
