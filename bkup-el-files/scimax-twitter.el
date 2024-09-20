;;; scimax-twitter.el --- Twitter functions

;;; Commentary:
;;
;; Install the commandline twitter from https://github.com/sferik/t
;; There was a bug described in https://github.com/sferik/t/issues/395
;; I installed an older version like this (uses Ruby).
;; gem install t -v 2.10
;;
;; Then run: t authorize
;;
;; to setup authorization for your twitter account.
;;
;; Send a tweet from a program: `scimax-twitter-update'.
;; Reply to a tweet from a program: `scimax-twitter-reply'
;;
;; Tweet from an org headline: `scimax-twitter-tweet-headline'
;;
;; `scimax-twitter-ivy' an interface to your followers and followees.
;;
;; Tweet a subtree as a thread: `scimax-twitter-org-subtree-tweet-thread'
;; TODO: check for lengths before trying to send.
;;
(require 'f)
(require 'scimax-functional-text)
(use-package gist)

;; * Hashtag functional text
(defface scimax-twitter-hashtag-face
  `((t (:inherit org-link
		 :weight bold)))
  "Color for twitter hashtags."
  :group 'scimax-twitter)

(scimax-functional-text
 "\\(^\\|[[:punct:]]\\|[[:space:]]\\)\\(?2:#\\(?1:[[:alnum:]]+\\)\\)"
 (lambda ()
   (browse-url (format "https://twitter.com/hashtag/%s" (match-string 1))))
 :grouping 2
 :face (list 'scimax-twitter-hashtag-face)
 :help-echo "Click me to open hashtag.")


;; * Twitter handles
(defface scimax-twitter-handle-face
  `((t (:inherit org-link
                 :foreground "DarkOrange1"
		 :weight bold)))
  "Color for twitter handles."
  :group 'scimax-twitter)

(scimax-functional-text
 "\\(^\\|[[:punct:]]\\|[[:space:]]\\)\\(?2:@\\(?1:[[:alnum:]]+\\)\\)"
 'scimax-twitter-handle-hydra/body
 :grouping 2
 :face (list 'scimax-twitter-handle-face)
 :help-echo "Click me to open username.")

(defhydra scimax-twitter-handle-hydra (:color blue)
  "
_k_: copy  _o_: open"
  ("k" (lambda () (kill-new (match-string 1))))
  ("o" (lambda ()
	 (browse-url (format "https://twitter.com/%s" (match-string 1)))))
  ;; TODO:
  ;; ("f" follow)
  )


;; * Twitter usernames

(defcustom scimax-twitter-directory "~/.scimax-twitter/"
  "Directory to cache scimax-twitter data."
  :group 'scimax
  :type 'directory)


(unless (f-dir? scimax-twitter-directory)
  (make-directory scimax-twitter-directory t))


(defun scimax-twitter-download-whois-info (&optional reload)
  "Download info on who you follow and followers."
  (interactive "P")

  (when (or reload (null scimax-twitter-usernames))
    (setq scimax-twitter-usernames (-uniq (append (process-lines "t" "followings")
						  (process-lines "t" "followers")
						  (process-lines "t" "followings")
						  (process-lines "t" "leaders")
						  (process-lines "t" "groupies")))))

  (unless (f-dir? (f-join scimax-twitter-directory "whois"))
    (make-directory (f-join scimax-twitter-directory "whois") t))

  ;; Here we download the files if necessary.
  (cl-loop for username in scimax-twitter-usernames
	   do
	   (let ((userfile (expand-file-name username
					     (f-join scimax-twitter-directory "whois"))))
	     (when (or reload (not (f-exists? userfile)))
	       (message "Getting user %s" username)
	       (with-temp-file userfile
		 (insert (shell-command-to-string
			  (format "t whois %s" username))))))))


(defvar scimax-twitter-usernames nil
  "List of usernames that either you follow or that follow you.")


(defvar scimax-twitter-ivy-candidates '()
  "List of candidate usernames for ivy.")


(defun scimax-twitter-ivy-candidates (&optional reload)
  "Returns a list of Twitter handle candidates."
  (interactive "P")
  (when (or reload (null scimax-twitter-usernames))
    (setq scimax-twitter-usernames (-uniq
				    (mapcar 'file-name-nondirectory
					    (f-entries
					     (f-join scimax-twitter-directory "whois"))))))
  (when (or reload (null scimax-twitter-ivy-candidates))
    (setq scimax-twitter-ivy-candidates
	  (loop for username in scimax-twitter-usernames
		collect
		(let* ((userfile (expand-file-name username
						   (f-join scimax-twitter-directory "whois")))
		       (info (when (f-exists? userfile)
			       (mapcar (lambda (line)
					 (cons (s-trim (substring line 0 13))
					       (substring line 13)))
				       (process-lines "cat" userfile)))))
		  (list (format "%20s | %20s | %40s | %s"
				(cdr (assoc "Screen name" info))
				(cdr (assoc "Name" info))
				(cdr (assoc "Bio" info))
				(cdr (assoc "URL" info)))
			info)))))
  ;; return the variable
  scimax-twitter-ivy-candidates)


(defun scimax-twitter-ivy (&optional reload)
  "Select from who you follow and your followers with ivy.
Default action is to insert the screen name, but you can also
open their twitter page or url."
  (interactive "P")
  (ivy-read "Username: " (scimax-twitter-ivy-candidates reload)
	    :action '(1
		      ("i" (lambda (cand)
			     (let ((info (cadr cand)))
			       (insert (cdr (assoc "Screen name" info)))))
		       "insert username")
		      ("d" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info)))
				    (msg (read-string "Msg: ")))
			       (message (s-join "\n" (process-lines "t" "dm" user msg)))))
		       "direct message")
		      ("f" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info))))
			       (message (s-join "\n" (process-lines "t" "follow" user)))))
		       "follow")
		      ;; list commands seem to be broken.
		      ;; /usr/local/bin/t: Sorry, that page does not exist.
		      ;; ("l" (lambda (cand)
		      ;; 	     (let* ((info (cadr cand))
		      ;; 		    (user (cdr (assoc "Screen name" info)))
		      ;; 		    (list (completing-read
		      ;; 			   "List: "
		      ;; 			   (process-lines "t" "lists"))))
		      ;; 	       (message (s-join "\n" (process-lines "t" "list" "add"
		      ;; 						    list user)))))
		      ;;  "add to list")
		      ("M" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info))))
			       (message (s-join "\n" (process-lines "t" "mute" user)))))
		       "Mute")
		      ;; TODO: update variable? remove whois entry?
		      ("U" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info))))
			       (message (s-join "\n" (process-lines "t" "unfollow" user)))))
		       "unfollow")
		      ("o" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info))))
			       (message (s-join "\n" (process-lines "t" "open" user)))))
		       "Open profile")
		      ("u" (lambda (cand)
			     (let ((info (cadr cand)))
			       (browse-url (cdr (assoc "URL" info)))))
		       "Open their url")
		      ("w" (lambda (cand)
			     (let* ((info (cadr cand))
				    (user (cdr (assoc "Screen name" info))))
			       (message (s-join "\n" (process-lines "t" "whois" user)))))
		       "whois"))))



;; * Tweet functions

(defun scimax-twitter-update (msg &optional file)
  "Post MSG as a tweet with an optional media FILE.
Returns the msgid for the posted tweet or the output from t."
  (interactive (list (read-string "Msg: ")
		     (read-file-name "File: ")))

  (unless (and file (f-ext? file "png"))
    (setq file nil))

  ;; This will convert org-entities to utf-8 chars
  (setq msg (org-export-string-as msg 'twitter t '(:ascii-charset utf-8)))

  (let* ((cmd `("t" "update" ,msg
		,@(when file '("-f"))
		,@(when file `(,file))))
	 (msg (message "\"%s\"" (s-join " " cmd)))
	 (output (apply 'process-lines cmd))
	 (last-line (car (last output))))
    (if (string-match "`t delete status \\([0-9]*\\)`" last-line)
	(prog1
	    (match-string-no-properties 1 last-line)
	  (org-entry-put nil "TWEETED_AT"
			 (format-time-string "[%Y-%m-%d %a %H:%M]")))
      ;; this probably means there was an error.
      output)))


(defun scimax-twitter-reply (msg msgid &optional file)
  "Reply MSG to tweet with MSGID and optional media FILE.
Returns the msgid for the posted tweet or the output from t."

  (setq msg (org-export-string-as msg 'twitter t '(:ascii-charset utf-8)))
  ;; Note this does not account for links/images which shorten the count.

  (let* ((cmds `("t" "reply" ,msgid ,msg
		 ,@(when file '("-f"))
		 ,@(when file `(,file))))
	 (s (message "%S" (s-join " " `,@cmds)))
	 (output (apply 'process-lines `("t" "reply" ,msgid ,msg
					 ,@(when file '("-f"))
					 ,@(when file `(,file)))))
	 (last-line (car (last output))))
    (if (string-match "`t delete status \\([0-9]*\\)`" last-line)
	(match-string-no-properties 1 last-line)
      last-line)))


(defmacro scimax-twitter-tweet-thread (&rest tweets)
  "Each tweet is a list of msg and optional file.
After the first tweet, each remaining tweet is a reply to the
last one."
  ;; Send the first one.
  (let (msgid
	(tweet (pop tweets)))
    (setq msgid (apply 'tweet (if (stringp tweet)
				  (list tweet)
				tweet)))
    ;; now send the rest of them
    (while tweets
      (setq tweet (pop tweets)
	    msgid (apply 'tweet-reply `(,msgid ,@(if (stringp tweet)
						     (list tweet)
						   tweet)))))))


(defmacro scimax-twitter-save-account (&rest body)
  "Execute body but save and restore the current account.
You need this for tweeting from multiple accounts."
  (let ((current-account (scimax-twitter-active-account)))
    `,@body
    (scimax-twitter-set-account current-account)))


;; * Twitter - org integration

(defun scimax-twitter-org-reply-p ()
  "For the current headline, determine if it is a reply to another tweet.
It is if the previous heading has TWITTER_MSGID property, and the
current headline is tagged as part of a tweet thread. Returns the
id to reply to if those conditions are met."
  (let ((tags (mapcar 'org-no-properties (org-get-tags))))
    (or (org-entry-get nil "TWITTER_IN_REPLY_TO")
	(and (-contains?  tags "tweet")
	     (-contains? tags "thread")
	     (save-excursion
	       (unless (looking-at org-heading-regexp)
		 (org-back-to-heading))
	       (org-previous-visible-heading 1)
	       ;; Make sure previous heading is part of the thread
	       (let ((tags (mapcar 'org-no-properties (org-get-tags))))
		 (and (-contains?  tags "tweet")
		      (-contains? tags "thread")
		      (org-entry-get nil "TWITTER_MSGID"))))))))


(defun scimax-twitter-org-tweet-components ()
  "Get the components required for tweeting a headline.
This is a list of (message reply-id file) where message is a
string that will be the tweet, reply-id is a string of the id to
reply to (it may be nil), and file is an optional media file to
attach to the tweet. If there are code blocks with a :gist in the
header, they will be uploaded as a gist, and the link added to
the msg."
  (let (msg
	reply-id
	file
	gists
	cp
	latex-frag
	next-heading)

    (save-excursion
      (unless (looking-at org-heading-regexp)
	(org-back-to-heading))
      (setq cp (point)
	    msg (nth 4 (org-heading-components))
	    reply-id (scimax-twitter-org-reply-p)))

    ;; check for files to attach
    (save-excursion
      (when (looking-at org-heading-regexp) (forward-char))
      (setq next-heading (re-search-forward org-heading-regexp nil t 1)))

    (save-restriction
      (narrow-to-region cp (or next-heading (point-max)))
      (setq file (car (org-element-map (org-element-parse-buffer) 'link
			(lambda (link)
			  (when
			      (and
			       (string= "file" (org-element-property :type link))
			       (f-ext? (org-element-property :path link) "png"))
			    (org-element-property :path link))))))
      ;; latex fragments overrule files.
      (setq latex-frag (car (org-element-map (org-element-parse-buffer)
				'(latex-environment latex-fragment) 'identity)))
      (when latex-frag
	(goto-char (org-element-property :begin latex-frag))
	;; Customized look for tweets.
	(let ((org-preview-latex-default-process 'imagemagick)
	      (org-latex-default-packages-alist org-latex-default-packages-alist)
	      (org-format-latex-options org-format-latex-options))
	  (add-to-list 'org-latex-default-packages-alist '("" "amsmath" t) t)
	  (add-to-list 'org-latex-default-packages-alist '("theorems, skins" "tcolorbox" t) t)
	  (add-to-list 'org-latex-default-packages-alist '("" "fourier" t) t)
	  (plist-put org-format-latex-options :latex-fragment-pre-body "\\mathversion{bold}\n")
	  (org-clear-latex-preview)
	  (org-latex-preview))
	(setq file (plist-get (cdr (overlay-get (ov-at) 'display)) :file)))

      ;; src-blocks
      (setq gists (org-element-map (org-element-parse-buffer) 'src-block
		    (lambda (src)
		      (when (and (stringp (org-element-property :parameters src))
				 (s-contains? ":gist" (org-element-property :parameters src)))
			(save-excursion
			  (goto-char (org-element-property :begin src))
			  (org-edit-special)
			  (gist-buffer)
			  (org-edit-src-abort)
			  (org-no-properties (pop kill-ring))))))))
    (when gists (setq msg (s-concat msg " " (s-join " " gists))))

    (list msg reply-id file)))


(defun scimax-twitter-tweet-headline (&optional force)
  "Tweet a headline.
The headline itself is the tweet, and the first image is
attached. If the headline is in a :tweet:thread:, reply if
necessary. Adds properties to the headline so you know what was
done."
  (interactive "P")
  (unless force
    (when (org-entry-get nil "TWITTER_MSGID")
      (user-error "This headline has already been tweeted.")))

  (when-let (account (org-entry-get nil "TWITTER_ACCOUNT" t))
    (shell-command (format "t set active %s" account)))

  (let* ((components (scimax-twitter-org-tweet-components))
	 (msgid (if (not (null (nth 1 components)))
		    ;; reply
		    (apply 'scimax-twitter-reply components)
		  (scimax-twitter-update (nth 0 components) (nth 2 components)))))
    (when (not (null (nth 1 components)))
      (org-entry-put nil "TWITTER_IN_REPLY_TO" (nth 1 components)))
    (org-entry-put nil "TWITTER_MSGID" msgid)
    (let* ((output (process-lines "t" "accounts"))
	   ;; Note: this may break if you have multiple keys on an account.
	   (i (-find-index
	       (lambda (s)
		 (s-contains? "(active)" s))
	       output))
	   (username (nth (- i 1) output)))
      (org-entry-put nil "TWITTER_URL" (format "https://twitter.com/%s/status/%s"
					       username
					       (s-trim msgid))))
    (message "%s" components)))

;; Replace the speed command
(setf (cdr (assoc "T" org-speed-commands-user)) 'scimax-twitter-tweet-headline)


(defun scimax-twitter-org-subtree-tweet-thread (&optional tweet)
  "Tweet the subtree as a thread.
The default behavior is to dry run and check each heading for length.
Use a prefix arg to make it actually tweet."
  (interactive "P")

  (when-let (account (org-entry-get nil "TWITTER_ACCOUNT" t))
    (shell-command (format "t set active %s" account)))

  (save-restriction
    (org-narrow-to-subtree)

    (save-excursion
      (goto-char (point-min))
      (unless (-contains?  (mapcar 'org-no-properties (org-get-tags)) "tweet")
	(let ((current-tags (org-get-tags)))
	  (org-set-tags (append current-tags '("tweet")))))

      (unless (-contains?  (mapcar 'org-no-properties (org-get-tags)) "thread")
	(let ((current-tags (org-get-tags)))
	  (org-set-tags (append current-tags '("thread")))))

      (while (looking-at org-heading-regexp)
	(if tweet
	    (scimax-twitter-tweet-headline)
	  (unless (scimax-twitter-check-length)
	    (org-todo "TODO")
	    (error "This headline is probably too long.")))
	(org-next-visible-heading 1)))))


(defun scimax-twitter-clear-thread-properties ()
  "Clear the Twitter properties in the subtree."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (while (looking-at org-heading-regexp)
	(org-entry-delete nil "TWITTER_URL")
	(org-entry-delete nil "TWITTER_MSGID")
	(org-entry-delete nil "TWITTER_IN_REPLY_TO")
	(org-next-visible-heading 1)))))

;; * Miscellaneous utilities

(defun scimax-twitter-status ()
  "Show status of tweet in current headline."
  (interactive)
  (message
   (shell-command-to-string
    (format "t status %s" (org-entry-get nil "TWITTER_MSGID")))))


(defun scimax-twitter-delete-status ()
  "Delete the tweet in the current headline."
  (interactive)
  (prog1
      (message
       (shell-command-to-string
	(format "echo y | t delete status %s" (org-entry-get nil "TWITTER_MSGID"))))
    (org-entry-put nil "TWITTER_MSGID" (concat (org-entry-get nil "TWITTER_MSGID")
					       " - deleted"))
    (org-entry-delete nil "TWITTER_URL")))


(defun scimax-twitter-delete-thread ()
  "Delete the tweets in the thread."
  (interactive)
  (save-restriction
    (org-narrow-to-subtree)
    (goto-char (point-min))
    (while (looking-at org-heading-regexp)
      (scimax-twitter-delete-status)
      (org-next-visible-heading 1))))


(defun scimax-twitter-dm (user msg)
  "Send USER a MSG by dm."
  (interactive
   (list
    (completing-read "User: " scimax-twitter-usernames)
    (read-string "Msg: ")))
  (message (shell-command-to-string
	    (format "t dm %s \"%s\"" user msg))))


(defun scimax-twitter-set-account (user)
  "Set the account to tweet from."
  (interactive (list (completing-read
		      "Account: "
		      (-slice (process-lines "t" "accounts") 0 -1 2))))
  (message (shell-command-to-string (format "t set active %s" user))))


(defun scimax-twitter-check-length ()
  "Return if the headline is less than 280 chars.
Any link will count 23 characters."
  (interactive)
  (let* ((text (nth 4 (org-heading-components)))
	 (nurls 0)
	 (nurl-chars 0)
	 (twitter-length)
	 (url-string)
	 (ret))
    ;; Count # urls and number of url chars.
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "http" nil t)
	(setq nurls (+ 1 nurls))
	(setq url-string (thing-at-point-url-at-point))
	(setq nurl-chars (+ nurl-chars (length url-string)))))

    (setq twitter-length (+ (- (length text) nurl-chars) (* 23 nurls)))

    (setq ret (< twitter-length 280 ))
    (message "Length is %s" (if ret "probably ok" "too long"))
    ret))


;; * Exporter
;; http://qaz.wtf/u/convert.cgi?text=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789
(defun scimax-twitter-filter-bold (text back-end info)
  (let ((plain "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
	(ubold "𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳𝟎𝟏𝟐𝟑𝟒𝟓𝟔𝟕𝟖𝟗")
	i)
    (replace-regexp-in-string "*" ""
			      (s-join "" (loop for letter across text collect
					       (progn
						 (setq i (s-index-of
							  (char-to-string letter)
							  plain))
						 (if i (substring ubold  i (cl-incf i))
						   (char-to-string letter))))))))


(defun scimax-twitter-filter-italic (text back-end info)
  (let ((plain "ABCDEFGHIJKLMNOPQRSTUVWZYZabcdefghijklmnopqrstuvwxyz0123456789")
	(uitalic "𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍𝑎𝑏𝑐𝑑𝑒𝑓𝑔ℎ𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧")
	i)
    (replace-regexp-in-string "/" ""
			      (s-join "" (loop for letter across text collect
					       (progn
						 (setq i (s-index-of
							  (char-to-string letter)
							  plain))
						 (if i (substring uitalic  i (cl-incf i))
						   (char-to-string letter))))))))


(defun scimax-twitter-filter-verbatim (text back-end info)
  (let ((plain "ABCDEFGHIJKLMNOPQRSTUVWZYZabcdefghijklmnopqrstuvwxyz0123456789")
	(uverbatim "𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚉𝚈𝚉𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠𝚡𝚢𝚣𝟶𝟷𝟸𝟹𝟺𝟻𝟼𝟽𝟾𝟿")
	i)
    (replace-regexp-in-string
     "`\\|'" ""
     (s-join "" (loop for letter across text collect
		      (progn
			(setq i (s-index-of
				 (char-to-string letter)
				 plain))
			(if i (substring uverbatim  i (cl-incf i))
			  (char-to-string letter))))))))


(defun scimax-twitter-export-headline (&rest args)
  "Pseudo-export function for tweeting a headline."
  (interactive)
  (scimax-twitter-tweet-headline))


(defun scimax-twitter-export-headline-force (&rest args)
  "Pseudo-export function for force tweeting a headline."
  (interactive)
  (scimax-twitter-tweet-headline t))


(defun scimax-twitter-export-subtree (&rest args)
  "Pseudo-export function for tweeting a subtree as a thread."
  (interactive)
  (scimax-twitter-org-subtree-tweet-thread))

(defun scimax-twitter-export-delete (&rest args)
  "Pseudo-export function for deleting a tweet in a headline."
  (scimax-twitter-delete-status))

(defun scimax-twitter-export-pdf (&rest args)
  (interactive)

  (let ((tw-handle-regex "\\(^\\|[[:punct:]]\\|[[:space:]]\\)\\(?2:@\\(?1:[[:alnum:]_]+\\)\\)")
	(tw-hashtag-regex "\\(^\\|[[:punct:]]\\|[[:space:]]\\)\\(?2:#\\(?1:[[:alnum:]]+\\)\\)")
	(org-export-with-toc nil)
	(org-export-with-title nil)
	(org-export-before-processing-hook '((lambda (_)
					       (while (re-search-forward tw-handle-regex nil t)
						 (replace-match (format " [[%s][@%s]]"
									(format "https://twitter.com/%s" (match-string 1))
									(match-string 1))
								t))

					       (goto-char (point-min))
					       (while (re-search-forward tw-hashtag-regex nil t)
						 (replace-match (format " [[%s][#%s]]"
									(format "https://twitter.com/hashtag/%s" (match-string 1))
									(match-string 1))
								t))
					       ;; This just makes figures a
					       ;; reasonable size. Most of the
					       ;; figure have attr_org
					       ;; attributes because they are
					       ;; screenshots.
					       (goto-char (point-min))
					       (while (re-search-forward "#\\+attr_org:" nil t)
						 (replace-match "#\+attr_latex: :placement [H] :width 3in"
								t))))))

    (org-open-file (org-latex-export-to-pdf nil t))))

(org-export-define-derived-backend 'twitter 'ascii
  :filters-alist '((:filter-bold . scimax-twitter-filter-bold)
		   (:filter-italic . scimax-twitter-filter-italic)
		   (:filter-verbatim . scimax-twitter-filter-verbatim))
  :menu-entry
  '(?w "Export with scimax-twitter"
       ((?h "Headline" scimax-twitter-export-headline)
	(?H "Headline (force)" scimax-twitter-export-headline-force)
	(?s "Subtree" scimax-twitter-export-subtree)
	(?d "delete" scimax-twitter-export-delete)
	(?p "pdf" scimax-twitter-export-pdf))))

;; * scheduling tweets

;; The Twitter API for scheduling tweets is not that easy to use, and you have
;; to register as an advertiser. Rather than do that, here I try to leverage the
;; at scheduler (available on Mac and Linux) to do scheduling of tweets. The
;; idea is to use the scheduled property on a headline to specify when to tweet
;; it, then create a shell script that runs to tweet it. This is limited to a
;; single image I think.

(unless (f-dir? (f-join scimax-twitter-directory "scheduled-tweets"))
  (make-directory (f-join scimax-twitter-directory "scheduled-tweets") t))

(defun scimax-twitter-schedule-tweet ()
  "Schedule a tweet using at.
This writes a shell script that saves the current account, sets
the active account, tweets, restores the account, and deletes the
script. It then schedules the tweet using at."
  (interactive)
  (let* ((id (org-id-get-create))
	 (sh-file (expand-file-name
		   (concat (org-entry-get nil "ID") ".sh")
		   (f-join scimax-twitter-directory "scheduled-tweets")))
	 (scheduled-time (format-time-string
			  "%Y%m%d%H%M" (org-get-scheduled-time nil)))
	 (components (scimax-twitter-org-tweet-components))
	 (msg (nth 0 components))
	 (img (nth 2 components))
	 (twitter-account (prog1 (org-entry-get nil "TWITTER_ACCOUNT" t)
			    (unless (org-entry-get nil "TWITTER_ACCOUNT" t)
			      (error "You need to set a TWITTER_ACCOUNT."))))
	 (cmd (concat
	       "account=`t whoami | grep \"Screen name\" |sed 's/.*@//'`\n"
	       (format "t set active %s\n" twitter-account)
	       (format "t update \"%s\"" msg)
	       (if img (format " -f %s" (expand-file-name img)) "\n")
	       ;; here is where we save the output in case we need the tweet id later, or if there is an error.
	       (format "> %s.txt\n" id)
	       ;; This restores the current active account
	       "t set active $account\n")))

    (with-temp-file sh-file
      (insert "#!/bin/bash\n")
      (insert cmd)
      ;; To avoid accumulating these, I remove the file
      (insert (format "rm %s\n" sh-file)))
    (org-entry-put nil "TWEET_SCHEDULED" sh-file)
    (shell-command (format "at -f %s -t %s"
			   sh-file
			   scheduled-time))))



(defun scimax-twitter-active-account ()
  "Return the active Twitter account."
  (interactive)
  (let ((s (car (-filter (lambda (s) (s-starts-with? "Screen name" s))
			 (process-lines "t" "whoami")))))
    (string-match
     "@\\(.*\\)" s)
    (match-string 1 s)))

;; (defun scimax-twitter-schedule-tweet ()
;;   "This sets a tweet to be scheduled.
;; This creates a file to be loaded later."
;;   (interactive)
;;   (let* ((id (org-id-get-create))
;; 	 (datafile (expand-file-name
;; 		    (concat (org-entry-get nil "ID") ".el")
;; 		    (f-join scimax-twitter-directory "scheduled-tweets")))
;; 	 (data `(progn
;; 		  (find-file ,(buffer-file-name))
;; 		  (re-search-forward ,id)
;; 		  (when (org-time>
;; 			 ;; current-time
;; 			 (format-time-string "<%Y-%m-%d %a %H:%M>")
;; 			 ;; scheduled entry time
;; 			 ,(format-time-string
;; 			   "<%Y-%m-%d %a %H:%M>" (org-get-scheduled-time nil)))
;; 		    (scimax-twitter-tweet-headline)
;; 		    (org-todo "DONE")
;; 		    (org-entry-put nil "TWEETED_AT"
;; 				   (format-time-string "<%Y-%m-%d %a %H:%M>"))
;; 		    (f-delete ,datafile)))))

;;     (with-temp-file datafile
;;       (pp data (current-buffer)))
;;     (org-entry-put nil "TWEET_SCHEDULED" datafile)))


;; (defun scimax-twitter-process-scheduled ()
;;   (interactive)
;;   (loop for file in
;; 	(f-files (f-join scimax-twitter-directory "scheduled-tweets"))
;; 	do
;; 	(message "Loading %s" file)
;; 	(load-file file)))


(provide 'scimax-twitter)

;;; scimax-twitter.el ends here
