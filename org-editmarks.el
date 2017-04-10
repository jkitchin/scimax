;;; org-editmarks.el --- Edit marks for org-mode



;;; Commentary:
;; This library provides a set of org-mode links for making "edit marks" in an
;; org-file. These are:
;; [[comment:some text]]
;; [[insert:some text]]
;; [[delete:some text]]
;; [[typo:mispeling]]


;; `em-typo'
;; `em-comment-1'

;; It has org-mode link limitations. So, don't include line breaks in the links.

;; The comment links export to LaTeX as todonotes. The delete,insert, and typo links export as colored text.

;; You need to put this in your header to get todonotes:
;; #+LATEX_HEADER: \usepackage[colorinlistoftodos]{todonotes}
;;
;; Put these in your org-file
;; \presetkeys{todonotes}{color=blue!30}{}
;; \todotoc
;; \listoftodos

;; Alternatively, use `ox-manuscript-build-with-comments', which should insert
;; those things automatically for you.

;;; Code:

(require 'org-inlinetask)

(defun em-escape-tex (s)
  "Escape the special characters in S for LaTeX export."
  (loop for (char . rep) in '(("\\" . "\\\\textbackslash")
			      ("&" . "\\\\&")
			      ("%" . "\\\\%")
			      ("$" . "\\\\$")
			      ("#" . "\\\\#")
			      ("_" . "\\\\_")
			      ("{" . "\\\\{")
			      ("}" . "\\\\}")
			      ("~" . "\\\\textasciitilde")
			      ("^" . "\\\\textasciicircum"))
	do
	(setq s (replace-regexp-in-string (regexp-quote char) rep s)))
  s)



;; * The links
(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "comment"
     :follow (lambda (path)
	       "Offer to delete the comment."
	       (em-delete-edit-mark-at-point))
     :export (lambda (path description format)
	       (cond
		((eq format 'latex)
		 (format "\\todo[inline]{%s}%s"
			 (em-escape-tex path)
			 (if description (format "{%s}" (em-escape-tex description)) "")))))
     :face 'em-comment-face)
  (org-add-link-type
   "comment"
   (lambda (path)
     "Offer to delete the comment."
     (em-delete-edit-mark-at-point))
   (lambda (path description format)
     (cond
      ((eq format 'latex)
       (format "\\todo[inline]{%s}%s"
	       (em-escape-tex path)
	       (if description (format "{%s}" (em-escape-tex description)) "")))))))


(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "delete"
     :follow (lambda (path)
	       (em-delete-edit-mark-at-point))
     :export (lambda (path description format)
	       (cond
		((eq format 'latex)
		 (format "\\textcolor{red}{%s}" (em-escape-tex path)))
		((eq format 'html)
		 (format "<font color=\"red\">%s</font>" path))))
     :face 'em-delete-face) 
  (org-add-link-type
   "delete"
   (lambda (path)
     (em-delete-edit-mark-at-point))
   (lambda (path description format)
     (cond
      ((eq format 'latex)
       (format "\\textcolor{red}{%s}" (em-escape-tex path)))))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "insert"
     :follow (lambda (path)
	       (em-delete-edit-mark-at-point))
     :export (lambda (path description format)
	       (cond
		((eq format 'latex)
		 (format "\\textcolor{blue}{%s}" (em-escape-tex path)))
		((eq format 'html)
		 (format "<font color=\"blue\">%s</font>" path))))
     :face 'em-insert-face) 
  (org-add-link-type
   "insert"
   (lambda (path)
     (em-delete-edit-mark-at-point))
   (lambda (path description format)
     (cond
      ((eq format 'latex)
       (format "\\textcolor{blue}{%s}" (em-escape-tex path)))))))


(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "typo"
     :follow (lambda (path)
	       "Spell check region, and offer to delete."
	       (let ((link (org-element-context)))
		 (save-excursion
		   (ispell-region (org-element-property :begin link)
				  (org-element-property :end link))
		   (when (y-or-n-p "Delete link? ")
		     (save-excursion
		       (setf (buffer-substring
			      (org-element-property :begin link)
			      (- (org-element-property :end link)
				 (org-element-property :post-blank link)))
			     (org-element-property :path (org-element-context))))))))
     :export (lambda (path description format)
	       (cond
		((eq format 'latex)
		 (format "\\bold{\\textcolor{red}{%s}}" (em-escape-tex path)))))
     :face 'em-typo-face)
  (org-add-link-type
   "typo"
   (lambda (path)
     "Spell check region, and offer to delete."
     (let ((link (org-element-context)))
       (save-excursion
	 (ispell-region (org-element-property :begin link)
			(org-element-property :end link))
	 (when (y-or-n-p "Delete link? ")
	   (save-excursion
	     (setf (buffer-substring
		    (org-element-property :begin link)
		    (- (org-element-property :end link)
		       (org-element-property :post-blank link)))
		   (org-element-property :path (org-element-context))))))))
   (lambda (path description format)
     (cond
      ((eq format 'latex)
       (format "\\bold{\\textcolor{red}{%s}}" (em-escape-tex path)))))))


;; ** Link faces and font-lock
(defface em-comment-face
  `((t (:inherit org-link
                 :foreground "DarkOrange1")))
  "Color for comment links.")


(defface em-insert-face
  `((t (:inherit org-link
                 :foreground "blue")))
  "Color for insert links.")


(defface em-delete-face
  `((t (:inherit org-link
                 :foreground "red"
		 :strike-through t)))
  "Color for delete links.")


(defface em-typo-face
  `((t (:inherit org-link
                 :foreground "red"
		 :weight bold)))
  "Color for typo links.")

(unless (fboundp 'org-link-set-parameters)
  (defun em-activate-links (link-type face limit)
    "Add text properties for editmark bracketed links."
    (while (and (re-search-forward org-bracket-link-regexp limit t)
		(not (org-in-src-block-p)))
      (let* ((hl (org-match-string-no-properties 1))
	     (type (save-match-data
		     (and (string-match org-plain-link-re hl)
			  (match-string-no-properties 1 hl))))
	     help ip vp)
	
	(when (string= type link-type)
	  (setq help (concat "LINK: " (save-match-data (org-link-unescape hl))) 
		ip (org-maybe-intangible
		    (list 'invisible 'org-link
			  'face face
			  'keymap org-mouse-map
			  'mouse-face 'highlight
			  'font-lock-multiline t
			  'help-echo help
			  'htmlize-link `(:uri ,hl)))
		vp (list 'keymap org-mouse-map
			 'face face
			 'mouse-face 'highlight
			 'font-lock-multiline t
			 'help-echo help
			 'htmlize-link `(:uri ,hl)))
	  ;; We need to remove the invisible property here.  Table narrowing
	  ;; may have made some of this invisible.
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(invisible nil))
	  (if (match-end 3)
	      (progn
		(add-text-properties (match-beginning 0) (match-beginning 3) ip)
		(org-rear-nonsticky-at (match-beginning 3))
		(add-text-properties (match-beginning 3) (match-end 3) vp)
		(org-rear-nonsticky-at (match-end 3))
		(add-text-properties (match-end 3) (match-end 0) ip)
		(org-rear-nonsticky-at (match-end 0)))
	    (add-text-properties (match-beginning 0) (match-beginning 1) ip)
	    (org-rear-nonsticky-at (match-beginning 1))
	    (add-text-properties (match-beginning 1) (match-end 1) vp)
	    (org-rear-nonsticky-at (match-end 1))
	    (add-text-properties (match-end 1) (match-end 0) ip)
	    (org-rear-nonsticky-at (match-end 0)))
	  t))))


  (defun em-activate-comments (limit)
    (em-activate-links "comment" 'em-comment-face limit))


  (defun em-activate-typos (limit)
    (em-activate-links "typo" 'em-typo-face limit))


  (defun em-activate-inserts (limit)
    (em-activate-links "insert" 'em-insert-face limit))


  (defun em-activate-deletes (limit)
    (em-activate-links "delete" 'em-delete-face limit))


  (defun em-font-lock-enable ()
    "Add the font "
    (font-lock-add-keywords
     nil
     '((em-activate-comments (0  'em-comment-face t))
       (em-activate-typos (0  'em-typo-face t))
       (em-activate-inserts (0  'em-insert-face t))
       (em-activate-deletes (0  'em-delete-face t)))
     t))


  (add-hook 'org-mode-hook 'em-font-lock-enable))

;; * Insertion functions
(defvar *em-window-configuration* nil
  "Stores window configuration when inserting a comment.")

(defun em-finish-comment (buffer)
  "Finish a comment indirect buffer.
This replaces the text with a link, or if there is a line break
in the comment a block."
  (interactive "b") 
  (let ((comment (buffer-string)))
    (kill-ring-save (point-min) (point-max))
    (setf (buffer-substring (point-min) (point-max))
	  (format
	   (if (string-match "\n" comment)
	       "#+BEGIN_COMMENT\n%s\n#+END_COMMENT\n"
	     "[[comment:%s]]")
	   comment)) 
    (kill-buffer) 
    (set-window-configuration *em-window-configuration*)))


(defun em-comment-1 ()
  "Insert a comment with an indirect buffer."
  (interactive)
  (setq *em-window-configuration* (current-window-configuration))
  (let ((cb (current-buffer)))
    (clone-indirect-buffer-other-window nil t)
    (setq header-line-format "Enter comment. Type s-<return> to finish.")
    (narrow-to-region (point) (point))
    (local-set-key (kbd "s-<return>")
		   `(lambda ()
		      (interactive)
		      (em-finish-comment ,cb)))))


(defvar em-comment-list '()
  "A list of previously used comments.")

(defun em-comment (comment)
  "Insert a link with COMMENT as the description.
The comment is entered interactively in the minibuffer. If region
is active, it is wrapped in the comment."
  (interactive (list (read-string "Comment: " nil 'em-comment-list)))
  (add-to-list 'em-comment-list comment)
  (if (region-active-p)
      (setf (buffer-substring (region-beginning)
			      (region-end))
	    (format "[[comment:%s][%s]] " 
		    comment
		    (buffer-substring (region-beginning) (region-end))))
    (insert (format "[[comment:%s]] " 
		    comment))))


(defun em-typo ()
  "Insert a typo link.
If the region is active, enclose it in the link, otherwise wrap the word at point."
  (interactive)
  (if (region-active-p)
      (setf (buffer-substring (region-beginning)
			      (region-end))
	    (format "[[typo:%s]]" (buffer-substring (region-beginning)
						    (region-end))))

    (let ((w (thing-at-point 'word)))
      (backward-char (length w))
      (re-search-forward w)
      (setf (buffer-substring (match-beginning 0) (match-end 0))
	    (format "[[typo:%s]]" (match-string 0))))))


(defun em-insert ()
  "Insert an insert link, or mark the active region for insertion."
  (interactive)
  (if (region-active-p)
      (setf (buffer-substring (region-beginning)
			      (region-end))
	    (format "[[insert:%s]] " 
		    (buffer-substring (region-beginning) (region-end))))
    (insert "[[insert:]] ")
    (backward-char 3)))


(defun em-delete (r1 r2)
  "Mark the region from R1 to R2 for deletion."
  (interactive "r")
  (setf (buffer-substring (region-beginning)
			  (region-end))
	(format "[[delete:%s]] " 
		(buffer-substring (region-beginning) (region-end))))
  (goto-char (org-element-property :end (org-element-context))))


(defun em-replace (r1 r2)
  "Delete the region R1 to R2 and insert new text."
  (interactive "r")
  (em-delete r1 r2)
  (deactivate-mark)
  (em-insert))


(defun em-next-editmark ()
  "Jump to next editmark."
  (interactive)
  (re-search-forward "\\(insert\\|delete\\|typo\\|comment\\):" nil t))


(defun em-previous-editmark ()
  "Jump to previous editmark."
  (interactive)
  (when
      (re-search-backward "\\(insert\\|delete\\|typo\\|comment\\):" nil t)
    (forward-char 1)))


;; * Delete an editmark
;;;###autoload
(defun em-delete-edit-mark-at-point ()
  "Delete the editmark at point."
  (interactive)
  (let ((link (org-element-context)))
    (cond
     ((member (org-element-property :type link) '("typo"
						  "insert"
						  "delete"))
      (when (y-or-n-p "Delete? ")
	(setf (buffer-substring (org-element-property :begin link)
				(org-element-property :end link))
	      "")))
     ((string= (org-element-property :type link) "comment")
      (when (y-or-n-p "Delete? ")
	(setf (buffer-substring (org-element-property :begin link)
				(org-element-property :end link))
	      (if (org-element-property :contents-begin link)
		  ;; this is a [[comment:text][wrapped text]]
		  (buffer-substring (org-element-property :contents-begin link)
				    (org-element-property :contents-end link))

		""))))
     (t
      (error "%s not implemented for deletion." (org-element-property :type link))))))

;;;###autoload
(defun em-accept-edit-mark-at-point ()
  "Accept the editmark at point.
For insert, it means delete the link and keep the insert text.
For delete, comment, and typo it means delete the link."
  (interactive)
  (let ((link (org-element-context)))
    (cond
     ;; accept means delete these links
     ((member (org-element-property :type link) '("typo"
						  "comment"
						  "delete"))
      (setf (buffer-substring (org-element-property :begin link)
			      (- (org-element-property :end link)
				 (org-element-property :post-blank link)))
	    ""))
     ;; accept means take the insert path.
     ((string= (org-element-property :type link) "insert") 
      (setf (buffer-substring (org-element-property :begin link)
			      (- (org-element-property :end link)
				 (org-element-property :post-blank link)))
	    (org-element-property :path link)))
     (t
      (error "%s not implemented" (org-element-property :type link))))))


;;;###autoload
(defun em-reject-edit-mark-at-point ()
  "Reject the editmark at point.
For delete, it means delete the link and keep the delete text.
For insert, comment, and typo it means delete the link."
  (interactive)
  (let ((link (org-element-context)))
    (cond
     ;; accept means delete these links
     ((member (org-element-property :type link) '("typo"
						  "comment"
						  "insert"))
      (setf (buffer-substring (org-element-property :begin link)
			      (- (org-element-property :end link)
				 (org-element-property :post-blank link)))
	    ""))
     ;; reject means take the delete path.
     ((string= (org-element-property :type link) "delete") 
      (setf (buffer-substring (org-element-property :begin link)
			      (- (org-element-property :end link)
				 (org-element-property :post-blank link)))
	    (org-element-property :path link)))
     (t
      (error "%s not implemented" (org-element-property :type link))))))


;; * Convenience

;;;###autoload
(defun em-accept-all-changes ()
  "Accept all editmarks."
  (interactive)
  (goto-char (point-min))
  (while (em-next-editmark)
    (em-accept-edit-mark-at-point)))


;;;###autoload
(defun em-reject-all-changes ()
  "Reject all editmarks."
  (interactive)
  (goto-char (point-min))
  (while (em-next-editmark)
    (em-reject-edit-mark-at-point)))

;; * See all Edit Marks

(defun em-editmark-candidates ()
  "Provides an alist of editmarks and their beginning position."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (member (org-element-property :type link) '("comment"
							"typo"
							"insert"
							"delete"))
	(cons (buffer-substring (org-element-property :begin link)
				(org-element-property :end link))
	      (org-element-property :begin link))))))


;;;###autoload
(defun em-editmarks ()
  "Show an ivy selection buffer of all the editmarks in the buffer.
The default action is to visit the mark."
  (interactive)
  (ivy-read "Select editmark: " (em-editmark-candidates)
	    :action '(1
		      ("o" (lambda (x)
			     (goto-char (cdr x)))
		       "open editmark")
		      ("d" (lambda (x)
			     (goto-char (cdr x))
			     (em-delete-edit-mark-at-point))
		       "delete editmark"))))


;; * Keybindings

(define-prefix-command 'em-map)
(global-set-key (kbd "H-e") em-map)

(define-key em-map "t" 'em-typo)
(define-key em-map "c" 'em-comment-1)
(define-key em-map "m" 'em-comment)
(define-key em-map "i" 'em-insert)
(define-key em-map "d" 'em-delete)
(define-key em-map "k" 'em-delete-edit-mark-at-point)
(define-key em-map "l" 'em-editmarks)
(define-key em-map "r" 'em-replace)
(define-key em-map "o" 'org-inlinetask-insert-task)

(define-key em-map "n" 'em-next-editmark)
(define-key em-map "p" 'em-previous-editmark)

(define-key em-map "a" 'em-accept-edit-mark-at-point)
(define-key em-map "j" 'em-reject-edit-mark-at-point)

(define-key em-map "A" 'em-accept-all-changes)
(define-key em-map "R" 'em-reject-all-changes)

(define-key em-map "w" 'em-wdiff-git)


;; * editmarks diff

(defcustom em-wdiff-cmd
  "wdiff -w [[delete: -x ]] -y [[insert: -z ]] "
  "Command to run wdiff with.")

(defun em-git-commit-selector ()
  "Return list of commits."
  (helm :sources `((name . "commits")
		   (candidates . ,(mapcar (lambda (s)
					    (let ((commit
						   (nth
						    0
						    (split-string s))))
					      (cons s
						    commit)))
					  (split-string
					   (shell-command-to-string
					    "git log --pretty=format:\"%h %ad | %s%d [%an]\" --date=relative") "\n")))
		   (action . (lambda (candidate)
			       (helm-marked-candidates))))))

(defun em-wdiff-git (commits)
  "Perform a wdiff between git COMMITS.
A helm selection buffer is used to choose commits.

If you choose one commit, the wdiff is between that commit and
the current version.

If you choose two commits, the wdiff is between those two
commits. Returns the buffer."
  (interactive
   (list (em-git-commit-selector)))
  (let ((buf (get-buffer-create
	      "*org-wdiff-git*"))
	(mmode major-mode)
	(git-root (vc-git-root
		   (buffer-file-name)))
	(fname
	 (file-relative-name
	  (buffer-file-name)
	  (vc-git-root (buffer-file-name))))
	cmd)
    (cond
     ;; current version vs commit
     ((= 1 (length commits))
      (setq cmd (format "%s <(git show %s:%s) %s"
			em-wdiff-cmd
			(car commits) fname
			fname)))
     ;; more than 1 commit, we just take first two
     ((> (length commits) 1)
      (setq cmd (format "%s <(git show %s:%s) <(git show %s:%s)"
			em-wdiff-cmd
			(nth 0 commits) fname
			(nth 1 commits) fname))))

    ;; Save fname in global var for convenience to save buffer later
    (setq *em-wdiff-git-source* fname)
    (switch-to-buffer-other-window buf)
    (let ((inhibit-read-only t))
      (erase-buffer))

    ;; Try to keep same major mode
    (funcall mmode)

    ;; get the wdiff. we do this in git-root so the paths are all correct.
    (let ((default-directory git-root))
      (insert (shell-command-to-string cmd))) 
    (goto-char (point-min))
    buf))

(provide 'org-editmarks)

;;; org-editmarks.el ends here
