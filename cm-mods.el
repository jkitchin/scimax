;;; cm-mods.el --- some additions to cm-mode                              -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Builds on https://github.com/joostkremers/criticmarkup-emacs

;;; Code:

(defcustom cm-wdiff-cmd
  "wdiff -w {-- -x --} -y {++ -z ++} "
  "Command to run wdiff with."
  :group 'cm-mods)

(defvar *cm-wdiff-git-source* nil
  "Global var to hold filename for cm-wdiff-git.")

(require 'cm-mode)
(require 'vc-git)

;; I like more obvious colors
(set-face-foreground cm-deletion-face "red")
(set-face-bold cm-deletion-face t)

(set-face-foreground cm-addition-face "blue")
(set-face-bold cm-addition-face t)

(set-face-foreground cm-comment-face "orange")
(set-face-bold cm-comment-face t)

;;* Parser approach to fontification
;; These functions set match data for fontification.
;; This is how to add them
;; (font-lock-add-keywords
;;  nil
;;  '((cm-next-deletion (0 cm-deletion-face))
;;    (cm-next-addition (0 cm-addition-face))))

;; the original cm-mode regular expressions are used. Here I use a search based,
;; parsing approach to be more robust over multiple lines.

(defun cm-next-deletion (limit)
  "Find the next deletion up to LIMIT.
{--content--}.
Sets `match-data'.
group 0 the whole match
group 1 opening marker
group 2 closing marker
group 3 the content.

See `cm-forward-deletion' for an alternative."
  (let (start end s1 s2 e1 e2 contents-start contents-end)
    (when (re-search-forward
	   (regexp-quote
	    (nth 1 (assoc 'cm-deletion cm-delimiters)))
	   limit t)
      (setq start (match-beginning 0)
            s1 (match-beginning 0)
            s2 (match-end 0)
            contents-start (match-end 0)))

    (when (re-search-forward
	   (regexp-quote
	    (nth 2 (assoc 'cm-deletion cm-delimiters)))
	   limit t)
      (setq end (match-end 0)
            e1 (match-beginning 0)
            e2 (match-end 0)
            contents-end (match-beginning 0)))

    (if (and start end)
        (progn
	  (let ((inhibit-read-only t))
	    (set-text-properties start end '(font-lock-multiline t)))
          (set-match-data (list
                           start end
                           s1 s2
                           e1 e2
                           contents-start contents-end))
	  ;; return t for font-lock to work
          t)
      nil)))


(defun cm-next-addition (limit)
  "Find the next addition up to LIMIT.
{++content++}.
Sets `match-data'.
group 0 the whole match
group 1 opening marker
group 2 closing marker
group 3 the content.

See `cm-forward-addition' for an alternative."
  (let (start end s1 s2 e1 e2 contents-start contents-end)
    (when (re-search-forward
	   (regexp-quote
	    (nth 1 (assoc 'cm-addition cm-delimiters)))
	   limit t)
      (setq start (match-beginning 0)
            s1 (match-beginning 0)
            s2 (match-end 0)
            contents-start (match-end 0)))

    (when (re-search-forward
	   (regexp-quote
	    (nth 2 (assoc 'cm-addition cm-delimiters)))
	   limit t)
      (setq end (match-end 0)
            e1 (match-beginning 0)
            e2 (match-end 0)
            contents-end (match-beginning 0)))

    (if (and start end)
        (progn
	  (let ((inhibit-read-only t))
	    (set-text-properties start end '(font-lock-multiline t)))
          (set-match-data (list
                           start end
                           s1 s2
                           e1 e2
                           contents-start contents-end))
	  ;; return t for font-lock to work
          t)
      nil)))


(defun cm-next-comment (limit)
  "Find the next addition up to LIMIT.
{>>content<<}.
Sets `match-data'.
group 0 the whole match
group 1 opening marker
group 2 closing marker
group 3 the content.

See `cm-forward-comment' for an alternative."
  (let (start end s1 s2 e1 e2 contents-start contents-end)
    (when (re-search-forward
	   (regexp-quote
	    (nth 1 (assoc 'cm-comment cm-delimiters)))
	   limit t)
      (setq start (match-beginning 0)
            s1 (match-beginning 0)
            s2 (match-end 0)
            contents-start (match-end 0)))

    (when (re-search-forward
	   (regexp-quote
	    (nth 2 (assoc 'cm-comment cm-delimiters)))
	   limit t)
      (setq end (match-end 0)
            e1 (match-beginning 0)
            e2 (match-end 0)
            contents-end (match-beginning 0)))

    (if (and start end)
        (progn
	  (let ((inhibit-read-only t))
	    (set-text-properties start end '(font-lock-multiline t)))
          (set-match-data (list
                           start end
                           s1 s2
                           e1 e2
                           contents-start contents-end))
	  ;; return t for font-lock to work
          t)
      nil)))


;;* Convenience functions

(defun cm-accept-all-changes ()
  "Accept all changes in the document."
  (interactive)
  (goto-char (point-min))
  (while (cm-forward-change)
    (let ((change (cm-expand-change (cm-markup-at-point)))
	  (inhibit-read-only t))
      (cm-without-following-changes
	(delete-region (third change) (fourth change))
	(insert (cm-substitution-string change ?a))))))


(defun cm-reject-all-changes ()
  "Reject all changes in the document."
  (interactive)
  (goto-char (point-min))
  (while (cm-forward-change)
    (let ((change (cm-expand-change (cm-markup-at-point)))
	  (inhibit-read-only t))
      (cm-without-following-changes
	(delete-region (third change) (fourth change))
	(insert (cm-substitution-string change ?r))))))


;;* Convert cm markup to LaTeX
(defun multiline-p (content)
  (save-match-data
    (string-match "\n" content)))

(defun cm-markup-to-org-latex ()
  "Convert cm markup in an org-file to LaTeX.
Inserts some headers at the top for todonotes and ulem, and the
LaTeX markup commands. This is not super robust, but works for
simple changes. There are issues with changes in citations,
tables, and other changes that cross org-element boundaries."
  (interactive)
  (goto-char (point-min))
  (insert "
#+latex_header: \\usepackage[normalem]{ulem}
#+latex_header: \\usepackage{todonotes}
#+latex_header: \\usepackage[usenames, dvipsnames]{color}

#+latex_header: \\newcommand\\cmred{\\bgroup\\markoverwith{\\textcolor{red}{\\rule[0.5ex]{4pt}{1.4pt}}}\\ULon}
#+latex_header: \\newcommand\\cmblue{\\bgroup\\markoverwith{\\textcolor{blue}{\\rule[-0.5ex]{4pt}{1.4pt}}}\\ULon}

")
  ;; comments should only be one line so we wrap them in a snippet.
  (goto-char (point-min))
  (while (cm-next-comment nil)
    (replace-match "@@latex:\\\\todo{\\3}@@"))

  ;; Deletions
  (goto-char (point-min))
  (while (cm-next-deletion nil)
    (replace-match "@@latex:\\\\protect\\\\cmred{\\3}@@"))

  ;; Additions
  (goto-char (point-min))
  (while (cm-next-addition nil)
    (replace-match "@@latex:\\\\protect\\\\cmblue{\\3}@@")))


(defun cm-wdiff-to-pdf (commits)
  "On current buffer, select a commit(s) and convert the wdiff to
a PDF."
  (interactive (list (cm-git-commit-selector)))

  (with-current-buffer
      (cm-wdiff-git (list (if (= (length commits) 2)
			      (nth 1 commits)
			    "HEAD")
			  (nth 0 commits)))

    (cm-markup-to-org-latex)
    (let ((revised-org (replace-regexp-in-string
			".org" "-revised.org"
			*cm-wdiff-git-source*)))

      (write-file revised-org)
      (ox-manuscript-export-and-build-and-open)
      (kill-buffer revised-org))))



;;* Get cm markup with wdiff and git


(defun cm-git-commit-selector ()
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


(defun cm-wdiff-git (commits)
  "Perform a wdiff between git commits.
a helm selection buffer is used to choose commits.

If you choose one commit, the wdiff is between that commit and
the current version.

If you choose two commits, the wdiff is between those two
commits. Returns the buffer."
  (interactive
   (list (cm-git-commit-selector)))
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
			cm-wdiff-cmd
			(car commits) fname
			fname)))
     ;; more than 1 commit, we just take first two
     ((> (length commits) 1)
      (setq cmd (format "%s <(git show %s:%s) <(git show %s:%s)"
			cm-wdiff-cmd
			(nth 0 commits) fname
			(nth 1 commits) fname))))

    ;; Save fname in global var for convenience to save buffer later
    (setq *cm-wdiff-git-source* fname)
    (switch-to-buffer-other-window buf)
    (let ((inhibit-read-only t))
      (erase-buffer))

    ;; Try to keep same major mode
    (funcall mmode)

    ;; get the wdiff. we do this in git-root so the paths are all correct.
    (let ((default-directory git-root))
      (insert (shell-command-to-string cmd)))

    ;; Turn on cm-mode
    (cm-mode)
    (goto-char (point-min))
    buf))


(defun cm-wdiff-save ()
  "Save changes.
IF there is an *org-wdiff-git* buffer, then we copy that content
to the buffer visiting `*cm-wdiff-git-source*'. You may use
*org-wdiff-git* to accept/reject changes, and then put it back to
where it came from. Otherwise we just save the buffer."
  (interactive)
  (if (get-buffer "*org-wdiff-git*")
      (progn
	(switch-to-buffer (find-buffer-visiting *cm-wdiff-git-source*))
	(erase-buffer)
	(insert-buffer-substring "*org-wdiff-git*")
	(kill-buffer "*org-wdiff-git*"))
    (save-buffer)))


(defun cm-wdiff-buffer-with-file ()
  "Do a wdiff of the buffer with the last saved version.
For line-based diff use `diff-buffer-with-file'."
  (interactive)
  (let ((contents (buffer-string))
	(tempf (make-temp-file "wdiff-"))
	(fname (buffer-file-name)))
    (with-temp-file tempf
      (insert contents))

    (switch-to-buffer "*wdiff-buffer*")
    (insert
     (shell-command-to-string
      (format "%s %s %s"
	      cm-wdiff-cmd
	      fname
	      tempf)))
    (delete-file tempf)
    (goto-char (point-min))
    (cm-mode)))

;;* A Hydra menu
(defhydra cm (:color blue :hint nil)
  "
Track changes:
_i_: insert text  _d_: delete text     _c_: comment
_n_: next change  _p_: previous change _e_: accept/reject this change
_a_: acc/rej all  ^ ^                  _t_: toggle track changes
_A_: accept all   _R_: reject all      _s_: save changes
_b_: buffer wdiff _g_: git wdiff
"
  ("i" cm-addition)
  ("d" cm-deletion)
  ("c" cm-comment)
  ("t" (lambda ()
	 (interactive)
	 (unless cm-mode
	   (cm-mode))
	 (cm-follow-changes 'toggle)))
  ("n" cm-forward-change :color red)
  ("p" cm-backward-change :color red)
  ("e" cm-accept/reject-change-at-point :color red)
  ("a" cm-accept/reject-all-changes)
  ("A" cm-accept-all-changes)
  ("R" cm-reject-all-changes)
  ("g" cm-wdiff-git)
  ("b" cm-wdiff-buffer-with-file)
  ("s" cm-wdiff-save))

(global-set-key (kbd "H-t") 'cm/body)

(provide 'cm-mods)

;;; cm-mods.el ends here
