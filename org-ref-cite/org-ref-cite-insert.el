;;; org-ref-cite-insert.el --- org-cite insert processor
;;
;; Copyright(C) 2021 John Kitchin
;;
;; This file is not currently part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; Provides an insert processor for `org-cite' that is based on
;; `bibtex-completion' for the candidates, and `ivy' for completion. It also
;; provides `ivy-bibtex' actions.
;;
;; I spent a long time trying to find a way to avoid the dependency on `ivy-read'
;; here, and concluded that is is not possible. The issue is that I want to be
;; able to insert one or more citations from a single call to `org-cite-insert',
;; with the following behavior:
;;
;; 1. RET always inserts the current or marked entries and ends the completion.
;; 2. You can sequentially insert entries one at a time without ending
;; completion using keys like C-M-m/n/p
;; 3. You can mark entries without inserting them, and insert at the end when you
;; press RET.
;; 4. If you need an alternate action instead of inserting, you should be able
;; to be able to call it, with or without closing completion. For example, if
;; you want to open the notes/pdf/url of a candidate to check something before
;; inserting it.
;;
;; All four are necessary (IMO) and that can only reasonably be accomplished
;; with `ivy-read'. I do not see a way to use `completing-read' to get the
;; behavior described above.
;;
;;; Code:

(require 'ivy)
(require 'bibtex-completion)
(require 'ivy-bibtex)
(require 'org-ref-cite-core)


;; * Inserting

;; all these functions are defined in ivy-bibtex
(defcustom org-ref-cite-alternate-insert-actions
  '(("p" ivy-bibtex-open-pdf "Open PDF file (if present)")
    ("u" ivy-bibtex-open-url-or-doi "Open URL or DOI in browser")
    ("c" ivy-bibtex-insert-citation "Insert citation")
    ("r" ivy-bibtex-insert-reference "Insert reference")
    ("k" ivy-bibtex-insert-key "Insert BibTeX key")
    ("b" ivy-bibtex-insert-bibtex "Insert BibTeX entry")
    ("a" ivy-bibtex-add-PDF-attachment "Attach PDF to email")
    ("e" ivy-bibtex-edit-notes "Edit notes")
    ("s" ivy-bibtex-show-entry "Show entry")
    ("l" ivy-bibtex-add-pdf-to-library "Add PDF to library")
    ("f" (lambda (_candidate) (ivy-bibtex-fallback ivy-text)) "Fallback options"))
  "Alternate actions to do instead of inserting.")


(defun org-ref-cite-insert-processor (context arg)
  "Function for inserting a citation.
With one prefix ARG, set style
With two prefix ARG delete reference/cite at point.
Argument CONTEXT is an org element at point, usually a citation
or citation-reference.
This is called by `org-cite-insert'."
  (interactive (list (org-element-context) current-prefix-arg))

  ;; I do this here in case you change the actions after loading this, so that
  ;; it should be up to date.
  (ivy-set-actions
   'org-cite-insert
   org-ref-cite-alternate-insert-actions)

  (cond
   ;; the usual case where we insert a ref
   ((null arg)
    (bibtex-completion-init)
    (let* ((candidates (bibtex-completion-candidates)))
      (ivy-read "BibTeX entries: " candidates
		:action (lambda (candidate)
			  (org-ref-cite-insert-citation
			   (cdr (assoc "=key=" (cdr candidate))) arg)))))

   ;; Here you are either updating the style, or inserting a new ref with a
   ;; selected style.
   ((= (prefix-numeric-value  arg) 4)
    (if context
	(org-ref-cite-update-style)
      (bibtex-completion-init)
      (let* ((candidates (bibtex-completion-candidates)))
	(ivy-read "BibTeX entries: " candidates
		  :caller 'org-cite-insert
		  :action '(1
			    ("i" (lambda (candidate)
				   (org-ref-cite-insert-citation
				    (cdr (assoc "=key=" (cdr candidate)))
				    current-prefix-arg)) "insert"))))))

   ;; delete thing at point, either ref or citation
   ((= (prefix-numeric-value  current-prefix-arg) 16)
    (when (memq (org-element-type context) '(citation citation-reference))
      (org-cite-delete-citation context)))))


(defun org-ref-cite-insert-citation (select-key arg)
  "Insert a citation.
If you are not on a citation, insert one.
If you are the beginning (on @) insert before the current cite
If you at the end of a reference or the citation (on ; or ] or post blanks)
 insert after  the current cite
If you are in the middle of a cite, replace it.

If the point is looking back at a cite, we append to it.
Argument SELECT-KEY is a string containing a bibtex key.
Argument ARG prefix arg."

  ;; with ivy, the context may change if you are running multiple commands
  ;; so we get a new one every time.
  (let ((context (org-element-context)))
    (when (and (eq 'citation (org-element-type context))
	       (>= (point) (cdr (org-cite-boundaries context))))
      (skip-chars-backward "]"))

    (pcase (org-element-type context)
      ;; When on a citation, check point is not on the blanks after it.
      ;; Otherwise, consider we're after it.
      ((and 'citation
	    (guard
	     (let ((boundaries (org-cite-boundaries context)))
	       (and (< (point) (cdr boundaries))
		    (> (point) (car boundaries))))))
       ;; action depends on the point.
       (let* ((begin (org-element-property :begin context))
	      (style-end (1- (org-with-point-at begin (search-forward ":")))))
	 (when (>= style-end (point))
	   (search-forward ":"))
	 (let* ((references (org-cite-get-references context))
		(key (concat "@" select-key)))
	   (if (< (point) (org-element-property :contents-begin context))
	       (org-cite--insert-string-before key (car references))
	     (org-cite--insert-string-after key (org-last references))))))

      ;; On a citation reference.  action depends on the point.
      ('citation-reference
       (pcase-let* ((`(,start . ,end) (org-cite-key-boundaries context))
		    (key (concat "@" select-key)))
	 ;; Right before the "@" character, do not replace the reference
	 ;; at point, but insert a new one before it.  It makes adding
	 ;; a new reference at the beginning easier in the following
	 ;; case: [cite:@key].
	 (cond
	  ((>= start (point)) (org-cite--insert-string-before key context))
	  ((<= end (point)) (org-cite--insert-string-after key context))
	  (t
	   (org-with-point-at start
	     (delete-region start end)
	     (insert key))))))
      (_
       (insert
	(format "[cite%s:%s]"
		;; this is the style. with arg, choose it
		(if arg
		    (concat "/" (org-ref-cite-select-style))
		  "/t")
		(concat "@" select-key)))))))

(provide 'org-ref-cite-insert)

;;; org-ref-cite-insert.el ends here
