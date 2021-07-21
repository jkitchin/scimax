;;; oc-bibtex.el --- Citation processor using bibtex  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library registers the `bibtex' citation processor, which provides the
;; "export" capability for citations. This is for people who use bibtex with
;; LaTeX, and not biblatex.

;; It is lightly adapted from oc-natbib.el to provide a user-experience more consistent with `org-ref'.
;; These include:
;; 1. green cite links
;; 2. cites that more closely resemble the natbib cite links from org-ref.
;; 3. Use ivy-bibtex for key selection
;; 4. Use a hydra for following
;; 5. There is a keymap on each citation like in org-ref.

;;; Code:
(require 'oc)
(require 'ivy-bibtex)
(require 'pretty-hydra)

;; I like green links
(set-face-attribute 'org-cite nil
                    :foreground "DarkSeaGreen4")

(set-face-attribute 'org-cite-key nil
                    :foreground "forest green")


;; org-cite uses (style . option) for styles, but that is more complicated than
;; I need. I use simple strings here.
(defcustom oc-bibtex-styles
  '(;; In principle, this should map to \citet, but we use natmove alot, and it
    ;; only works on \cite. So, I am making it be \cite here.
    ("t" . "\\cite")
    ("p" . "\\citep")
    ("num" . "\\citenum")  		;this may not be csl compatible
    ("a" . "\\citeauthor")
    ("a/f" . "\\citeauthor*")
    ("a/c" . "\\Citeauthor")
    ("a/cf" . "\\Citeauthor*")
    ;; Suppress author
    ("na/b" . "\\citeyear")
    ("na" . "\\citeyearpar")
    ("nocite" . "\\nocite")
    ;; text styles
    ("t/b" . "\\citealt")
    ("t/f" . "\\citet*")
    ("t/bf" . "\\citealt*")
    ("t/c" . "\\Citet")
    ("t/cf" . "\\Citet*")
    ("t/bc" . "\\Citealt")
    ("t/bcf" . "\\Citealt*")
    ;; bare styles
    ("/b" . "\\citealp")
    ("/bf" . "\\citealp*")
    ("/bc" . "\\Citealp")
    ("/bcf" . "\\Citealp*")
    ("/f" . "\\citep*")
    ("/c" . "\\Citep")
    ("/cf" . "\\Citep*"))
  "Styles for natbib citations.
See http://tug.ctan.org/macros/latex/contrib/natbib/natnotes.pdf

\citetext is not supported as it does not use a key, and doesn't
seem to do anything in the bibliography. citetalias and
citepalias are also not supported. Otherwise I think this covers
the natbib types, and these styles are compatible with csl styles
that look similar.

`oc-natbib.el' chooses \citep as default. This library uses
\citet because it is more common in the scientific writing of the
author.")


(defcustom oc-bibtex-alternate-insert-actions
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
  "Atlternate actions to do instead of inserting.")


(defun org-cite-bibtex--style-to-command (style)
  "Return command name to use according to STYLE pair.
Defaults to citet"
  (or (cdr (assoc style oc-bibtex-styles)) "\\citet"))

;; modified so it only uses
(defun org-cite-list-bibliography-files ()
  "List all bibliography files defined in the buffer."
  (delete-dups
   (or
    (mapcar (lambda (value)
	      (pcase value
		(`(,f . ,d)
                 (expand-file-name (org-strip-quotes f) d))))
	    (pcase (org-collect-keywords
                    '("BIBLIOGRAPHY") nil '("BIBLIOGRAPHY"))
	      (`(("BIBLIOGRAPHY" . ,pairs)) pairs)))
    org-cite-global-bibliography)))

;; * Flyspell setup
;; keys are often misspelled, so we turn that off here.

(defun oc-bibtex-flyspell-predicate ()
  (interactive)
  (let ((faces (get-text-property (point) 'face)))
    (when (not (listp faces))
      (setq faces (list faces)))
    (and (org-mode-flyspell-verify) (not (memq 'org-cite faces)))))

(put 'org-mode 'flyspell-mode-predicate 'oc-bibtex-flyspell-predicate)

;; * Navigation functions
;; There can be some subtle failures when there are duplicate keys sometimes.
(defun oc-bibtex-next-reference ()
  "Move point to the next reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum))
			datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position refs current-ref
						(lambda (r1 r2)
						  (= (org-element-property :begin r1)
						     (org-element-property :begin r2)))))))
    (cond
     ;; ((null current-ref)
     ;;  (goto-char (org-element-property :begin (first (org-cite-get-references  datum)))))
     ;; this means it was not found.
     ((null index)
      (goto-char (org-element-property :begin (first refs))))
     ;; on last reference, try to jump to next one
     ((= index (- (length refs) 1))
      (when  (re-search-forward "\\[cite" nil t)
	(goto-char (org-element-property :begin (first (org-cite-get-references (org-element-context)))))))
     ;; otherwise jump to the next one
     (t
      (goto-char
       (org-element-property :begin (nth (min (+ index 1) (- (length refs) 1)) refs)))))))


(defun oc-bibtex-previous-reference ()
  "Move point to previous reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position refs current-ref
						(lambda (r1 r2)
						  (= (org-element-property :begin r1)
						     (org-element-property :begin r2)))))))
    (cond
     ;; not found or on style part
     ((or (= index 0) (null index))
      (when (re-search-backward "\\[cite" nil t 2)
	(goto-char (org-element-property :begin (car (last (org-cite-get-references (org-element-context))))))))

     (t
      (goto-char (org-element-property :begin (nth (max (- index 1) 0) refs)))))))


;; * Editing

(defun oc-bibtex-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun oc-bibtex-shift-left ()
  "Shift the reference at point to the left."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (seq-position refs current-ref
			      (lambda (r1 r2)
				(and (string= (org-element-property :key r1)
					      (org-element-property :key r2))
				     (equal (org-element-property :prefix r1)
					    (org-element-property :prefix r2))
				     (equal (org-element-property :suffix r1)
					    (org-element-property :suffix r2)))))))
    (when (= 1 (length refs))
      (error "You only have one reference. You cannot shift this"))
    (setf (buffer-substring (org-element-property :contents-begin current-citation)
			    (org-element-property :contents-end current-citation))
	  (org-element-interpret-data (oc-bibtex-swap index (- index 1) refs)))
    ;; Now get on the original ref.
    (let* ((newrefs (org-cite-get-references current-citation))
	   (index (seq-position newrefs current-ref
				(lambda (r1 r2)
				  (and (string= (org-element-property :key r1)
						(org-element-property :key r2))
				       (equal (org-element-property :prefix r1)
					      (org-element-property :prefix r2))
				       (equal (org-element-property :suffix r1)
					      (org-element-property :suffix r2)))))))
      (unless index (error "nothing found"))
      (goto-char (org-element-property :begin (nth index newrefs))))))


(defun oc-bibtex-shift-right ()
  "Shift the reference at point to the right."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (seq-position refs current-ref
			      (lambda (r1 r2)
				(and (string= (org-element-property :key r1)
					      (org-element-property :key r2))
				     (equal (org-element-property :prefix r1)
					    (org-element-property :prefix r2))
				     (equal (org-element-property :suffix r1)
					    (org-element-property :suffix r2)))))))
    (when (= 1 (length refs))
      (error "You only have one reference. You cannot shift this"))

    ;; Don't go past the end.
    (unless (= index (-  (length refs) 1))
      (setf (buffer-substring (org-element-property :contents-begin current-citation)
			      (org-element-property :contents-end current-citation))
	    (org-element-interpret-data (oc-bibtex-swap index (+ index 1) refs)))
      ;; Now get on the original ref.
      (let* ((newrefs (org-cite-get-references current-citation))
	     (index (seq-position newrefs current-ref
				  (lambda (r1 r2)
				    (and (string= (org-element-property :key r1)
						  (org-element-property :key r2))
					 (equal (org-element-property :prefix r1)
						(org-element-property :prefix r2))
					 (equal (org-element-property :suffix r1)
						(org-element-property :suffix r2)))))))
	(unless index (error "nothing found"))
	(goto-char (org-element-property :begin (nth index newrefs)))))))


(defun oc-bibtex-sort-year-ascending ()
  "Sort the references at point by year (from early to later)."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (cp (point)))

    (setf (buffer-substring (org-element-property :contents-begin current-citation)
			    (org-element-property :contents-end current-citation))
	  (org-element-interpret-data
	   (sort refs (lambda (ref1 ref2)
			(let* ((e1 (bibtex-completion-get-entry (org-element-property :key ref1)))
			       (e2 (bibtex-completion-get-entry (org-element-property :key ref2)))
			       (y1 (string-to-number (or (cdr (assoc "year" e1)) "0")))
			       (y2 (string-to-number (or (cdr (assoc "year" e2)) "0"))))
			  (> y2 y1))))))
    (goto-char cp)))


(defun oc-bibtex-goto-cite-beginning ()
  "Move to the beginning of the citation."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum))))
    (if (= (point) (org-element-property :begin current-citation))
	(org-beginning-of-line)
      (goto-char (org-element-property :begin current-citation)))))


(defun oc-bibtex-goto-cite-end ()
  "Move to the end of the citation.
If at the end, use `org-end-of-line' instead."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum))))
    (if (= (point) (- (org-element-property :end current-citation)
		      (org-element-property :post-blank current-citation)))
	(org-end-of-line)

      (goto-char (- (org-element-property :end current-citation)
		    (org-element-property :post-blank current-citation))))))

(defun oc-bibtex-mark-cite ()
  "Mark the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (set-mark (- (org-element-property :end datum)
		 (or (org-element-property :post-blank datum) 0)))
    (goto-char (org-element-property :begin datum))))


(defun oc-bibtex-kill-cite ()
  "Kill the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (kill-region (org-element-property :begin datum) (org-element-property :end datum))))


(defun oc-bibtex-copy-cite ()
  "Copy the reference/citation at point."
  (interactive)
  (oc-bibtex-mark-cite)
  (call-interactively 'kill-ring-save))

;; * Keymap

(defcustom oc-bibtex-keymap
  (let ((map (copy-keymap org-mouse-map)))
    (define-key map (kbd "C-<right>") 'oc-bibtex-next-reference)
    (define-key map (kbd "C-<left>") 'oc-bibtex-previous-reference)
    (define-key map (kbd "S-<right>") 'oc-bibtex-shift-right)
    (define-key map (kbd "S-<left>") 'oc-bibtex-shift-left)
    (define-key map (kbd "S-<up>") 'oc-bibtex-sort-year-ascending)
    (define-key map (kbd "C-a") 'oc-bibtex-goto-cite-beginning)
    (define-key map (kbd "C-e") 'oc-bibtex-goto-cite-end)
    (define-key map (kbd "C-d") (lambda ()
				  (interactive)
				  (org-cite-delete-citation (org-element-context))))
    (define-key map (kbd "C-k") 'oc-bibtex-kill-cite)
    (define-key map (kbd "M-w") 'oc-bibtex-copy-cite)
    (define-key map (kbd "M-m") 'oc-bibtex-mark-cite)
    (define-key map (kbd "M-s") 'oc-bibtex-update-style)
    (define-key map (kbd "M-p") 'oc-bibtex-update-pre/post)

    (define-key map (kbd "RET") (lambda ()
				  (interactive)
				  (oc-bibtex-follow (org-element-context))))

    map)
  "A keymap for cite objects.")


(defun oc-bibtex-activate (citation)
  "Add a keymap to cites.
Argument CITATION is an org-element holding the references."
  (org-cite-basic-activate citation)
  (pcase-let ((`(,beg . ,end) (org-cite-boundaries citation)))
    (put-text-property beg end 'keymap oc-bibtex-keymap)
    (put-text-property
     beg (1- (org-with-point-at beg (search-forward ":")))
     'help-echo (org-trim (org-export-string-as (buffer-substring beg end) 'latex t)))))




;; * Inserting

(defun oc-bibtex-insert-processor (context arg)
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
   oc-bibtex-alternate-insert-actions)

  (cond
   ;; the usual case where we insert a ref
   ((null arg)
    (bibtex-completion-init)
    (let* ((candidates (bibtex-completion-candidates)))
      (ivy-read "BibTeX entries: " candidates
		:action (lambda (candidate)
			  (oc-bibtex-insert-citation
			   (cdr (assoc "=key=" (cdr candidate))) arg)))))

   ;; Here you are either updating the style, or inserting a new ref with a selected style.
   ((= (prefix-numeric-value  arg) 4)
    (if context
	(oc-bibtex-update-style)
      (bibtex-completion-init)
      (let* ((candidates (bibtex-completion-candidates)))
	(ivy-read "BibTeX entries: " candidates
		  :caller 'org-cite-insert
		  :action '(1
			    ("i" (lambda (candidate)
				   (oc-bibtex-insert-citation
				    (cdr (assoc "=key=" (cdr candidate))) current-prefix-arg)) "insert"))))))

   ;; delete thing at point, either ref or citation
   ((= (prefix-numeric-value  current-prefix-arg) 16)
    (org-cite-delete-citation context))))


(defun oc-bibtex-insert-citation (select-key arg)
  "Insert a citation.
If you are not on a citation, insert one.
If you are the beginning (on @) i3nsert before the current cite
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
		    (concat "/" (completing-read "Style: " (mapcar 'car oc-bibtex-styles)))
		  "/t")
		(concat "@" select-key)))))))


;; * Following
;; These need to be more robust. It is implied you are on a reference, butI don't check this.

(defun oc-bibtex-info ()
  "Show information about the element at point."
  (interactive)
  (message "%S" (org-element-context)))


(defun oc-bibtex-open-pdf ()
  "Open the pdf of the reference at point (if it exists)."
  (interactive)
  (bibtex-completion-open-pdf (list (org-element-property :key (org-element-context)))))


(defun oc-bibtex-open-url-or-doi ()
  "Open the url or doi of a reference if it exists."
  (interactive)
  (bibtex-completion-open-url-or-doi (list (org-element-property :key (org-element-context)))))


(defun oc-bibtex-show-entry ()
  "Open the entry for the reference at point."
  (interactive)
  (bibtex-completion-show-entry (list (org-element-property :key (org-element-context)))))


(defun oc-bibtex-open-notes ()
  "Open the notes for the reference at point."
  (interactive)
  (bibtex-completion-edit-notes (list (org-element-property :key (org-element-context)))))


(defun oc-bibtex-copy-key ()
  "Copy the key at point."
  (interactive)
  (kill-new (org-element-property :key (org-element-context))))


(defun oc-bibtex-copy-bibtex-entry ()
  "Copy the bibtex entry for the reference at point."
  (interactive)
  (save-window-excursion
    (bibtex-completion-show-entry (list (org-element-property :key (org-element-context))))
    (bibtex-beginning-of-entry)
    (bibtex-copy-entry-as-kill)
    (kill-new (pop bibtex-entry-kill-ring))))


(defun oc-bibtex-copy-citation ()
  "Copy the current citation at point."
  (interactive)
  (let ((parent (org-element-property :parent (org-element-context))))
    (kill-new (buffer-substring (org-element-property :begin parent)
				(org-element-property :end parent)))))


;; TODO: I probably need to do what I did here in all these functions. The issue
;; this solves is that `(org-cite-list-bibliography-files)' lists local and
;; global files, and these commands should only use what is defined locally if
;; it is, and globally otherwise. There is a lot of repetition, and maybe an
;; anophoric macro would simplify it?
(defun oc-bibtex-copy-formatted-reference ()
  "Copy a formatted version of the reference at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (kill-new (bibtex-completion-apa-format-reference (org-element-property :key (org-element-context))))))


(defun oc-bibtex-doi (key)
  "Get the DOI associated with the KEY."
  (save-window-excursion
    (bibtex-completion-show-entry (list (org-element-property :key (org-element-context))))
    (bibtex-beginning-of-entry)
    (replace-regexp-in-string "^http\\(s\\)?://dx.doi.org/" "" (bibtex-autokey-get-field "doi"))))


(defun oc-bibtex-wos ()
  "Open the reference at point in WOS."
  (interactive)
  (doi-utils-wos (oc-bibtex-doi (org-element-property :key (org-element-context)))))


(defun oc-bibtex-wos-related ()
  "Open the reference at point to related articles in WOS."
  (interactive)
  (doi-utils-wos-related (oc-bibtex-doi (org-element-property :key (org-element-context)))))


(defun oc-bibtex-wos-citing ()
  "Open the reference at point to citing articles in WOS."
  (interactive)
  (doi-utils-wos-citing (oc-bibtex-doi (org-element-property :key (org-element-context)))))


(defun oc-bibtex-pubmed ()
  "Open the reference at point PubMED."
  (interactive)
  (doi-utils-pubmed (oc-bibtex-doi (org-element-property :key (org-element-context)))))


(defun oc-bibtex-crossref ()
  "Open the reference at point in crossref."
  (interactive)
  (doi-utils-crossref (oc-bibtex-doi (org-element-property :key (org-element-context)))))


(defun oc-bibtex-google-scholar ()
  "Open the reference at point in Google Scholar."
  (interactive)
  (browse-url
   (url-encode-url
    (format
     "http://scholar.google.com/scholar?q=%s"
     (let* ((key (org-element-property :key (org-element-context)))
	    (entry (bibtex-completion-get-entry key)))
       (cdr (assoc "title" entry)))))))


(defun oc-bibtex-email ()
  "Email the reference at point with attached PDFs."
  (interactive)
  (let* ((key (org-element-property :key (org-element-context)))
	 (pdfs (bibtex-completion-find-pdf-in-library key)))
    (save-window-excursion
      (bibtex-completion-show-entry (list key))
      (bibtex-beginning-of-entry)
      (bibtex-copy-entry-as-kill))
    (compose-mail)
    (message-goto-body)
    (insert (pop bibtex-entry-kill-ring))
    (message-goto-subject)
    (insert key)

    (when pdfs
      (cl-loop for pdf in pdfs do (mml-attach-file pdf)))

    (message-goto-to)))


(defun oc-bibtex-ivy-select-style ()
  "Select a style with completion."
  (interactive)
  (ivy-read "Style: " oc-bibtex-styles
	    :caller 'oc-bibtex-ivy-select-style))


(defun oc-bibtex-ivy-style-transformer (candidate)
  "Transform CANDIDATE for selection."
  (format "%-20s%s" candidate
	  (propertize (cdr (assoc candidate oc-bibtex-styles)) 'face  '(:foreground "forest green"))))


(ivy-set-display-transformer 'oc-bibtex-ivy-select-style 'oc-bibtex-ivy-style-transformer)


(defun oc-bibtex-update-style ()
  "Change the style of the citation at point."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (refs (org-cite-get-references current-citation))
	 (style (oc-bibtex-ivy-select-style))
	 (cp (point)))
    (setf (buffer-substring (org-element-property :begin current-citation)
			    (org-element-property :end current-citation))
	  (format "[cite%s:%s]" (if (string= "nil" style)
				    ""
				  (concat "/" style))
		  (org-element-interpret-data refs)))
    (goto-char cp)))


(defun oc-bibtex-delete ()
  "Delete the reference or citation at point."
  (interactive)
  (org-cite-delete-citation (org-element-context)))


(defun oc-bibtex-update-pre/post ()
  "Change the pre/post text of the reference at point."
  (interactive)
  (let* ((datum (org-element-context))
	 (ref (if (eq (org-element-type datum) 'citation-reference)
		  datum
		(error "Not on a citation reference")))
	 (key (org-element-property :key ref))
	 (pre (read-string "Prefix text: " (org-element-property :prefix ref)))
	 (post (read-string "Suffix text: " (org-element-property :suffix ref))))

    (setf (buffer-substring (org-element-property :begin ref)
			    (org-element-property :end ref))
	  (org-element-interpret-data `(citation-reference
					(:key ,key :prefix ,(concat pre " ") :suffix ,(concat  " " post)))))))


(pretty-hydra-define oc-bibtex-citation-reference (:color blue)
  ("Actions"
   (("p" oc-bibtex-open-pdf "open pdf")
    ("b" oc-bibtex-show-entry  "open bibtex")
    ("u" oc-bibtex-open-url-or-doi  "open url")
    ("n" oc-bibtex-open-notes "open notes")
    ("I" oc-bibtex-info "Info"))

   "Edit"
   (("ii" org-cite-insert "Insert")
    ("ib" (lambda ()
	    (interactive)
	    (let ((context (org-element-context)))
	      (unless (eq (point) (org-element-property :begin context))
		(oc-bibtex-previous-reference)))
	    (org-cite-insert nil))  "Insert before")
    ("ia" (lambda ()
	    (interactive)
	    (oc-bibtex-next-reference)
	    (org-cite-insert nil))  "Insert after")
    ("s" oc-bibtex-update-style "Change style")
    ("P" oc-bibtex-update-pre/post "Update pre/post")
    ("d" oc-bibtex-delete "Delete key/citation" :color red)
    ("y" oc-bibtex-sort-year-ascending "Sort year"))

   "Navigation"
   (("j" oc-bibtex-previous-reference "Previous reference" :color red)
    ("k" oc-bibtex-next-reference "Next reference" :color red))

   "Copy"
   (("ck" oc-bibtex-copy-key "Copy key")
    ("cc" oc-bibtex-copy-citation)
    ("cf" oc-bibtex-copy-formatted-reference "Copy formatted entry")
    ("ce" oc-bibtex-copy-bibtex-entry "Copy bibtex entry"))

   "WWW"
   (("ww" oc-bibtex-wos "WOS")
    ("wr" oc-bibtex-wos-related "WOS related")
    ("wc" oc-bibtex-wos-citing "WOS citing")
    ("wg" oc-bibtex-google-scholar "Google Scholar")
    ("wc" oc-bibtex-crossref "Crossref")
    ("wp" oc-bibtex-pubmed "Pubmed"))

   "Misc"
   (("e" oc-bibtex-email "Email entry"))))


(defun oc-bibtex-follow (&optional datum _)
  "Follow function consistent with the org-cite API.
Optional argument DATUM The element at point."
  (interactive)
  (if (eq 'citation-reference (org-element-type datum))
      (oc-bibtex-citation-reference/body)
    ;; at style part.
    (let* ((refs (org-cite-get-references datum))
	   (keys (mapcar (lambda (ref) (org-element-property :key ref)) refs))
	   (key (completing-read "Key: " keys)))
      (search-forward (concat "@" key))
      (goto-char (match-beginning 0))
      (oc-bibtex-citation-reference/body))))

;; * Exporting

(defun org-cite-bibtex--build-optional-arguments (citation info)
  "Build optional arguments for citation command.
CITATION is the citation object.  INFO is the export state, as a property list."
  (let* ((origin (pcase (org-cite-get-references citation)
                   (`(,reference) reference)
                   (_ citation)))
         (suffix (org-element-property :suffix origin))
         (prefix (org-element-property :prefix origin)))
    (concat (and prefix (format "[%s]" (org-trim (org-export-data prefix info))))
            (cond
             (suffix (format "[%s]" (org-trim (org-export-data suffix info))))
             (prefix "[]")
             (t nil)))))

(defun org-cite-bibtex--build-arguments (citation)
  "Build arguments for citation command for CITATION object.
These are the cite keys"
  (format "{%s}"
          (mapconcat #'identity
                     (org-cite-get-references citation t)
                     ",")))


(defun org-cite-bibtex-export-citation (citation style _ info)
  "Export CITATION object.
STYLE is the citation style, as a pair of strings or nil.  INFO is the export
state, as a property list."
  (concat (cdr (assoc (car style) oc-bibtex-styles))
	  (org-cite-bibtex--build-optional-arguments citation info)
	  (org-cite-bibtex--build-arguments citation)))


(defun org-cite-bibtex-use-package (output &rest _)
  "Ensure output requires \"natbib\" package.
OUTPUT is the final output of the export process."
  (with-temp-buffer
    (save-excursion (insert output))
    (when (search-forward "\\begin{document}" nil t)
      ;; Ensure there is a \usepackage{natbib} somewhere or add one.
      (goto-char (match-beginning 0))
      (let ((re (rx "\\usepackage" (opt "[" (*? nonl) "]") "{natbib}")))
        (unless (re-search-backward re nil t)
          (insert
           (format "\\usepackage%s{natbib}\n"
                   (if (null natbib-options)
		       ""
		     (format "[%s]" natbib-options)))))))
    (buffer-string)))


(defun org-cite-bibtex-export-bibliography (_keys files &rest _)
  "Print references from bibliography FILES.
FILES is a list of absolute file names.  STYLE is the bibliography style, as
a string or nil.

The actual bibliography command is determined by the
PRINT_BIBLIOGRAPHY keyword. If it contains a non-nil value for
:nobibliography then the command is \\nobibliography otherwise it
is \\bibliography.

You can use a :title option to set the title of the bibliography. The default is Bibliography.
You can use a :numbered option to set if the Bibliography section should be numbered. The default is not numbered.
"
  (let* ((bibtitle (or (plist-get (org-export-read-attribute
				   :attr
				   `(nil (:attr (,(cadr (assoc "PRINT_BIBLIOGRAPHY"
							       (org-collect-keywords
								'("PRINT_BIBLIOGRAPHY"))))))))
				  :title)))
	 (numbered (plist-get (org-export-read-attribute
			       :attr
			       `(nil (:attr (,(cadr (assoc "PRINT_BIBLIOGRAPHY"
							   (org-collect-keywords
							    '("PRINT_BIBLIOGRAPHY"))))))))
			      :numbered))

	 (bibcmd (if  (plist-get (org-export-read-attribute
				  :attr
				  `(nil (:attr (,(cadr (assoc "PRINT_BIBLIOGRAPHY"
							      (org-collect-keywords
							       '("PRINT_BIBLIOGRAPHY"))))))))
				 :nobibliography)
		     "nobibliography"
		   "bibliography"))
	 (style (cadr (assoc "BIBLIOGRAPHYSTYLE"
			     (org-collect-keywords '("BIBLIOGRAPHYSTYLE"))))))

    (when (and (string= "nobibliography" bibcmd)
	       (or bibtitle numbered))
      (error "You cannot combine nobibliography and title/numbered yet."))

    (concat
     (and style (format "\\bibliographystyle{%s}\n" style))
     (and bibtitle (format "\\renewcommand{\\bibsection}{\\section%s{%s}}\n"
			   (if numbered  "" "*")
			   (org-strip-quotes bibtitle)))
     (format "\\%s{%s}"
	     bibcmd
             (mapconcat #'file-name-sans-extension
			(mapcar #'expand-file-name files)
			",")))))



;;; Register `bibtex' processor
(org-cite-register-processor 'bibtex
  :activate #'oc-bibtex-activate
  :follow #'oc-bibtex-follow
  :insert #'oc-bibtex-insert-processor
  :export-bibliography #'org-cite-bibtex-export-bibliography
  :export-citation #'org-cite-bibtex-export-citation
  :export-finalizer #'org-cite-bibtex-use-package
  :cite-styles (mapcar 'car oc-bibtex-styles))

(provide 'org-cite-bibtex)
(provide 'oc-bibtex)
;;; oc-bibtex.el ends here
