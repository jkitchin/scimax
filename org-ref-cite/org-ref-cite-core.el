;;; org-ref-cite-core.el --- Core functions
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

;;; Code:
(require 'oc)
(require 'oc-basic)
(require 'bibtex-completion)

;; org-cite uses (style . option) for styles, but that is more complicated than
;; I need. I use simple strings here.
(defcustom org-ref-cite-styles
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
author."
  :group 'org-ref-cite)


(defcustom org-ref-cite-default-citation-command "\\citet"
  "Default command for citations."
  :group 'org-ref-cite)


;; * Style

(defun org-ref-cite--style-to-command (style)
  "Return command name to use according to STYLE.
Defaults to `org-ref-cite-default-citation-command' if STYLE is not found"
  (or (cdr (assoc style org-ref-cite-styles)) org-ref-cite-default-citation-command))


(defun org-ref-cite-annotate-style (s)
  "Annotation function for selecting style.
Argument S The candidate string."
  (let* ((w (+  (- 5 (length s)) 20)))
    (concat (make-string w ? )
	    (propertize
	     (cdr (assoc s org-ref-cite-styles))
	     'face '(:foreground "forest green")))))


(defun org-ref-cite-select-style ()
  "Select a style with completion."
  (interactive)
  (let ((completion-extra-properties '(:annotation-function  org-ref-cite-annotate-style)))
    (completing-read "Style: " org-ref-cite-styles)))


(defun org-ref-cite-update-style ()
  "Change the style of the citation at point."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (refs (org-cite-get-references current-citation))
	 (style (org-ref-cite-select-style))
	 (cp (point)))
    (setf (buffer-substring (org-element-property :begin current-citation)
			    (org-element-property :end current-citation))
	  (format "[cite%s:%s]" (if (string= "nil" style)
				    ""
				  (concat "/" style))
		  (org-element-interpret-data refs)))
    (goto-char cp)))



;; * Navigation functions
;; There can be some subtle failures when there are duplicate keys sometimes.
(defun org-ref-cite-next-reference ()
  "Move point to the next reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum))
			datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position
				   refs current-ref
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
	(goto-char
	 (org-element-property :begin (first (org-cite-get-references
					      (org-element-context)))))))
     ;; otherwise jump to the next one
     (t
      (goto-char
       (org-element-property :begin (nth (min (+ index 1) (- (length refs) 1)) refs)))))))


(defun org-ref-cite-previous-reference ()
  "Move point to previous reference."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum)))
	 (current-ref (when (eq 'citation-reference (org-element-type datum)) datum))
	 (refs (org-cite-get-references current-citation))
	 (index (when current-ref (seq-position
				   refs current-ref
				   (lambda (r1 r2)
				     (= (org-element-property :begin r1)
					(org-element-property :begin r2)))))))
    (cond
     ;; not found or on style part
     ((or (= index 0) (null index))
      (when (re-search-backward "\\[cite" nil t 2)
	(goto-char (org-element-property
		    :begin
		    (car (last (org-cite-get-references (org-element-context))))))))

     (t
      (goto-char (org-element-property :begin (nth (max (- index 1) 0) refs)))))))


(defun org-ref-cite-goto-cite-beginning ()
  "Move to the beginning of the citation."
  (interactive)
  (let* ((datum (org-element-context))
	 (current-citation (if (eq 'citation (org-element-type datum)) datum
			     (org-element-property :parent datum))))
    (if (= (point) (org-element-property :begin current-citation))
	(org-beginning-of-line)
      (goto-char (org-element-property :begin current-citation)))))


(defun org-ref-cite-goto-cite-end ()
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


;; * Editing

(defun org-ref-cite-swap (i j lst)
  "Swap index I and J in the list LST."
  (let ((tempi (nth i lst)))
    (setf (nth i lst) (nth j lst))
    (setf (nth j lst) tempi))
  lst)


(defun org-ref-cite-shift-left ()
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
    (when (null index)
      (error "Nothing to shift here"))
    (setf (buffer-substring (org-element-property :contents-begin current-citation)
			    (org-element-property :contents-end current-citation))
	  (org-element-interpret-data (org-ref-cite-swap index (- index 1) refs)))
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

      (goto-char (org-element-property :begin (nth index newrefs))))))


(defun org-ref-cite-shift-right ()
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

    (when (null index)
      (error "Nothing to shift here"))

    ;; Don't go past the end.
    (unless (= index (-  (length refs) 1))
      (setf (buffer-substring (org-element-property :contents-begin current-citation)
			      (org-element-property :contents-end current-citation))
	    (org-element-interpret-data (org-ref-cite-swap index (+ index 1) refs)))
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
	(unless index (error "Nothing found"))
	(goto-char (org-element-property :begin (nth index newrefs)))))))


(defun org-ref-cite-sort-year-ascending ()
  "Sort the references at point by year (from earlier to later)."
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
			(let* ((e1 (bibtex-completion-get-entry
				    (org-element-property :key ref1)))
			       (e2 (bibtex-completion-get-entry
				    (org-element-property :key ref2)))
			       (y1 (string-to-number (or (cdr (assoc "year" e1)) "0")))
			       (y2 (string-to-number (or (cdr (assoc "year" e2)) "0"))))
			  (> y2 y1))))))
    (goto-char cp)))


(defun org-ref-cite-delete ()
  "Delete the reference or citation at point."
  (interactive)
  (org-cite-delete-citation (org-element-context)))


(defun org-ref-cite-update-pre/post ()
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
					(:key ,key :prefix ,(concat pre " ")
					      :suffix ,(concat  " " post)))))))


(defun org-ref-cite-kill-cite ()
  "Kill the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (kill-region (org-element-property :begin datum) (org-element-property :end datum))))


(defun org-ref-cite-replace-key-with-suggestions ()
  "Replace key at point with suggestions.
This is intended for use in fixing bad keys, but would work for similar keys."
  (interactive)
  (let* ((datum (org-element-context))
	 (prefix (org-element-property :prefix datum))
	 (suffix (org-element-property :suffix datum))
	 (beg (org-element-property :begin datum))
	 (end (org-element-property :end datum))
	 (key (org-element-property :key datum))
	 (bibtex-completion-bibliography (org-cite-list-bibliography-files))
	 (keys (cl-loop for cand in (bibtex-completion-candidates) collect
			(cdr (assoc "=key=" (cdr cand)))))
	 (suggestions (org-cite-basic--close-keys key keys))
	 (choice (completing-read "Replace with: " suggestions))
	 (cp (point)))
    (setf (buffer-substring beg end)
	  (org-element-interpret-data
	   `(citation-reference (:key ,choice :prefix ,prefix :suffix ,suffix))))
    (goto-char cp)))


;; * miscellaneous utilities

(defun org-ref-cite-copy-cite ()
  "Copy the reference/citation at point."
  (interactive)
  (org-ref-cite-mark-cite)
  (call-interactively 'kill-ring-save))


(defun org-ref-cite-mark-cite ()
  "Mark the reference/citation at point."
  (interactive)
  (let* ((datum (org-element-context)))
    (set-mark (- (org-element-property :end datum)
		 (or (org-element-property :post-blank datum) 0)))
    (goto-char (org-element-property :begin datum))))


(provide 'org-ref-cite-core)

;;; org-ref-cite-core.el ends here
