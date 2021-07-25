;;; org-ref-cite-advice.el --- advice functions for `org-ref-cite'
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
;; This library advises org-cite functions that don't act like I think they should.

;;; Code:

;; there is an alternative to put #+org-cite-global-bibliography: nil in the
;; file but I don't like it. I think this is more consistent with using local
;; properties to supercede higher level properties.

(defun org-ref-cite-list-bibliography-files ()
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

(advice-add 'org-cite-list-bibliography-files :override 'org-ref-cite-list-bibliography-files)



;; * Flyspell setup

;; keys are often misspelled, so we try to turn that off here so they don't get
;; flagged by flyspell.. I don't know if we can be smart about prefix/suffix
;; text

;; I don't understand why I have to do this, but it is the only reliable way I
;; have gotten flyspell to work. I don't know if we can get spell-checking on
;; the prefix/suffix with this approach though.

(defun org-ref-cite--flyspell-object-check-p (element)
  "Non-nil when Flyspell can check object at point.
ELEMENT is the element at point."
  (let ((object (save-excursion
		  (when (looking-at-p "\\>") (backward-char))
		  (org-element-context element))))
    (cl-case (org-element-type object)
      ;; Prevent checks in links due to keybinding conflict with
      ;; Flyspell.
      ((code entity export-snippet inline-babel-call
	     inline-src-block line-break latex-fragment link macro
	     statistics-cookie target timestamp verbatim
	     ;; add these for org-ref-cite
	     citation citation-reference)
       nil)
      (footnote-reference
       ;; Only in inline footnotes, within the definition.
       (and (eq (org-element-property :type object) 'inline)
	    (< (save-excursion
		 (goto-char (org-element-property :begin object))
		 (search-forward ":" nil t 2))
	       (point))))
      (otherwise t))))

(advice-add 'org--flyspell-object-check-p :override 'org-ref-cite--flyspell-object-check-p)
;; (advice-remove 'org--flyspell-object-check-p 'org-ref-cite--flyspell-object-check-p)


;; This does not reliably do what I want, and I don't understand why. Maybe org
;; is doing some check elsewhere and not just relying on the predicate function.
;; (defun org-ref-cite-flyspell-predicate ()
;;   "Predicate function to ignore flyspell on citations."
;;   (interactive)
;;   (and (not (memq (org-element-type (org-element-context)) '(citation citation-reference)))
;;        (org-mode-flyspell-verify)))


;; (put 'org-mode 'flyspell-mode-predicate 'org-ref-cite-flyspell-predicate)
;; (get 'org-mode 'flyspell-mode-predicate)

(provide 'org-ref-cite-advice)

;;; org-ref-cite-advice.el ends here
