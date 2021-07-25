;;; org-ref-cite-compat.el --- Utilities for org-ref-cite
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

;;; Commentary:
;; Provides a function to convert org-ref cite links to orc-citations.

;;; Code:

(require 'org-ref)

;; * Compatibility functions

(defun org-ref-to-org-cite ()
  "Convert `org-ref' links to `org-cite' syntax in the current buffer."
  (interactive)
  (let ((cites (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (lnk)
			    (when (member (org-element-property :type lnk)
					  org-ref-cite-types)
			      lnk))))))
    (cl-loop for cite in cites do
	     (setf (buffer-substring (org-element-property :begin cite)
				     (org-element-property :end cite))
		   (let* ((type (org-element-property :type cite))
			  (style (or (car (rassoc (concat "\\" type) oc-bibtex-styles)) "t"))
			  (keys (split-string (org-element-property :path cite) ","))
			  (desc (when (org-element-property :contents-begin cite)
				  (buffer-substring-no-properties
				   (org-element-property :contents-begin cite)
				   (org-element-property :contents-end cite))))
			  (pre-post (when desc (split-string desc "::")))
			  (pre (car pre-post))
			  (post (or (second pre-post) "")))
		     (if pre-post
			 ;; we only have a description on single keys
			 (format "[cite/%s:%s]" style
				 (org-element-interpret-data `(citation-reference
							       (:key ,(first keys)
								     :prefix ,(concat pre " ")
								     :suffix ,(concat  " " post)))))
		       ;; these are multiple keys
		       (format "[cite/%s:%s]" style
			       (org-element-interpret-data
				(cl-loop for key in keys collect
					 `(citation-reference
					   (:key ,key)))))))))))

(provide 'org-ref-cite-compat)

;;; org-ref-cite-compat.el ends here
