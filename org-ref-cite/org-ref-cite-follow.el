;;; org-ref-cite-follow.el --- org-cite follow processor
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
;;
;;; Code:


(require 'bibtex-completion)
(require 'biblio)
(require 'hydra)
(require 'org-ref-cite-core)

;; * Following

(defun org-ref-cite-copy-formatted-reference ()
  "Copy a formatted version of the reference at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (kill-new (bibtex-completion-apa-format-reference (org-element-property
						       :key (org-element-context))))))


(defun org-ref-cite-info ()
  "Show information about the element at point."
  (interactive)
  (message "%S" (org-element-context)))


(defun org-ref-cite-open-pdf ()
  "Open the pdf of the reference at point (if it exists)."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (bibtex-completion-open-pdf (list (org-element-property
				       :key (org-element-context))))))


(defun org-ref-cite-open-url-or-doi ()
  "Open the url or doi of a reference if it exists."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (bibtex-completion-open-url-or-doi (list (org-element-property
					      :key (org-element-context))))))


(defun org-ref-cite-show-entry ()
  "Open the entry for the reference at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (bibtex-completion-show-entry (list (org-element-property
					 :key (org-element-context))))))


(defun org-ref-cite-open-notes ()
  "Open the notes for the reference at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (bibtex-completion-edit-notes (list (org-element-property
					 :key (org-element-context))))))


(defun org-ref-cite-open-notes-other-frame ()
  "Open the notes for the reference at point in another frame."
  (interactive)
  (with-selected-frame (make-frame)
    (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
      (bibtex-completion-edit-notes (list (org-element-property
					   :key (org-element-context)))))))


(defun org-ref-cite-copy-key ()
  "Copy the key at point."
  (interactive)
  (kill-new (org-element-property :key (org-element-context))))


(defun org-ref-cite-copy-bibtex-entry ()
  "Copy the bibtex entry for the reference at point."
  (interactive)
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (save-window-excursion
      (bibtex-completion-show-entry (list (org-element-property
					   :key
					   (org-element-context))))
      (bibtex-beginning-of-entry)
      (bibtex-copy-entry-as-kill)
      (kill-new (pop bibtex-entry-kill-ring)))))


(defun org-ref-cite-copy-citation ()
  "Copy the current citation at point."
  (interactive)
  (let ((parent (org-element-property :parent (org-element-context))))
    (kill-new (buffer-substring (org-element-property :begin parent)
				(org-element-property :end parent)))))


(defun org-ref-cite-doi (key)
  "Get the DOI associated with the KEY."
  (let ((bibtex-completion-bibliography (org-cite-list-bibliography-files)))
    (save-window-excursion
      (bibtex-completion-show-entry (list (org-element-property
					   :key (org-element-context))))
      (bibtex-beginning-of-entry)
      (replace-regexp-in-string "^http\\(s\\)?://dx.doi.org/" ""
				(bibtex-autokey-get-field "doi")))))


(defun org-ref-cite-wos ()
  "Open the reference at point in WOS."
  (interactive)
  (browse-url
   (format
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info:doi/%s"
    (org-ref-cite-doi (org-element-property :key (org-element-context))))))


(defun org-ref-cite-wos-related ()
  "Open the reference at point to related articles in WOS."
  (interactive)
  (browse-url
   (concat "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
           (org-ref-cite-doi (org-element-property :key (org-element-context)))
           "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.related=yes")))


(defun org-ref-cite-wos-citing ()
  "Open the reference at point to citing articles in WOS."
  (interactive)
  (browse-url
   (concat
    "http://ws.isiknowledge.com/cps/openurl/service?url_ver=Z39.88-2004&rft_id=info%3Adoi%2F"
    (org-ref-cite-doi (org-element-property :key (org-element-context)))
    "&svc_val_fmt=info%3Aofi%2Ffmt%3Akev%3Amtx%3Asch_svc&svc.citing=yes")))


(defun org-ref-cite-pubmed ()
  "Open the reference at point PubMED."
  (interactive)
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (url-hexify-string (org-ref-cite-doi (org-element-property :key (org-element-context)))))))


(defun org-ref-cite-crossref ()
  "Open the reference at point in crossref."
  (interactive)
  (browse-url
   (format
    "http://search.crossref.org/?q=%s"
    (org-ref-cite-doi (org-element-property :key (org-element-context))))))


(defun org-ref-cite-google-scholar ()
  "Open the reference at point in Google Scholar."
  (interactive)
  (browse-url
   (url-encode-url
    (format
     "http://scholar.google.com/scholar?q=%s"
     (let* ((bibtex-completion-bibliography (org-cite-list-bibliography-files))
	    (key (org-element-property :key (org-element-context)))
	    (entry (bibtex-completion-get-entry key)))
       (cdr (assoc "title" entry)))))))


(defun org-ref-cite-biblio-lookup ()
  "Lookup with biblio on title."
  (interactive)
  (let ((backend (biblio--read-backend)))
    (biblio-lookup backend (let* ((bibtex-completion-bibliography (org-cite-list-bibliography-files))
				  (key (org-element-property :key (org-element-context)))
				  (entry (bibtex-completion-get-entry key)))
			     (cdr (assoc "title" entry))))))


(defun org-ref-cite-email ()
  "Email the reference at point with attached PDFs."
  (interactive)
  (let* ((bibtex-completion-bibliography (org-cite-list-bibliography-files))
	 (key (org-element-property :key (org-element-context)))
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





;; * Follow menu in hydra

(pretty-hydra-define org-ref-cite-citation-reference (:color blue)
  ("Actions:"
   (("p" org-ref-cite-open-pdf "open pdf")
    ("b" org-ref-cite-show-entry  "open bibtex")
    ("u" org-ref-cite-open-url-or-doi  "open url")
    ("n" org-ref-cite-open-notes "open notes")
    ("N" org-ref-cite-open-notes-other-frame "open notes in other frame")
    ("I" org-ref-cite-info "Info"))

   "Edit"
   (("ii" org-cite-insert "Insert")
    ("ib" (lambda ()
	    (interactive)
	    (let ((context (org-element-context)))
	      (unless (eq (point) (org-element-property :begin context))
		(org-ref-cite-previous-reference)))
	    (org-cite-insert nil))  "Insert before")
    ("ia" (lambda ()
	    (interactive)
	    (org-ref-cite-next-reference)
	    (org-cite-insert nil))  "Insert after")
    ("s" org-ref-cite-update-style "Change style")
    ("P" org-ref-cite-update-pre/post "Update pre/post")
    ("d" org-ref-cite-delete "Delete key/citation" :color red)
    ("y" org-ref-cite-sort-year-ascending "Sort year")
    ("r" org-ref-cite-replace-key-with-suggestions "Replace key with suggestions"))

   "Navigation"
   (("j" org-ref-cite-previous-reference "Previous reference" :color red)
    ("k" org-ref-cite-next-reference "Next reference" :color red))

   "Copy"
   (("ck" org-ref-cite-copy-key "Copy key")
    ("cc" org-ref-cite-copy-citation)
    ("cf" org-ref-cite-copy-formatted-reference "Copy formatted entry")
    ("ce" org-ref-cite-copy-bibtex-entry "Copy bibtex entry"))

   "WWW"
   (("ww" org-ref-cite-wos "WOS")
    ("wr" org-ref-cite-wos-related "WOS related")
    ("wc" org-ref-cite-wos-citing "WOS citing")
    ("wg" org-ref-cite-google-scholar "Google Scholar")
    ("wc" org-ref-cite-crossref "Crossref")
    ("wp" org-ref-cite-pubmed "Pubmed"))

   "Misc"
   (("e" org-ref-cite-email "Email entry"))))



(defun org-ref-cite-follow (&optional datum _)
  "Follow function consistent with the org-cite API.
Optional argument DATUM: The element at point.
If you follow on the style part you will be prompted for a key to act on."
  (interactive)
  (when (null datum) (setq datum (org-element-context)))
  (if (eq 'citation-reference (org-element-type datum))
      (org-ref-cite-citation-reference/body)
    ;; at style part or end part
    (if (= (point) (org-element-property :end datum))
	(org-return)
      (let* ((refs (org-cite-get-references datum))
	     (keys (mapcar (lambda (ref) (org-element-property :key ref)) refs))
	     (key (completing-read "Key: " keys)))
	(search-forward (concat "@" key))
	(goto-char (match-beginning 0))
	(org-ref-cite-citation-reference/body)))))


;; If this is integrated into org-mode I will remove this.
(defun org-ref-cite-Cc-Cc ()
  "Function to follow a cite with C-c C-c."
  (interactive)
  (let ((context (org-element-context)))
    (when (member (org-element-type context) '(citation citation-reference))
      (org-ref-cite-follow context)
      t)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'org-ref-cite-Cc-Cc)

(provide 'org-ref-cite-follow)

;;; org-ref-cite-follow.el ends here
