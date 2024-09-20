;;; org-clone.el --- library to create an org-zip archive

;; Copyright(C) 2016 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; This file is not currently part of GNU Emacs.

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
;; This library provides code to make standalone org-files in new directories
;; and zip-files. Any files that are linked to in the org-file will be copied to
;; the new directory, and given unique names. the links in the standalone file
;; will be updated to point to them.



;;;###autoload
(defun org-clone-standalone (rebase-directory)
  "Rebase the current org-file to DIRECTORY.
This means create the REBASE-DIRECTORY if needed, and copy the
org-file to it. Also, copy any files the org-file links to into
the directory, and the bibliography file."
  (interactive
   (list (read-directory-name
	  "Directory: " "."
	  (replace-regexp-in-string " " "-" (file-name-base (buffer-file-name))))))

  ;; Make sure destination exists
  (unless (file-directory-p rebase-directory)
    (make-directory rebase-directory t))

  ;; Analyse links. Store (md5 name begin end replacement)
  (let ((link-list '())
	(org-content (buffer-string))
	(org-file (file-name-nondirectory (buffer-file-name)))
	;; we use these later to see if a bibliography will be needed. That is
	;; true if there is a bibliography and/or cite link.
	cite-p bib-p)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
	(let* ((type (org-element-property :type link))
	       (path (org-element-property :path link))
	       (fname (car (last (split-string path "/"))))
	       (temporary-file-directory rebase-directory)
	       new-file md5-hash)
	  (cond
	   ((member type org-ref-cite-types)
	    ;; remember if a cite is in here
	    (setq cite-p t))
	   ((string= type "bibliography")
	    (setq bib-p t)
	    (push (list
		   path
		   path
		   (org-element-property :begin link)
		   (- (org-element-property :end link)
		      (org-element-property :post-blank link))
		   "bibliography:references.bib")
		  link-list))
	   ((string= type "file")
	    (if (file-directory-p path)
		(push (list
		       path
		       path
		       (org-element-property :begin link)
		       (- (org-element-property :end link)
			  (org-element-property :post-blank link))
		       (format "[[%s]%s" path
			       (if (org-element-property :contents-begin link)
				   (format "[%s - directory not copied]]"
					   (buffer-substring
					    (org-element-property :contents-begin link)
					    (org-element-property :contents-end link)))
				 "]")))
		      link-list)
	      ;; else a regular file link, i.e. not a directory
	      (setq md5-hash (with-temp-buffer
			       (insert-file-contents path)
			       (md5 (current-buffer))))

	      ;; only copy if we haven't copied the file before.
	      (if (assoc md5-hash link-list)
		  ;; already copied this one, so we do not need to again.
		  (push (list
			 md5-hash
			 (nth 1 (assoc md5-hash link-list))
			 (org-element-property :begin link)
			 (- (org-element-property :end link)
			    (org-element-property :post-blank link))
			 (format "[[file:%s]%s"
				 (file-name-nondirectory (nth 1 (assoc md5-hash link-list)))
				 (if (org-element-property :contents-begin link)
				     (format "[%s]]" (buffer-substring
						      (org-element-property :contents-begin link)
						      (org-element-property :contents-end link)))
				   "]")))
			link-list)
		;; copy the file, then add the link. we just add the hash to the
		;; end of the file. That should ensure uniqueness in every sense
		;; that matters.
		(setq new-file (expand-file-name
				(concat (file-name-sans-extension fname)
					"-"
					md5-hash
					"." (file-name-extension fname))
				rebase-directory))
		;; this is how I copy it.
		(with-temp-file new-file
		  (insert-file-contents path))
		(push (list
		       md5-hash
		       new-file
		       (org-element-property :begin link)
		       (- (org-element-property :end link)
			  (org-element-property :post-blank link))
		       (format "[[file:%s]%s"
			       (file-name-nondirectory new-file)
			       (if (org-element-property :contents-begin link)
				   (format "[%s]]" (buffer-substring
						    (org-element-property :contents-begin link)
						    (org-element-property :contents-end link)))
				 "]")))
		      link-list))))))))

    ;; Now I need a copy of the org-file, and to replace the links in it. The
    ;; links are in reverse order and that is how we replace them so the points
    ;; do not get messed up as we go from bottom to top.
    (with-temp-file (expand-file-name org-file rebase-directory)
      (insert (with-temp-buffer
		(insert org-content)
		(cl-loop for link in link-list
			 do
			 (cl--set-buffer-substring
			  (nth 2 link) (nth 3 link)
			  (nth 4 link)))
		;; add bibliography references if they exist. if there is a
		;; citation, and not a bibliography link we add one. If there is
		;; a bibliography link, it should get inserted automatically.
		(when cite-p (and (not bib-p))
		      (insert "\nbibliography:references.bib"))
		(buffer-string))))

    ;; Finally we create a bibliography file if we need one.
    (when (or cite-p bib-p)
      (let ((bibtex-files (org-ref-find-bibliography))
	    (bibtex-entry-kill-ring '()))

	(save-window-excursion
	  (cl-loop for key in (reverse (org-ref-get-bibtex-keys))
		   do
		   (bibtex-search-entry key t)
		   (bibtex-kill-entry t)))

	(with-temp-file (expand-file-name "references.bib" rebase-directory)
	  (insert (mapconcat
		   'identity
		   bibtex-entry-kill-ring
		   "\n\n")))))
    rebase-directory))

;;;###autoload
(defun org-create-standalone-zip ()
  "Generate a standalone zip file from the current org-file.
The zip file-name is the same as the org-file, with a zip
extension, e.g. report.org will go to report.zip. All files
referenced in the org-file will be copied to the zip file, and
the links updated to point to them. A bibliography will be
created for the org-file if needed.

Returns the zip filename."
  (interactive)
  (let* ((rebase-directory (expand-file-name
			    (org-clone-standalone
			     (replace-regexp-in-string
			      " " "-"
			      (file-name-base (buffer-file-name))))))
	 (default-directory rebase-directory)
	 (zip-file (format "%s.zip" rebase-directory)))
    (when (file-exists-p zip-file)
      (delete-file zip-file))
    (shell-command (format "zip -v -r %s *" zip-file))
    zip-file))


;;;###autoload
(defun org-create-zip-and-mail ()
  "create a zip file from current org-file and attach it to an email"
  (interactive)
  (let ((zip-file (org-create-standalone-zip)))
    (message-mail)
    (mml-attach-file zip-file)
    (message-goto-to)))


(provide 'org-clone)
