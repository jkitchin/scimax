;;; ox-archive.el --- library to create an org-zip archive

;; Copyright(C) 2014 John Kitchin

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
;; This library provides code to convert an org-file into a nearly
;; standalone zip file. Any files linked to in the org-file will be
;; copied into the zip file, and the link to the file modified to
;; point to the new file.
;;
;; (ox-archive-create-zip) will create a zip file with the basename of
;; the org-file and the tag provided.
;;
;; (ox-archive-create-and-mail) creates the zip file and attaches it
;; to an email.
;;
;; This is very slow on windows. I am not sure why. It is blazing fast
;; on Linux.

(defun ox-archive-create-zip (&optional zip-base-name)
  "create a zip archive from the current org-file. You provide the basename for the zip file (do not include the extension).

All references to files will be copied to the archive. Directories are not copied."
  (interactive "sZip basename (no extension): ")
  (let* ((org-file (buffer-name))
	 (org-file-abs-path (buffer-file-name))
	 (base-name (file-name-sans-extension org-file))
	 ;; directory to save all exports in, using the current date
	 (org-archive
	  (if (string= zip-base-name "")
	      (concat base-name
		      "-"
		      (format-time-string "%Y-%m-%d" (current-time)))
	    zip-base-name))
	 ;; Name of the zip file we will use.
	 (org-archive-zip (concat org-archive ".zip"))
	 link-list)

    ;; delete zip file if it exists
    (when (file-exists-p org-archive-zip)
      (delete-file org-archive-zip))

    ;; delete the directory if it exists
    (when (file-exists-p org-archive)
      (delete-directory org-archive t))

    ;; make directory
    (make-directory org-archive t)

    ;; get list of links, copy files and save names
    (setq link-list
	  (let ((parsetree (org-element-parse-buffer))
		(counter 0))
	    (org-element-map parsetree 'link
	      (lambda (link)
		(let* ((type (nth 0 link))
		       (plist (nth 1 link))
		       (content (nth 2 link))
		       (path (plist-get plist :path))
		       (type (plist-get plist ':type))
		       (fname (car (last (split-string path "/"))))
		       (temporary-file-directory org-archive)
		       (new-file)
		       )
		  (message (format "path:%s is-dir:%s" path (file-directory-p path)))
		  (message (format "type:%s content:%s" type content))
		  (cond
		   ;; regular file with content
		   ((and (string= type "file")  content)
		    ;; replace links to directories

		    (if (file-directory-p path)
			(progn
			  (let ((tmpdir (make-temp-file (file-name-directory path) t "-test")))
			    (copy-directory path tmpdir t nil nil)
			    (format "[[./%s/%s][%s]]" (file-name-nondirectory tmpdir) fname content)))
		      ;; else a regular file
		      (setq new-file  (make-temp-file (file-name-sans-extension fname) nil
						      (concat "." (file-name-extension fname))))
		      (with-temp-file new-file
			(insert-file-contents path))
		      (format "[[./%s][%s]] " (file-name-nondirectory new-file) content)))

		   ;; regular file with no content
		   ((and (string= type "file"))
		    (if (file-directory-p path)
			(progn
			  (let ((tmpdir (make-temp-file (file-name-directory path) t "-test")))
			    (copy-directory path tmpdir t nil nil)
			    (format "[[./%s/%s]]" (file-name-nondirectory tmpdir) fname)))
		      ;; else a regular file
		      (setq new-file  (make-temp-file (file-name-sans-extension fname) nil
						      (concat "." (file-name-extension fname))))
		      (with-temp-file new-file
			(insert-file-contents path))
		      (format "[[./%s]] " (file-name-nondirectory new-file))))
		   ;; We do not modify any other links
		   (t "nil")))))))
    (message (format "\n\nlink-list: %s\n\n" link-list))

    ;; create filter for links and export org buffer. We have a list
    ;; of links, we count each link as it is processed so we can get
    ;; the nth replacement text if needed.
    (let ((counter 0))
      (defun ox-mrkup-filter-link (text back-end info)
	(message "handing link %s: %s\n" counter text)
	;; get the nth link from our list
	(let ((link (nth counter link-list)))
	  (message "  %s replacement is %S\n" counter link)
	  (if (and link (not (string= link "nil")))
	      (progn
		(message "  replacing with %s\n\n" (format "%s" link))
		;; replace output with our modified content
		(setq output (format "%s" link)))
	    ;; keep the original content
	    (setq output (format "%s" text)))
	  (setq counter (+ counter 1))
	  output))

      (let ((org-export-filter-link-functions '(ox-mrkup-filter-link)))
	(org-org-export-as-org)))

    (switch-to-buffer "*Org ORG Export*")
    (insert (format "# archived from %s on %s\n"
		    org-file-abs-path
		    (format-time-string "%Y-%m-%d" (current-time))))

    ;; write file to temporary directory
    (write-file (expand-file-name org-file org-archive))

    ;; add bibliography references if they exist.
    (org-ref-extract-bibtex-entries)
    (save-buffer)
    ;; zip contents into a zip file
    (shell-command (concat "zip -v -r " org-archive-zip " *"))

    ;; move the zip archive up a level
    (rename-file org-archive-zip (concat "../"org-archive ".zip"))
    (kill-buffer)

    ;; get rid of temp-dir
    (delete-directory org-archive t)
    org-archive-zip))

(defun ox-archive-create-and-mail (&optional tag)
  "create a zip file from current org-file and attach it to an email"
  (interactive "sZip basename (no extension): ")
  (let ((zip (ox-archive-create-zip tag)))
    (message-mail)
    (mml-attach-file zip)
    (message-goto-to)))

(provide 'ox-archive)
