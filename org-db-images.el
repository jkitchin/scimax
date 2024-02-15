;;; org-db-images.el --- Indexes images for text search in org-db


;;; Commentary:
;;

(require 'org-db)

;;; Code:

(defun org-db-images-setup ()
  (sqlite-execute org-db "create table if not exists images(
rowid integer primary key,
filename_id integer,
img_filename text,
position integer,
text text,
foreign key(filename_id) references files(rowid) on delete cascade)"))


(defun org-db-update-images (filename-id parse-tree org-db)
  "Update the image content for the current buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-images-setup)
  (let* ((img-data (org-element-map parse-tree 'link
		     (lambda (lnk)
		       (when (and
			      (string= "file" (org-element-property :type lnk))
			      (member (file-name-extension (org-element-property :path lnk))
				      '("png" "jpg")))
			 (cons
			  (org-element-property :path lnk)
			  (org-element-property :begin lnk))))))
	 text)

    ;; eliminate old entries
    (sqlite-execute org-db "delete from images where filename_id = ?"
		    (list filename-id))

    ;; Add new entries
    (cl-loop for (img-file . begin) in img-data do
	     (setq text (shell-command-to-string
			 (format "tesseract %s -" img-file)))
	     (sqlite-execute org-db "insert into images values (?, ?, ?, ?, ?)"
			     (list nil filename-id
				   (expand-file-name img-file)
				   begin
				   text)))))


(add-to-list 'org-db-update-functions #'org-db-update-images t)


(defun org-db-images ()
  "Search the images database."
  (interactive)
  (let* ((db-candidates (with-org-db
			 (sqlite-select org-db "select images.text, images.img_filename, images.position, files.filename
from images
inner join files on files.rowid = images.filename_id")))
	 (candidates (cl-loop for c in db-candidates collect
			      (append
			       (list
				(concat
				 (s-join " " (s-split "\n" (car c)))
				 "\n"
				 (propertize "\n" 'display (create-image (cl-second c) nil nil :width 300))))
			       (cdr c)))))

    (ivy-read "Query: " candidates
	      :action '(1
			("o" (lambda (candidate)
			       (pcase-let ((`(,text ,img-filename ,position ,org-file) candidate))
				 (find-file org-file)
				 (goto-char position)))
			 "open org at img")
			("m" (lambda (candidate)
			       (pcase-let ((`(,text ,img-filename ,position ,org-file) candidate))
				 (find-file img-filename)))
			 "open img")
			("c" (lambda (candidate)
			       (pcase-let ((`(,text ,img-filename ,position ,org-file) candidate))
				 (kill-new img-filename)))
			 "Copy path to image")))))


(defun org-db-images-purge ()
  "Remove image entries where the image path does not exist."
  (interactive)
  (let ((imgs (mapcar 'car (with-org-db
			    (sqlite-select org-db "select img_filename from images")))))

    (cl-loop for img in imgs do
	     (when (not (file-exists-p img))
	       (message "Deleting row for %s" img)
	       (with-org-db
		(sqlite-execute org-db "delete from images where img_filename = ?"
				(list img)))))))

(provide 'org-db-images)

;;; org-db-images.el ends here
