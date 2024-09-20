;;; org-db-images.el --- Indexes images for text search in org-db


;;; Commentary:
;; This relies on tesseract https://github.com/tesseract-ocr/tesseract to
;; extract text from images, and then store that text for searching. It is sort
;; of a proof of concept. It works ok, and is certainly limited by the OCR
;; quality.

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
			 (format "tesseract %s - quiet" img-file)))
	     (sqlite-execute org-db "insert into images values (?, ?, ?, ?, ?)"
			     (list nil filename-id
				   (expand-file-name img-file)
				   begin
				   text)))))


(add-to-list 'org-db-update-functions #'org-db-update-images t)


(defun org-db-candidate-transformer (candidate)
  "Propertize CANDIDATE with an image.
This is much faster than putting it on the candidates at creation time"
  (let* ((idx (get-text-property 1 'idx candidate))
	 (entry (nth idx (ivy-state-collection ivy-last))))
    
    (concat
     ;; pad each line text to 80 chars wide after we wrap it at 70
     (mapconcat (lambda (s) (s-pad-right 80 " " s))
		(s-split "\n" (s-word-wrap 80 candidate))
		"\n" )
     ;; add a thumbnail
     (propertize " " 'display (create-image (nth 1 entry) nil nil :width 300)))))

(ivy-configure 'org-db-images :display-transformer-fn #'org-db-candidate-transformer)


(defun org-db-images (&optional project)
  "Search the images database."
  (interactive "P")
  (let* ((db-candidates (with-org-db
			 (sqlite-select
			  org-db
			  (format
			   "select images.text, images.img_filename, images.position, files.filename
from images
inner join files on files.rowid = images.filename_id%s"
			   (if project
			       (format " where files.filename like \"%s%%\"" (projectile-project-root))
			     "")))))
	 (candidates (cl-loop for (text fname position ofile) in db-candidates collect
			      (list
			       (s-join " " (s-split "\n" text))
			       fname
			       position
			       ofile))))

    (ivy-read "Query: " candidates
	      :caller 'org-db-images
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
  "Remove image entries where the image path does not exist.
There is not an automated way to remove images when you delete
files, or change filenames, etc. You have to run this manually
from time to time."
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
