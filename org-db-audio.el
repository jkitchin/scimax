;;; org-db-audio.el --- an org-db module for searching audio from aiff and mov

;;; Commentary:
;;
;; This package extracts audio from audio and video editmarks, and saves it in
;; org-db so you can search it.
;; 
;; You need to install these python packages
;; 
;; pip install pocketsphinx SpeechRecognition moviepy
;;
;; and you need to install scimax python
;;
;; The utility of this depends on the quality of the transcription.

(require 'org-db)

(defun org-db-audio-setup ()
  "Code to setup an audio table in org-db."
  (sqlite-execute org-db "create table if not exists audio(
rowid integer primary key,
filename_id integer,
audio_filename text,
position integer,
text text,
foreign key(filename_id) references files(rowid) on delete cascade)"))


(defun org-db-update-audio (filename-id parse-tree org-db)
  "Update the audio content for the current buffer.
FILENAME-ID is the rowid for the org-file.
PARSE-TREE is from `org-element-parse-buffer'."
  (org-db-audio-setup)

  ;; Only when we have sem installed
  (when (fboundp 'sem-get-editmarks)
     
    (sqlite-execute org-db "delete from audio where filename_id = ?" (list filename-id))

    (save-excursion
      (cl-loop for (em-type buffer (start . end) em-content) in (sem-get-editmarks)
	       do
	       (cond
		;; This should work for aiff audio and mov video
		((or (eq em-type 'audio)
		     (eq em-type 'video))
		 (goto-char start)
		 (let* ((avfile (plist-get (sem-editmark-plist) :file))
			;; a2t is a cli I wrote in scimax python
			;; [[nb:user::python/scimax/scimax/audio.py::c1]]
			(text (shell-command-to-string (format "a2t %s" avfile))))
		   (sqlite-execute org-db "insert into audio values (?, ?, ?, ?, ?)"
				   (list nil
					 filename-id
					 avfile					     
					 start
					 text)))))))))


(add-to-list 'org-db-update-functions #'org-db-update-audio t)


(defun org-db-audio (&optional project)
  "Search the audio table."
  (interactive "P")
  (let* ((db-candidates (with-org-db
			 (sqlite-select
			  org-db
			  (format
			   "select audio.text, audio.audio_filename, audio.position, files.filename
from audio
inner join files on files.rowid = audio.filename_id%s"
			   (if project
			       (format " where files.filename like \"%s%%\"" (projectile-project-root))
			     "")))))
	 (candidates (cl-loop for (text fname position ofile) in db-candidates collect
			      (list
			       (concat
				(format "%s in %s" fname ofile)
				"\n"
				(s-word-wrap
				 80 
				 (s-join " " (s-split "\n" text))))
			       fname
			       position
			       ofile))))

    (ivy-read "Query: " candidates
	      :caller 'org-db-audio
	      :action '(1
			("o" (lambda (candidate)
			       (pcase-let ((`(,text ,audio-filename ,position ,org-file) candidate))
				 (find-file org-file)
				 (goto-char position)))
			 "open org at audio")
			("m" (lambda (candidate)
			       (pcase-let ((`(,text ,audio-filename ,position ,org-file) candidate))
				 (shell-command (format "open %s" audio-filename))))
			 "open audio")))))


(provide 'org-db-audio)

;;; org-db-audio.el ends here
