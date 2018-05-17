;;; scimax-lob.el --- Library of babel for scimax

;;; Commentary:
;; This was motivated by the work described at
;; http://kdr2.com/tech/emacs/1805-approach-org-ref-code-to-text.html. This
;; enables one to reuse code-blocks in org files more easily.


(require 'f)
(require 'cl)

(defvar scimax-lob-directory (directory-file-name (expand-file-name "scimax-lob" scimax-dir))
  "Directory where library of babel org files reside.")

(unless (file-directory-p scimax-lob-directory)
  (make-directory scimax-lob-directory t))

(defun scimax-load-lob ()
  "Load the library of babel files."
  (interactive)
  (cl-loop for org-file in (f-entries scimax-lob-directory (lambda (f) (f-ext? f "org")) t)
	   do
	   (message "ingesting %s" org-file)
	   (org-babel-lob-ingest org-file)))

(scimax-load-lob)

(provide 'scimax-lob)

;;; scimax-lob.el ends here
