;;; scimax-link-thumbnails.el --- Put thumbnails on org-mode links.

;;; Commentary:
;; This module puts thumbnails on org-mode file links that have images in the
;; description.

(defcustom org-file-thumbnail-width 300
  "Width of a thumbnail on file links."
  :group 'scimax-org)


(defun org-file-link-image-thumbnail (start end path bracketp)
  "Put a thumbnail on a file link if the description is an image."
  (when bracketp
    (let ((s (buffer-substring-no-properties start end))
	  img ov)
      (when (and s (string-match org-bracket-link-regexp s))
	(setq imgfile (match-string 3 s))
	(when (and
	       ;; got a match
	       imgfile
	       ;; it is an image
	       (org-string-match-p (image-file-name-regexp) imgfile)
	       ;; and it exists
	       (f-exists? imgfile)
	       ;; and there is no overlay here.
	       (not (ov-at start)))
	  (setq img (create-image (expand-file-name imgfile)
				  'imagemagick nil :width org-file-thumbnail-width
				  :background "lightgray"))
	  (setq ov (make-overlay start end))
	  (overlay-put ov 'display img)
	  (overlay-put ov 'face 'default)
	  (overlay-put ov 'org-image-overlay t)
	  (overlay-put ov 'modification-hooks
		       (list
			`(lambda (&rest args)
			   (org-display-inline-remove-overlay ,ov t ,start ,end))))
	  (push ov org-inline-image-overlays))))))


;; We use the activate-func feature to get images on links.
(org-link-set-parameters "file" :activate-func 'org-file-link-image-thumbnail)


(defun org-file-link-redraw-thumbnails (&rest args)
  (org-restart-font-lock))


;; this redisplays these thumbnails on image toggling
(advice-add 'org-display-inline-images :after 'org-file-link-redraw-thumbnails)


;; modify ] to redisplay images if needed.
(defun org-link-closing-square-bracket ()
  "Make a closing square bracket redisplay images.
This just makes images show up if they already exist."
  (interactive)
  (insert "]")
  (when (looking-back org-bracket-link-regexp (line-beginning-position))
    (org-redisplay-inline-images)))


(define-key org-mode-map (kbd "]") #'org-link-closing-square-bracket)


(provide 'scimax-link-thumbnails)

;;; scimax-link-thumbnails.el ends here
