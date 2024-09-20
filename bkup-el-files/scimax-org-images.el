;;; scimax-org-images.el --- scimax customizations for images in org-mode
;; * Rescaling inline-images
;; This may eventually be obsolete if this makes it into org-mode

;;; Commentary:
;; This module provides some functionality for images that is not in vanilla
;; org-mode. 1. On Windows, emacs is not always built with imagemagick support.
;; This library enables you to use an external program from imagemagick to build
;; the thumbnails required to show images. 2. It expands the ways you can resize
;; an image to support more options from mogrify.

;;; Code:

(defvar org-inline-image-resize-function
  #'scimax-org-inline-image-resize
  "Function that takes a filename and resize argument and returns
 a new filename pointing to the resized image.")


(defun scimax-org-inline-image-resize (fname resize-options)
  "Resize FNAME with RESIZE-OPTIONS.
RESIZE-OPTIONS are passed to \"mogrify resized-fname -resize resize-options\".
RESIZE-OPTIONS could be:

N% to scale the image by a percentage.
N to set the width, keeping the aspect ratio constant.
xN to set the height, keeping the aspect ratio constant.
NxM! to set the width and height, ignoring the aspect ratio.

See http://www.imagemagick.org/Usage/resize/#resize for more options."
  (let* ((md5-hash (with-temp-buffer (insert-file-contents fname)
				     (insert (format "%s" resize-options))
				     (md5 (buffer-string))))
	 (resized-fname (concat (expand-file-name
				 md5-hash
				 temporary-file-directory)
				"."
				(file-name-extension fname)))
	 (cmd (format "mogrify -resize %s %s"
		      resize-options
		      resized-fname)))
    (if (not (executable-find "mogrify"))
	(progn
	  (message "No mogrify executable found. To eliminate this message, set  `org-inline-image-resize-function' to nil or install imagemagick from http://www.imagemagick.org/script/binary-releases.php")
	  fname)
      (unless (file-exists-p resized-fname)
	(copy-file fname resized-fname)
	(shell-command cmd))
      resized-fname)))




;; this is copied and modified from org.el
(defun scimax-org-display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.

An inline image is a link which follows either of these
conventions:

  1. Its path is a file with an extension matching return value
     from `image-file-name-regexp' and it has no contents.

  2. Its description consists in a single link of the previous
     type.

When optional argument INCLUDE-LINKED is non-nil, also links with
a text description part will be inlined.  This can be nice for
a quick look at those images, but it does not reflect what
exported files will look like.

When optional argument REFRESH is non-nil, refresh existing
images between BEG and END.  This will create new image displays
only if necessary.  BEG and END default to the buffer
boundaries."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let ((case-fold-search t)
	   (file-extension-re (image-file-name-regexp)))
       (while (re-search-forward "[][]\\[\\(?:file\\|[./~]\\)" end t)
	 (let ((link (save-match-data (org-element-context))))
	   ;; Check if we're at an inline image.
	   (when (and (equal (org-element-property :type link) "file")
		      (or include-linked
			  (not (org-element-property :contents-begin link)))
		      (let ((parent (org-element-property :parent link)))
			(or (not (eq (org-element-type parent) 'link))
			    (not (cdr (org-element-contents parent)))))
		      (org-string-match-p file-extension-re
					  (org-element-property :path link)))
	     (let ((file (expand-file-name
			  (org-link-unescape
			   (org-element-property :path link)))))
	       (when (file-exists-p file)
		 (let ((width
			;; Apply `org-image-actual-width' specifications.
			(cond
			 ((and (not (image-type-available-p 'imagemagick))
			       (not org-inline-image-resize-function))
			  nil)
			 ((eq org-image-actual-width t) nil)
			 ((listp org-image-actual-width)
			  (or
			   ;; First try to find a width among
			   ;; attributes associated to the paragraph
			   ;; containing link.
			   (let* ((paragraph
				   (let ((e link))
				     (while (and (setq e (org-element-property
							  :parent e))
						 (not (eq (org-element-type e)
							  'paragraph))))
				     e))
				  (attr_org (org-element-property :attr_org paragraph)))
			     (when attr_org
			       (plist-get
				(org-export-read-attribute :attr_org  paragraph) :width)))
			   ;; Otherwise, fall-back to provided number.
			   (car org-image-actual-width)))
			 ((numberp org-image-actual-width)
			  org-image-actual-width)))
		       (old (get-char-property-and-overlay
			     (org-element-property :begin link)
			     'org-image-overlay)))
		   (if (and (car-safe old) refresh)
		       (image-refresh (overlay-get (cdr old) 'display))

		     (when (and width org-inline-image-resize-function)
		       (setq file (funcall  org-inline-image-resize-function file width)
			     width nil))
		     (let ((image (create-image file
						(cond
						 ((image-type-available-p 'imagemagick)
						  (and width 'imagemagick))
						 (t nil))
						nil
						:width width)))
		       (when image
			 (let* ((link
				 ;; If inline image is the description
				 ;; of another link, be sure to
				 ;; consider the latter as the one to
				 ;; apply the overlay on.
				 (let ((parent
					(org-element-property :parent link)))
				   (if (eq (org-element-type parent) 'link)
				       parent
				     link)))
				(ov (make-overlay
				     (org-element-property :begin link)
				     (progn
				       (goto-char
					(org-element-property :end link))
				       (skip-chars-backward " \t")
				       (point)))))
			   (overlay-put ov 'display image)
			   (overlay-put ov 'face 'default)
			   (overlay-put ov 'org-image-overlay t)
			   (overlay-put
			    ov 'modification-hooks
			    (list 'org-display-inline-remove-overlay))
			   (push ov org-inline-image-overlays)))))))))))))))


(defun scimax-toggle-image-advice ()
  "Toggle the scimax image advice."
  (interactive)
  (if (not (get 'scimax-org-display-inline-images 'enabled))
      (progn
	(advice-add 'org-display-inline-images :override #'scimax-org-display-inline-images)
	(put 'scimax-org-display-inline-images 'enabled t)
	(message "Scimax image advice enabled."))
    (advice-remove 'org-display-inline-images #'scimax-org-display-inline-images)
    (put 'scimax-org-display-inline-images 'enabled nil)
    (message "Scimax image advice disabled.")))


;; * Copy images to the clipboard
;; This only works for Macs.

;; Adapted from https://apple.stackexchange.com/questions/15318/using-terminal-to-copy-a-file-to-clipboard/15542#15542
;;
;; The idea is if I click on an image it is copied to the clipboard so I can paste it somewhere.

(defun scimax-org-copy-image ()
  "Copy the image at point to the clipboard.
Run this on a link"
  (interactive)
  (unless (string= system-type "darwin")
    (error "`scimax-org-copy-image' only works on a Mac."))
  (let* ((oe (org-element-context))
	 ;; I do not know how to set this to a PNG picture. So far JPEG works.
	 ;; TIFF also works. I would really like a PNG, but this is so hacky it
	 ;; is fine for now.
	 (applescript "set the clipboard to (read (POSIX file \"%s\") as JPEG picture)")
	 (path (cond
		((eq 'link (car oe))
		 (setq path (org-element-property :path oe)))
		;; we can handle overlays
		((and (ov-at (point))
		      (eq 'image (car (overlay-get (ov-at) 'display))))
		 (setq path (plist-get (cdr (overlay-get (ov-at) 'display)) :file)))
		(t
		 (error "Unable to copy %s." oe)))))
    (if (null path)
	(error "No image path found.")

      ;; we need absolute paths for the applescript
      (if (not (file-name-absolute-p path))
	  (setq path (expand-file-name path)))

      ;; Finally let's check that we have a path with an image extension. I
      ;; think this will work on all kinds of images.
      (if (member (f-ext path) image-file-name-extensions)
	  (prog1 (do-applescript (format applescript path))
	    (message "Copied %s to the clipboard" path))
	(error "%s cannot be copied. Maybe it is not an image?" path)))))

;; This lets me click on an image to copy it to the clipboard.
;; (when (string= system-type "darwin")
;;   (define-key image-map (kbd "<mouse-1>") #'scimax-org-copy-image))


(defun scimax-org-image-toggle-image-CcCc ()
  (when (org-element-property :attr_org (org-element-context))
    (org-toggle-inline-images t)
    (org-toggle-inline-images t)
    t))

(add-to-list 'org-ctrl-c-ctrl-c-hook 'scimax-org-image-toggle-image-CcCc)

(setq imagemagick-types-inhibit (remove 'PDF imagemagick-types-inhibit))

(provide 'scimax-org-images)

;;; scimax-org-images.el ends here
