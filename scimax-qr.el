;;; scimax-qr.el --- qr code utilities for scimax

;;; Commentary:
;;
;; * Utilities for QR codes in scimax.
;; 
;; `scimax-insert-qr' will generate and insert a QR code link in your document.
;; This relies on `org-id' so you need to be in a heading.
;;
;; `scimax-read-qr' uses a screencapture method to read a QR code somewhere on
;; your screen. I use applescript, so this is Mac specific.
;;
;; This relies on Python libraries: qrcode, PIL and zbarlight. Installation is
;; not totally obvious. see https://github.com/Polyconseil/zbarlight/

;; #+BEGIN_SRC sh
;; brew install zbar
;; export LDFLAGS="-L$(brew --prefix zbar)/lib"
;; export CFLAGS="-I$(brew --prefix zbar)/include"
;; pip install zbarlight
;; #+END_SRC
;; 
;; This is alpha code. It is not obvious what the best thing to save in the QR
;; code is. At the moment I save an `org-id' which is useful to get you back to
;; a heading. It is less useful to get to a location though.
;;
;; An alternative could be an editmark, or a an nb link, which could be a
;; pointer to a specific location. You would have to rely on `org-db' to find it
;; again (I already rely on it for `org-id').
;;
;; I use a qr link for this. It has limited export support: LaTeX and HTML for
;; now. Note when the link is activated, a qr code will be saved in the current
;; directory. I don't love that, but it is simple, and there is not an obvious
;; central place to keep them.
;;
;; Limitations: If you don't have an `id' corresponding to a QR code, nothing
;; will happen. It might even be true that if you haven't opened that id before,
;; so it isn't registered with your db, you won't be able to navigate to it.

(org-link-set-parameters
 "qr"
 :follow (lambda (path)
	   (scimax-qr/body))
 :help-echo "Click to open the QR code."
 ;; Overlay the QR code on it
 :activate-func (lambda (start end path bracketp)
		  (when
		      (and
		       ;; got a path
		       path 
		       ;; and there is no overlay here.
		       (not (ov-at start)))
		    (let* ((qr (concat path ".png"))
			   ov
			   img
			   (script (format "import qrcode
qr = qrcode.QRCode(box_size=5)
qr.add_data('%s')
qr.make(fit=True)
img = qr.make_image()
img.save('%s')
"
					   
					   (format "[[qr:%s]]" path)
					   qr)))
		      (unless (file-exists-p qr)
			(shell-command (format "python -c \"%s\"" script)))
		      (setq img (create-image (expand-file-name qr))
			    ov (make-overlay start end))
		      (overlay-put ov 'display img) 
		      (overlay-put ov 'face 'default)
		      (overlay-put ov 'org-image-overlay t)
		      (overlay-put ov 'modification-hooks
				   (list
				    `(lambda (&rest args)
				       (org-display-inline-remove-overlay ,ov t ,start ,end))))
		      (push ov org-inline-image-overlays))))
 ;; export is hard here. There is no easy way to get to the path of the overlay.
 ;; I don't really want to have to regenerate it.
 :export (lambda (path desc backend)
	   (cond
	    ((eq 'latex backend)
	     (format "\\includegraphics[width=1in]{%s}" (concat path ".png")))
	    ((eq 'html backend)
	     (format "<img src=\"%s\"/>" (concat path ".png")))
	    (t
	     (error "%s is not supported yet" backend)))))



(defun scimax-qr-redraw-thumbnails (&optional include-linked refresh beg end)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))


;; This gets the thumbnails to be redrawn with inline image toggling.
(advice-add 'org-display-inline-images :after 'scimax-qr-redraw-thumbnails)


(defhydra scimax-qr (:color blue :hint nil)
  "QR actions
"
  ("o" (lambda ()
	 (interactive)
	 (let ((lnk (org-element-context)))
	   (org-id-goto (org-element-property :path lnk))))
   "Open")
  ("c" (lambda ()
	 (interactive)
	 (let ((lnk (org-element-context)))
	   (kill-new (format "[[qr:%s]]" (org-element-property :path lnk)))))
   "Copy"))


(defun scimax-get-qr ()
  "Create a qr-code for the current heading at point.
Relies on `org-id'.
Returns a string for the qr link."
  (interactive )
  (when (org-before-first-heading-p)
    (error "You are not in an org-heading"))
  (format "[[qr:%s]]" (org-id-get-create)))


(defun scimax-insert-qr (&optional size)
  "Insert a QR code image link."
  (interactive "P")
  (insert (scimax-get-qr)))


(defun scimax-read-qr ()
  "Read a QR code.
You will be prompted to choose the QR code by a screenshot.
Returns the contents of the QR code."
  (interactive)
  (let* ((fname (make-temp-file "qr-" nil ".png"))
	 (script (format "from PIL import Image
import zbarlight

file_path = '%s'
with open(file_path, 'rb') as image_file:
    image = Image.open(image_file)
    image.load()

codes = zbarlight.scan_codes(['qrcode'], image)
print(codes[0].decode())" fname))
	 (result))
    (do-applescript
     (mapconcat
      'identity
      (list (format "set screenshotFilePath to \"%s\"" (expand-file-name fname))
	    "do shell script \"screencapture \" & \"-i \" & \" \" & quoted form of screenshotFilePath"
	    (concat "set result to \"[[./" fname "]]\"")
	    "set the clipboard to result")
      "\n"))
    (prog1 (setq result (shell-command-to-string (format "python -c \"%s\"" script)))
      (message result)
      (delete-file fname))))


(defun scimax-follow-qr ()
  "Read and follow a QR code.
Small effort made to only follow QR links to org-id."
  (interactive)
  (let ((result (scimax-read-qr)))
    (cond
     ((s-starts-with? "[[qr:" result)
      (with-temp-buffer
	(insert result)
	(org-mode)
	(goto-char (point-min))
	(org-id-goto (plist-get (cadr (org-element-link-parser)) :path))))
     (t
      (message "We don't handle \"%s\" yet." result)))))


(provide 'scimax-qr)

;;; scimax-qr.el ends here
