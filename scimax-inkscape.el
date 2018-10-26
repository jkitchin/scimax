;;; scimax-inkscape.el --- Using inkscape in org-mode

;;; Commentary:
;;
;; This library provides a new org-mode link for inkscape svg files. When you
;; click on an inkscape link, it will open the figure in inkscape. A thumbnail
;; image will be placed on the inkscape link.
;;
;; Export to HTML:
;; (browse-url (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
;;   (org-html-export-to-html)))
;;
;; (org-open-file (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
;;   (org-latex-export-to-pdf)))
;;
;; inkscape does not allow you to create empty files. We save the template in a
;; variable and create them on demand.

(defcustom scimax-inkscape-thumbnail-width 300
  "Width of thumbnails in pts."
  :group 'scimax-inkscape)

(defcustom scimax-inkscape-template-svg
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>
<!-- Created with Inkscape (http://www.inkscape.org/) -->

<svg
   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
   xmlns:cc=\"http://creativecommons.org/ns#\"
   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
   xmlns:svg=\"http://www.w3.org/2000/svg\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:sodipodi=\"http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd\"
   xmlns:inkscape=\"http://www.inkscape.org/namespaces/inkscape\"
   width=\"6in\"
   height=\"4in\"
   viewBox=\"0 100 152.4 201.6\"
   version=\"1.1\"
   id=\"svg8\"
   inkscape:version=\"0.92.2 (5c3e80d, 2017-08-06)\"
   sodipodi:docname=\"drawing.svg\">
  <defs
     id=\"defs2\" />
  <sodipodi:namedview
     id=\"base\"
     pagecolor=\"#ffffff\"
     bordercolor=\"#666666\"
     borderopacity=\"1.0\"
     inkscape:pageopacity=\"0.0\"
     inkscape:pageshadow=\"2\"
     inkscape:zoom=\"1\"
     inkscape:cx=\"400\"
     inkscape:cy=\"214.9\"
     inkscape:document-units=\"in\"
     inkscape:current-layer=\"layer1\"
     showgrid=\"false\"
     units=\"in\"
     inkscape:window-width=\"1080\"
     inkscape:window-height=\"675\"
     inkscape:window-x=\"0\"
     inkscape:window-y=\"78\"
     inkscape:window-maximized=\"0\"
     inkscape:lockguides=\"true\"
     fit-margin-top=\"0\"
     fit-margin-left=\"0\"
     fit-margin-right=\"0\"
     fit-margin-bottom=\"0\" />
  <metadata
     id=\"metadata5\">
    <rdf:RDF>
      <cc:Work
         rdf:about=\"\">
        <dc:format>image/svg+xml</dc:format>
        <dc:type
           rdf:resource=\"http://purl.org/dc/dcmitype/StillImage\" />
        <dc:title></dc:title>
      </cc:Work>
    </rdf:RDF>
  </metadata>
  <g
     inkscape:label=\"Layer 1\"
     inkscape:groupmode=\"layer\"
     id=\"layer1\"
     transform=\"translate(0,0)\" />
</svg>
"
  "Blank document for inkscape. You cannot create a file at the
  command line, so we put this template in and open it. This one works for Inkscape 0.92.2")


(defun scimax-inkscape-open (path)
  "Open the PATH in inkscape.
Make a new file if needed."
  (interactive)
  (unless (f-ext-p path "svg") (error "Must be an svg file."))
  (unless (file-exists-p path)
    (with-temp-file path
      (insert scimax-inkscape-template-svg)))
  (shell-command (format "inkscape %s &" path)))


(defun scimax-inkscape-thumbnail (start end path bracketp)
  "Put a thumbnail on an inkscape link."
  (let (img ov)
    (when (and
	   ;; got a path
	   path
	   ;; it is an image
	   (org-string-match-p (image-file-name-regexp) path)
	   ;; and it exists
	   (f-exists? path)
	   ;; and there is no overlay here.
	   (not (ov-at start)))
      (setq img (create-image
		 (expand-file-name path)
		 'imagemagick nil :width scimax-inkscape-thumbnail-width
		 :background "lightgray"))
      (setq ov (make-overlay start end))
      (overlay-put ov 'display img)
      (overlay-put ov 'face 'default)
      (overlay-put ov 'before-string "inkscape:")
      (overlay-put ov 'org-image-overlay t)
      (overlay-put ov 'modification-hooks
		   (list
		    `(lambda (&rest args)
		       (org-display-inline-remove-overlay ,ov t ,start ,end))))
      (push ov org-inline-image-overlays))))


(defun scimax-inkscape-redraw-thumbnails (&rest args)
  "Use font-lock to redraw the links."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (org-restart-font-lock)))

;; This gets the thumbnails to be redrawn with inline image toggling.
(advice-add 'org-display-inline-images :after 'scimax-inkscape-redraw-thumbnails)


(defun scimax-inkscape-preprocess (backend)
  "Preprocessing function to run in `org-export-before-processing-hook'.
Here are two examples:

 (browse-url (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-html-export-to-html)))

 (org-open-file (let ((org-export-before-processing-hook '(scimax-inkscape-preprocess)))
  (org-latex-export-to-pdf)))"
  (let ((links (reverse (org-element-map (org-element-parse-buffer) 'link
			  (lambda (link)
			    (when (string= (org-element-property :type link) "inkscape")
			      link))))))
    (loop for link in links
	  do
	  (goto-char (org-element-property :begin link))
	  (re-search-forward "inkscape:" (org-element-property :end link))
	  (replace-match "file:"))))


(org-link-set-parameters
 "inkscape"
 :follow 'scimax-inkscape-open
 :help-echo "Click to open in inkscape."
 :activate-func 'scimax-inkscape-thumbnail
 :export (lambda (path desc backend)
	   (cond
	    ((eq 'html backend)
	     (format "<img src=\"%s\"" path))))
 ;;  You need to use the `scimax-inkscape-preprocess' function in a hook for
 ;; more advanced export options like captions.
 )

(defun scimax-inkscape-insert-drawing (path)
  "Convenience function to insert a drawing with filename PATH."
  (interactive "sFilename: ")
  (insert (format "inkscape:%s" path)))

(provide 'scimax-inkscape)

;;; scimax-inkscape.el ends here
