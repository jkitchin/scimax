;;; scimax-md.el --- A better exporter for markdown

;;; Commentary:
;;

(require 'ox-md)

;; * A better link exporter
;; Handle figures better, mostly with captions and labels.

(defun scimax-md-link (link contents info)
  "Export a link to markdown."
  (cond
   ;; This is an image with a caption
   ((and (string= "file" (org-element-property :type link))
	 (-contains?
	  '("png")
	  (file-name-extension
	   (org-element-property :path link)))
	 (org-export-get-caption
	  (org-element-property :parent link)))
    (format "
<figure>
  <img src=\"%s\">
  <figcaption>Figure (%s): %s</figcaption>
</figure>"
	    ;; image path
	    (org-element-property :path link)
	    ;; TODO: Figure label. This is super-hacky...
	    (let ((caption (org-export-data
			    (org-export-get-caption
			     (org-element-property :parent link))
			    info)))
	      (string-match "name=\"\\(.*?\\)\">" caption)
	      (match-string 1 caption))


	    ;; The caption
	    (org-export-data
	     (org-export-get-caption
	      (org-element-property :parent link))
	     info)))

   ;; This is at least true for radio links.
   ((string= "fuzzy" (org-element-property :type link))
    (let ((path (org-element-property :path link)))
      (format "[%s](#%s)" path path)))

   ;; file links. treat links to org files as links to md files.
   ((and (string= "file" (org-element-property :type link))
	 (f-ext? (org-element-property :path link) "org")
	 (not (-contains?
	       '("png")
	       (file-name-extension
		(org-element-property :path link)))))
    
    (format "[%s](%s)"
	    ;; [%s] is the description
	    (if (org-element-property :contents-begin link)
		(buffer-substring (org-element-property :contents-begin link)
				  (org-element-property :contents-end link))
	      (file-name-sans-extension (org-element-property :path link)))
	    
	    (let ((path (org-element-property :path link)))
	      (if (plist-get info :md-link-org-files-as-md)
		  (concat (file-name-sans-extension path) ".md")
		path))))
   
   ;; fall-through to the default exporter.
   (t
    (org-md-link link contents info))))


(defun scimax-md-target (target contents info)
  "redefine targets as a div, since they are probably readable text."
  (let ((value (org-element-property :value target)))
    (format "<a name=\"%s\"></a>%s" value value)))


(defun scimax-md-src-block (src contents info)
  "use fences and language"
  (format "```%s\n%s\n```" (org-element-property :language src)
	  (org-element-property :value src)))

;; * New export backend
;; You need this to use the functions above.

(defun scimax-md-export-to-buffer (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer as a scimax-md buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Optional argument BODY-ONLY has no effect.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*scimax-md*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'scimax-md "*scimax-md*"
    async subtreep visible-only body-only ext-plist
    (lambda () (markdown-mode))))


(defun scimax-md-export-to-file
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a scimax-md file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Optional argument BODY-ONLY has no effect

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'scimax-md outfile
      async subtreep visible-only body-only ext-plist)))

(org-export-define-derived-backend 'scimax-md 'md
  :translate-alist '((link . scimax-md-link)
		     (target . scimax-md-target)
		     (src-block . scimax-md-src-block))
  :menu-entry
  '(?s "Export with scimax-md"
       ((?b "As buffer" scimax-md-export-to-buffer)
	(?s "As file" scimax-md-export-to-file)
	(?f "Publish current file" (lambda (&rest args) (org-publish-current-file)))
	(?p "Publish current project" (lambda (&rest args) (org-publish-current-project))))))


;; * Publishing

(defun scimax-md-publish-to-md (plist filename pub-dir)
  "Publish an org file to md.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'scimax-md filename
		      ".md"
		      plist pub-dir))



;; * buttons for markdown mode
(require 'button-lock)


(defun scimax-md-activate-buttons ()
  ;; Make links in markdown clickable.
  (button-lock-mode +1)

  ;; things like [Downsides to this approach](#orgbaa3187)
  ;; these are links to anchors
  (button-lock-set-button
   "\\[.*?\\](#\\(.*?\\))"
   (lambda ()
     (interactive)
     (save-excursion
       (re-search-backward "\\[")
       (when (looking-at "\\[.*?\\](#\\(.*?\\))")
	 (goto-char (match-end 1))))
     (goto-char (point-min))
     ;; look for name="label", or id="label" . Assume these are in anchors.
     (re-search-forward (format "name\\|id=\"%s\""
				(regexp-quote (match-string 1)))))
   :help-echo "This points to an anchor.")

  ;; file links
  (button-lock-set-button
   "\\[.*?\\](\\(.*?\\))"
   (lambda ()
     (interactive)
     (save-excursion
       (re-search-backward "\\[")
       (when (looking-at "\\[.*?\\](\\(.*?\\))")
	 (cond
	  ((file-exists-p (match-string 1))
	   (find-file (match-string 1)))
	  ((s-starts-with? "http" (match-string 1))
	   (browse-url (match-string 1)))
	  (t
	   (message "I don't know what to do with %s" (match-string 1))))))
     :help-echo "This points to a file or url.")))

(add-hook 'markdown-mode-hook 'scimax-md-activate-buttons)

(provide 'scimax-md)

;;; scimax-md.el ends here
