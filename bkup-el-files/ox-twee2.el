;;; ox-twee2.el --- export org to twee2 format for interactive fiction

;;; Commentary:
;;
;; https://twinery.org/
;; https://dan-q.github.io/twee2/
;;
;; A passage is a level one headline. You should put each passage in a separate file, and link them by filename.

;;; Code:

(defun ox-twee2-headline (headline contents info)
  "Convert a HEADLINE to twee2.
Level one headings are passages. Subheadings are rendered in twee markup.
Tags on level one headings are added as twee tags.
Argument CONTENTS the contents.
Argument INFO the info plist."
  (let* ((title (org-export-data (org-element-property :title headline) info))
	 (level (org-export-get-relative-level headline info))
	 (tags (and (plist-get info :with-tags)
		    (org-export-get-tags headline info))))
    (if (= level 1)
	(format "::%s%s\n\n%s" title
		(if tags (format " [%s]" (s-join " " tags)) "")
		contents)
      (format "%s%s\n\n%s"
	      (make-string level (string-to-char "!"))
	      title
	      contents))))


(defun ox-twee2-link (link contents info)
  "Transcode a LINK to twee.
Images are handled separate from regular links.
It is not likely that a link like a citation is handled right.
Argument CONTENTS .
Argument INFO ."
  (let* ((type (org-element-property :type link))
	 (path (org-element-property :path link))
	 (description (and (org-element-property :contents-begin link)
			   (buffer-substring (org-element-property :contents-begin link)
					     (org-element-property :contents-end link))))
	 (paragraph (let ((e link))
		      (while (and (setq e (org-element-property
					   :parent e))
				  (not (eq (org-element-type e)
					   'paragraph))))
		      e))
	 (attr-twee (when paragraph (org-export-read-attribute :attr_twee  paragraph))))
    (cond
     ;; an image file
     ;; maybe I should see about additional attr_twee2: things
     ;; like :link :title :setter later
     ((and (string= type "file") (org-file-image-p path))
      (let ((title (plist-get attr-twee :title))
	    (link (plist-get attr-twee :link))
	    (setter (plist-get attr-twee :setter)))
	(cond
	 ((and title link setter)
	  (format "[img[%s|%s][%s][%s]]" title path link setter))

	 ((and title link)
	  (format "[img[%s|%s][%s]]" title image link))

	 ((and link setter)
	  (format "[img[%s][%s][%s]]" path link setter))

	 (title
	  (format "[img[%s|%s]]" title path))

	 (link
	  (format "[img[%s][%s]]" path link))

	 (t
	  (format "[img[%s]]" path)))))
     ;; Special case for cite links. This is a little hacky, but simple. There
     ;; is no overall bibliography in a passage.
     ((-contains? org-ref-cite-types type)
      (format  "<font color=\"green\" title=\"%s\">%s</font>"
	       (org-ref-clean-unused-entry-html (org-ref-get-bibtex-entry-citation path))
	       path))
     ;; this is a goto link with a description
     (description
      (format "[[%s|%s]]" description path))
     ;; regular goto link
     (t
      (format "[[%s]]" path)))))


(defun ox-twee2-target (target contents info)
  "Transcode the TARGET to twee.
CONTENTS the contents
INFO plist"
  (format "<<%s>>" (org-element-property :value target)))


(defun ox-twee2-src-block (src-block contents info)
  "Wraps the html in the verbatim html tags for twee2.
CONTENTS the contents
INFO plist"
  (format "<html>%s</html>"
	  (let ((org-html-with-latex 'imagemagick))
	    (org-html-src-block src-block contents info))))


(defun ox-twee2-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK to twee.
CONTENTS the contents
INFO plist"
  (buffer-substring (org-element-property :contents-begin special-block)
		    (org-element-property :contents-end special-block)))


(defun ox-twee2-bold (bold contents info)
  "Transcode a BOLD element.
CONTENTS the contents
INFO plist"
  (format "''%s''" contents))


(defun ox-twee2-italic (italic contents info)
  "Transcode an ITALIC element to twee.
CONTENTS the contents
INFO plist"
  (format "//%s//" contents))


(defun ox-twee2-underline (underline contents info)
  "Transcode an UNDERLINE element.
CONTENTS the contents
INFO plist"
  (format "__%s__" contents))


(defun ox-twee2-strike-through (strikethrough contents info)
  "Transcode a STRIKETHROUGH element.
CONTENTS the contents
INFO plist"
  (format "==%s==" contents))


(defun ox-twee2-subscript (subscript contents info)
  "Transcode a SUBSCRIPT element.
CONTENTS the contents
INFO plist"
  (format "~~%s~~" contents))


(defun ox-twee2-superscript (superscript contents info)
  "Transcode a SUPERSCRIPT element.
CONTENTS the contents
INFO plist"
  (format "^^%s^^" contents))


;; I derive this from markdown. I thought html would be better, but it adds a lot
;; of stuff that tends to break twee2
(org-export-define-derived-backend 'twee2 'md
  :menu-entry
  '(?t "Export to twee2"
       ((?b "to buffer" ox-twee2-export-to-buffer)
	(?f "to file" ox-twee2-export-to-file)
	(?o "open" ox-twee2-export-to-file-and-open)))
  :options-alist '((:with-toc nil)
		   (:with-latex "imagemagick"))
  :translate-alist '((bold . ox-twee2-bold)
		     (headline . ox-twee2-headline)
		     (italic . ox-twee2-italic)
		     (link . ox-twee2-link)
		     ;; this is especially to get a verbatim block.
		     (special-block . ox-twee2-special-block)
		     (src-block . ox-twee2-src-block)
		     (strike-through . ox-twee2-strike-through)
		     (subscript . ox-twee2-subscript)
		     (superscript . ox-twee2-superscript)
		     (target . ox-twee2-target)
		     (underline . ox-twee2-underline)))


(defun ox-twee2-export-to-buffer (&optional async subtreep visible-only body-only info)
  "Export to a buffer.
ASYNC if non-nil
SUBTREEP if non-nil limit to subtree
VISIBLE-ONLY if non-nil limit to visible
BODY-ONLY if non-nil show only body
INFO plist."
  (org-export-to-buffer 'twee2 "*Org twee2 Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


(defun ox-twee2-export-to-file (&optional async subtreep visible-only body-only info)
  "Export to a file, with .tw2 extension.
ASYNC if non-nil
SUBTREEP if non-nil limit to subtree
VISIBLE-ONLY if non-nil limit to visible
BODY-ONLY if non-nil show only body
INFO plist."
  (let ((outfile (org-export-output-file-name ".tw2" subtreep)))
    (when (file-exists-p outfile) (delete-file outfile))
    (org-export-to-file 'twee2 outfile
      async subtreep visible-only body-only info)))


(defun ox-twee2-export-to-file-and-open (&optional async subtreep visible-only body-only info)
  "Export to HTML and open.
ASYNC if non-nil
SUBTREEP if non-nil limit to subtree
VISIBLE-ONLY if non-nil limit to visible
BODY-ONLY if non-nil show only body
INFO plist."
  (let* ((twfile)
	 (html (org-export-output-file-name ".html" subtreep)))
    (when (file-exists-p html) (delete-file html))
    (setq twfile (ox-twee2-export-to-file async subtreep visible-only body-only info))
    (shell-command (format "twee2 build %s %s" twfile html))
    (browse-url html)))


(provide 'ox-twee2)

;;; ox-twee2.el ends here
