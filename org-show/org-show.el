;;; org-show.el --- Summary
;; Copyright(C) 2014 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Contributions from Sacha Chua.
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; A simple mode for presenting org-files as slide-shows. A slide is a headline
;; with a :slide: tag. See file:org-show.org for usage.

;;; Code:
(require 'animate)
(require 'easymenu)

;;* Variables

(defvar org-show-presentation-file nil
  "File containing the presentation.")

(defvar org-show-slide-tag "slide"
  "Tag that marks slides.")

(defvar org-show-slide-tag-regexp
  (concat ":" (regexp-quote org-show-slide-tag) ":")
  "Regex to identify slide tags.")

(defvar org-show-latex-scale 4.0
  "Scale for latex preview.")

(defvar org-show-original-latex-scale
  (if (boundp 'org-format-latex-options)
      (plist-get org-format-latex-options :scale)
    nil)
  "Original scale for latex preview, so we can reset it.")

(defvar org-show-text-scale 4
  "Scale for text in presentation.")

(defvar org-show-current-slide-number 1
  "Holds current slide number.")

(defvar org-show-mogrify-p
  (executable-find "mogrify")
  "Determines if images are mogrified (changed size in presentation mode.")

(when org-show-mogrify-p
  (ignore-errors (require 'eimp)))

(defvar org-show-tags-column -60
  "Column position to move tags to in slide mode.")

(defvar org-show-original-tags-column org-tags-column
  "Save value so we can change back to it.")

(defvar *org-show-flyspell-mode* (when (boundp flyspell-mode)
				   flyspell-mode)
  "Whether flyspell mode is enabled at beginning of show.
Used to reset the state after the show.")

(defvar *org-show-running* nil
  "Flag for if the show is running.")

(defvar org-show-slide-list '()
  "List of slide numbers and markers to each slide.")

(defvar org-show-slide-titles '()
  "List of titles and slide numbers for each slide.")

;;* Functions
(defvar org-show-temp-images '() "List of temporary images.")

(defun org-show-execute-slide ()
  "Process slide at point.
If it contains an Emacs Lisp source block, evaluate it.
  If it contains an image, view it in a split buffer
  Else, focus on that buffer.
  Hide all drawers."
  (interactive)
  (setq org-show-presentation-file (expand-file-name (buffer-name)))
  (delete-other-windows)

  ;; make sure nothing is folded. This seems to be necessary to
  ;; prevent an error on narrowing then trying to make latex fragments
  ;; I think.
  (org-cycle '(64))

  (org-narrow-to-subtree)
  (visual-line-mode 1)
  (let ((heading-text (nth 4 (org-heading-components)))
        (org-format-latex-options (plist-put org-format-latex-options
					     :scale org-show-latex-scale)))

    (set-frame-name (format "%-180s%15s%s"
			    heading-text
			    "slide "
			    (cdr (assoc heading-text org-show-slide-titles))))

    ;; preview equations in the current subtree
    (org-preview-latex-fragment '(4))

    ;; setup the text
    (switch-to-buffer (current-buffer))
    (text-scale-set org-show-text-scale)
    (org-show-subtree)
    (org-cycle-hide-drawers t)
    (org-display-inline-images)
    (delete-other-windows)


    ;; evaluate special code blocks last as they may change the arrangement
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-babel-src-block-regexp nil t)
	(save-excursion
	  (goto-char (match-beginning 0))
	  (let* ((src (org-element-context))
		 (start (org-element-property :begin src))
		 (end (org-element-property :end src))
		 (info (save-excursion
			 (org-babel-get-src-block-info))))
	    (when (string= "emacs-lisp-slide" (car info))
	      ;; fold code
	      (org-cycle)
	      (unwind-protect
		  (eval (read (concat "(progn " (nth 1 info) ")")))))))))
    ;; clear the minibuffer
    (message "")))

    (defun org-show-next-slide ()
      "Goto next slide in presentation."
      (interactive)
      (find-file org-show-presentation-file)
      (widen)
      (if (<= (+ org-show-current-slide-number 1) (length org-show-slide-titles))
	  (progn
	    (setq org-show-current-slide-number (+ org-show-current-slide-number 1))
	    (org-show-goto-slide org-show-current-slide-number))
	(org-show-goto-slide org-show-current-slide-number)
	(message "This is the end. My only friend the end.  Jim Morrison.")))


(defun org-show-previous-slide ()
  "Goto previous slide in the list."
  (interactive)
  (find-file org-show-presentation-file)
  (widen)
  (if (> (- org-show-current-slide-number 1) 0)
      (progn
	(setq org-show-current-slide-number (- org-show-current-slide-number 1))
	(org-show-goto-slide org-show-current-slide-number))
    (org-show-goto-slide org-show-current-slide-number)
    (message "Once upon a time...")))


(defun org-show-open-slide ()
  "Start show at this slide."
  (setq org-show-presentation-file (expand-file-name (buffer-name)))
  (org-show-initialize)
  (let ((n (cdr (assoc (nth 4 (org-heading-components)) org-show-slide-titles))))
    (setq org-show-current-slide-number n)
    (org-show-goto-slide n)))


(defun org-show-initialize ()
  "Initialize the org-show.
Make slide lists for future navigation. Rerun this if you change
slide order."
  (setq  org-show-slide-titles '()
         org-show-temp-images '()
         org-show-slide-list '())

  (let ((n 0))
    (org-map-entries
     (lambda ()
       (when (string-match-p ":slide:" (or (nth 5 (org-heading-components)) ""))
	 (setq n (+ n 1))

	 (add-to-list 'org-show-slide-titles
		      (cons (nth 4 (org-heading-components)) n) t)

	 (add-to-list 'org-show-slide-list
		      (cons n (set-marker (make-marker) (point))) t))))))


(defun org-show-start-slideshow ()
  "Start the slide show, at the beginning."
  (interactive)
  (setq *org-show-running* t)
  (setq org-show-presentation-file (expand-file-name (buffer-name)))
  (beginning-of-buffer)
  (setq org-tags-column org-show-tags-column)
  (org-set-tags-command '(4) t)

  (org-show-initialize)
  ;; hide slide tags
  (save-excursion
    (while (re-search-forward ":slide:" nil t)
      (overlay-put
       (make-overlay (match-beginning 0) (match-end 0))
       'invisible 'slide)))
  ;; hide emacs-lisp-slide blocks
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-babel-src-block-regexp nil t)
      (save-excursion
	(goto-char (match-beginning 0))
	(let* ((src (org-element-context))
	       (start (org-element-property :begin src))
	       (end (org-element-property :end src))
	       (info (save-excursion
		       (org-babel-get-src-block-info))))
	  (when (string= "emacs-lisp-slide" (car info))
	    (save-restriction
	      (overlay-put
	       (make-overlay start end)
	       'invisibility 'slide)))))))
  (add-to-invisibility-spec 'slide)
  (beginning-of-buffer)
  (delete-other-windows)
  ;; (when (not org-show-mode) (org-show-mode 1))
  (setq org-show-current-slide-number 1)
  (org-show-goto-slide 1))


(defun org-show-stop-slideshow ()
  "Stop the org-show.
Try to reset the state of your Emacs. It isn't perfect ;)"
  (interactive)
  ;; make slide tag visible again
  (remove-from-invisibility-spec 'slide)

  ;; Redisplay inline images
  (org-display-inline-images)

  ;; reset latex scale
  (plist-put org-format-latex-options :scale org-show-original-latex-scale)

  ;; clean up temp images
  (mapcar (lambda (x)
	    (let ((bname (file-name-nondirectory x)))
	      (when (get-buffer bname)
                (set-buffer bname)
                (save-buffer)
		(kill-buffer bname)))

	    (when (file-exists-p x)
	      (delete-file x)))
	  org-show-temp-images)
  (setq org-show-temp-images '())

  ;; ;; clean up miscellaneous buffers
  (when (get-buffer "*Animation*") (kill-buffer "*Animation*"))

  (when org-show-presentation-file (find-file org-show-presentation-file))
  (widen)
  (text-scale-set 0)
  (delete-other-windows)
  (setq org-show-presentation-file nil)
  (setq org-show-current-slide-number 1)
  (set-frame-name (if (buffer-file-name)
		      (abbreviate-file-name (buffer-file-name))))
  (setq org-tags-column org-show-original-tags-column)
  (org-set-tags-command '(4) t)
  (setq *org-show-running* nil)
  (org-show-mode -1))


(defun org-show-goto-slide (n)
  "Goto slide N."
  (interactive "nSlide number: ")
  (message "Going to slide %s" n)
  (find-file org-show-presentation-file)
  (setq org-show-current-slide-number n)
  (widen)
  (goto-char (cdr (assoc n org-show-slide-list)))
  (org-show-execute-slide))


(defun org-show-toc ()
  "Show a table of contents for the slideshow."
  (interactive)
  (let ((links) (c-b (buffer-name)) (n))
    (save-excursion
      (widen)
      (mapcar
       (lambda (x)
	 (setq n (car x))
	 (goto-char (cdr x))
	 (add-to-list
	  'links
	  (format " [[elisp:(progn (switch-to-buffer \"%s\")(goto-char %s)(org-show-execute-slide))][%2s %s]]\n\n"
		  (marker-buffer (cdr x))
		  (marker-position (cdr x))
		  (car x)
		  (nth 4 (org-heading-components))) t))
       org-show-slide-list))

    (switch-to-buffer "*List of Slides*")
    (org-mode)
    (erase-buffer)

    (insert (mapconcat 'identity links ""))

    (use-local-map (copy-keymap org-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (kill-buffer)))))


(defun org-show-animate (strings)
  "Animate STRINGS in an *Animation* buffer."
  (switch-to-buffer (get-buffer-create
                     (or animation-buffer-name
                         "*Animation*")))
  (erase-buffer)
  (text-scale-set 6)
  (let* ((vpos (/ (- 20
		     1 ;; For the mode-line
		     (1- (length strings))
		     (length strings))
		  2))
	 (width 43)
	 hpos)
    (while strings
      (setq hpos (/ (- width (length (car strings))) 2))
      (when (> 0 hpos) (setq hpos 0))
      (when (> 0 vpos) (setq vpos 0))
      (animate-string (car strings) vpos hpos)
      (setq vpos (1+ vpos))
      (setq strings (cdr strings)))))


(defun org-show-increase-text-size (&optional arg)
  "Increase text size. Bound to \\[org-show-increase-text-size].
With prefix ARG, set `org-show-text-scale' so subsquent slides
are the same text size."
  (interactive "P")
  (text-scale-increase 1.5)
  (when arg
    (setq org-show-text-scale (* org-show-text-scale 1.5))))


(defun org-show-decrease-text-size (&optional arg)
  "Increase text size. Bound to \\[org-show-decrease-text-size].
With prefix ARG, set `org-show-text-scale' so subsquent slides
are the same text size."
  (interactive "P")
  (text-scale-decrease 1.5)
  (when arg
    (setq org-show-text-scale (/ org-show-text-scale 1.5))))

;;* Menu and org-show-mode

(defvar org-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [next] 'org-show-next-slide)
    (define-key map [prior] 'org-show-previous-slide)

    (define-key map [f5] 'org-show-start-slideshow)
    (define-key map [f6] 'org-show-execute-slide)
    (define-key map (kbd "C--") 'org-show-decrease-text-size)
    (define-key map (kbd "C-=") 'org-show-increase-text-size)
    (define-key map (kbd "\e\eg") 'org-show-goto-slide)
    (define-key map (kbd "\e\et") 'org-show-toc)
    (define-key map (kbd "\e\eq") 'org-show-stop-slideshow)
    map)
  "Keymap for function ‘org-show-mode’.")


(define-minor-mode org-show-mode
  "Minor mode for org-show

\\{org-show-mode-map}"
  :init-value nil
  :lighter " org-show"
  :global t
  :keymap org-show-mode-map
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minor-Mode-Conventions.html
  (let ((enable org-show-mode))
    (if enable
	;; do-enable
	(progn
	  (when flyspell-mode
	    (setq *org-show-flyspell-mode* t)
	    (flyspell-mode-off)
	    (setq *org-show-flyspell-mode* nil))

	  (easy-menu-define my-menu org-show-mode-map "My own menu"
	    '("org-show"
	      ["Start slide show" org-show-start-slideshow t]
	      ["Next slide" org-show-next-slide t]
	      ["Previous slide" org-show-previous-slide t]
	      ["Open this slide" org-show-open-slide t]
	      ["Goto slide" org-show-goto-slide t]
	      ["Table of contents" org-show-toc t]
	      ["Stop slide show"  org-show-stop-slideshow t])))
      ;; restore flyspell
      (when  *org-show-flyspell-mode*
	(flyspell-mode-on))

      ;; close the show.
      (when *org-show-running*
	(org-show-stop-slideshow)))))

;;* Make emacs-lisp-slide blocks executable

;; this is tricker than I thought. It seems babel usually runs in some
;; sub-process and I need the code to be executed in the current buffer.
(defun org-babel-execute:emacs-lisp-slide (body params)
  (message "%S" body)
  (let ((src (org-element-context)))
    (save-excursion
      (goto-char (org-element-property :begin src))
      (re-search-forward (org-element-property :value src))
      (eval-region (match-beginning 0) (match-end 0)))))

;; * help
(defun org-show-help ()
  "Open the help file."
  (interactive)
  (find-file (expand-file-name "org-show.org"
			       (file-name-directory
				(locate-library "org-show")))))



;;* The end

(provide 'org-show)

;;; org-show.el ends here
