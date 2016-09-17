;;; org-ov-highlighter.el --- Highlight text in org-mode with overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/scimax/org-ov-highlighter.el

;; Version: 0.1.0
;; Keywords: org-mode, highlight
;; Package-Requires: ((hydra "0.13.2") (dash) (s))
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
;; org-ov-highlighter provides a lightweight way to highlight text and put notes on
;; them in org-mode.

;; There is a hydra menu to make accessing all the commands below convenient:
;; `org-ov-highlighter/body'. I suggest you bind it to a key like H-h.

;; You can select text, and run these commands to add highlighting to it:
;; `org-ov-highlight' will prompt you for a color, and highlight with it.
;; These convenience functions skip the color prompt.
;; `org-ov-highlight-yellow'
;; `org-ov-highlight-blue'
;; `org-ov-highlight-green'
;; `org-ov-highlight-pink'

;; `org-ov-highlight-toggle-mouse-highlight' makes it possible to highlight text in
;; green using the mouse. Use a prefix arg to select the color.

;; `org-ov-highlight-note' will prompt you for a color, highlight the text and add
;; a Note to it as a tooltip. Notes are still experimental. You can edit a note
;; with `org-ov-highlight-note-edit'.

;; You can list all the highlights with `org-ov-highlight-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight with `org-ov-highlight-clear'.
;; Remove all the highlights with `org-ov-highlight-clear-all'.

;; org-ov-highlighter uses a local save-buffer-hook to update the data when you
;; save the buffer. It also uses local file variables to load the highlights
;; when you open the file.

;; Known issues:

;; - Highlights do not export in org-mode. They are not part of org-syntax, so
;; you would have to use a preprocessing hook function to make it work. 

;; Highlights are not visible everywhere. So far they don't seem to work in:
;; - tables, or code-blocks.
;; - equations
;; - probably other things that are fontified by org-mode.
;; - Highlights don't seem to copy and paste. This is related to the text
;;   properties I think. I am not sure how to fix it.

(require 'hydra)
(require 'ov)

;;; Code:
(defgroup org-ov-highlighter nil
  "Customization group for `org-ov-highlighter'."
  :tag "org-ov-highlighter")


(defcustom org-ov-highlight-mouse-color "Darkolivegreen1"
  "Color to use for mouse highlighting."
  :type 'string
  :group 'org-ov-highlighter)


;; * Highlight text and functions

(defun org-ov-highlight-color-chooser ()
  "Interactively choose a color with completion."
  (plist-get (get-text-property
	      0 'face
	      (completing-read
	       "Color: "
	       (progn
		 (save-selected-window
		   (list-colors-display))
		 (prog1
		     (with-current-buffer (get-buffer "*Colors*")
		       (mapcar (lambda (line)
				 (append (list line) (s-split " " line t)))
			       (s-split "\n" (buffer-string))))
		   (kill-buffer "*Colors*")))))
	     :background))


;;;###autoload
(defun org-ov-highlight (beg end &optional color)
  "Highlight region from BEG to END with COLOR.
COLOR is selected from `org-ov-highlight-color-chooser' when run interactively."
  (interactive "r")
  (unless color
    (setq color (org-ov-highlight-color-chooser)))

  ;; add a local hook
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t)
  
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face `(:background ,color))
    (overlay-put ov 'org-ov-highlight t)
    (set-buffer-modified-p t)
    ov))


(defun org-ov-get-highlight-overlays ()
  "Return a list of the highlight overlays.
The list is from first to last."
  (reverse (-filter (lambda (ov) (overlay-get ov 'org-ov-highlight)) 
		    (overlays-in (point-min) (point-max)))))

;;;###autoload
(defun org-ov-highlight-list ()
  "Make a list of highlighted text in another buffer."
  (interactive)
  (let* ((links (mapcar
		 (lambda (entry)
		   (format "[[elisp:(progn (find-file-other-window \"%s\")(goto-char %s))][link]] %-40s|%s\n"
			   (buffer-file-name)
			   (nth 0 entry)
			   (propertize
			    (buffer-substring (nth 0 entry) (nth 1 entry))
			    'font-lock-face `(:background ,(nth 2 entry)))
			   (nth 3 entry)))
		 (org-ov-highlight-get-highlights))))
    (if links
	(progn
	  (when (= (length (window-list)) 1)
	    (split-window-horizontally))
	  (switch-to-buffer-other-window "*highlights*") (org-mode)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "Click on link to jump to the position. Press q to quit.\n\n")

	  (dolist (link links)
	    (insert link))
	  (use-local-map (copy-keymap org-mode-map))
	  (local-set-key "q"
			 #'(lambda ()
			     (interactive)
			     (delete-window)))
	  (setq buffer-read-only t))
      (message "No highlights found."))))


;;;###autoload
(defun org-ov-highlight-yellow ()
  "Highlight region in yellow."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Yellow"))


;;;###autoload
(defun org-ov-highlight-blue ()
  "Highlight region in blue."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "LightBlue"))


;;;###autoload
(defun org-ov-highlight-pink ()
  "Highlight region in pink."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Pink"))


;;;###autoload
(defun org-ov-highlight-green ()
  "Highlight region in green."
  (interactive)
  (org-ov-highlight (region-beginning) (region-end) "Darkolivegreen1"))


(defvar org-ov-highlight-mouse nil
  "Stores if highlight mouse mode is active.")


;; create the advice for use later
(defadvice mouse-set-region (after org-ov-highlight () disable)
  "Advice for mouse highlighting."
  (when (eq major-mode 'org-mode)
    (org-ov-highlight (region-beginning) (region-end)
		      org-ov-highlight-mouse-color)))


;;;###autoload
(defun org-ov-highlight-toggle-mouse-highlight (arg)
  "Toggle mouse highlighting.
The default color is `org-ov-highlight-mouse-color'. Use a prefix
ARG to select a different color and save it."
  (interactive "P")
  (when arg
    (setq org-ov-highlight-mouse-color (org-ov-highlight-color-chooser)))
  
  (if org-ov-highlight-mouse
      ;; Turn it off
      (progn (ad-disable-advice 'mouse-set-region 'after 'org-ov-highlight)
	     (ad-deactivate 'mouse-set-region)
	     (setq org-ov-highlight-mouse nil)
	     (message "Mouse highlighting off."))
    (ad-enable-advice 'mouse-set-region 'after 'org-ov-highlight)
    (ad-activate 'mouse-set-region)
    (setq org-ov-highlight-mouse t)
    (message "Mouse highlighting on.")))


;;;###autoload
(defun org-ov-highlight-note (beg end &optional color note)
  "Highlight selected text from BEG to END with COLOR.
Add NOTE to it as a tooltip. If no text is selected, insert \" note \"
and propertize it."
  (interactive "r")
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t)

  (unless color (setq color (org-ov-highlight-color-chooser)))
  (unless note (setq note (read-input "Note: ")))

  (let ((ov (org-ov-highlight beg end color)))
    (overlay-put ov 'help-echo note)
    (set-buffer-modified-p t)))


;;;###autoload
(defun org-ov-highlight-note-edit (new-note)
  "Set tooltip of highlight at point to NEW-NOTE."
  (interactive (list (read-input
		      "Note: "
		      (overlay-get (ov-at) 'help-echo))))
  (overlay-put (ov-at) 
	       'help-echo new-note))


;;;###autoload
(defun org-ov-highlight-clear ()
  "Clear highlight at point."
  (interactive)
  (delete-overlay (ov-at))
  (set-buffer-modified-p t))


;;;###autoload
(defun org-ov-highlight-clear-all ()
  "Clear all highlighted text.
They are really deleted when you save the buffer."
  (interactive)
  (mapc 'delete-overlay (org-ov-get-highlight-overlays)) 
  (when (get-buffer "*highlights*")
    (kill-buffer "*highlights*"))
  (set-buffer-modified-p t)
  (save-buffer))


;;;###autoload
(defhydra org-ov-highlighter (:color blue) "highlighter"
  ("b" org-ov-highlight-blue "blue")
  ("g" org-ov-highlight-green "Green")
  ("p" org-ov-highlight-pink "Pink")
  ;; define as many special colors as you like.
  ("s" (org-ov-highlight (region-beginning) (region-end) "Lightsalmon1") "Salmon")
  ("y" org-ov-highlight-yellow "yellow")
  ("c" org-ov-highlight "Choose color")
  ("n" (org-ov-highlight-note (region-beginning) (region-end) "Thistle") "Note")
  ("N" org-ov-highlight-note "Note (c)")
  ("m" org-ov-highlight-toggle-mouse-highlight "Toggle mouse")
  ("e" org-ov-highlight-note-edit "Edit note")

  ;; Grading/feedback options
  ("t" org-ov-highlight-typo "Typo")
  ("f" org-ov-highlight-feedback "Feedback note")
  
  ("l" org-ov-highlight-list "List highlights")
  ("d" org-ov-highlight-clear "Delete")
  ("D" org-ov-highlight-clear-all "Delete All"))


(defun org-ov-highlighter-menu ()
  "Add org-ov-highlighter to the Org menu."
  (easy-menu-change
   '("Org") "Highlighter"
   '(["Highlight (B)" org-ov-highlight-blue]
     ["Highlight (G)" org-ov-highlight-green]
     ["Highlight (P)" org-ov-highlight-pink]
     ["Highlight (Y)" org-ov-highlight-yellow]
     ["Highlight note" org-ov-highlight-note]
     ["List highlights" org-ov-highlight-list]
     ["Delete highlight" org-ov-highlight-clear]
     ["Delete highlights" org-ov-highlight-clear-all])
   "Show/Hide")
  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-ov-highlighter-menu)


;; * Save and load functions
(defun org-ov-highlight-get-highlights ()
  "Returns a list of (beg end color note) for the overlays."
  (mapcar (lambda (ov)
	    (list (overlay-start ov)
		  (overlay-end ov)
		  (plist-get  (overlay-get ov 'face) :background)
		  (overlay-get ov 'help-echo)))
	  (org-ov-get-highlight-overlays)))


(defun org-ov-highlight-save ()
  "Save highlight information.
Data is saved in an org-section in the document."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* org-ov-highlighter data" nil 'mv)
      (insert "* org-ov-highlighter data :noexport:
  :PROPERTIES:
  :VISIBILITY: folded
  :ID: org-ov-highlighter-data
  :END:\nDo not delete this section. It stores information about the highlights in this document. Any information in this section may be deleted if you remove the highlights in this document.\n#+name: org-ov-highlighter-data\n#+BEGIN_SRC emacs-lisp :results code value replace\n(org-ov-highlight-get-highlights)\n#+END_SRC")

      (add-file-local-variable 'eval '(progn (require 'org-ov-highlighter) (org-ov-highlight-load))))
    (org-save-outline-visibility nil
      (org-babel-goto-named-src-block "org-ov-highlighter-data")
      (org-babel-execute-src-block)

      ;; delete section if there are no highlights
      (when (null (org-ov-highlight-get-highlights))
	(outline-previous-heading)
	(let ((hl (org-element-context)))
	  (setf (buffer-substring (org-element-property :begin hl)
				  (org-element-property :end hl))
		"")))
      (let ((after-save-hook '()))
	(save-buffer)
	(org-set-visibility-according-to-property)))))


(defun org-ov-highlight-load ()
  "Load and apply highlighted text."
  (interactive)
  
  (org-babel-goto-named-result "org-ov-highlighter-data")
  (let ((hls (read (org-element-property :value (org-element-context)))))
    (mapc
     (lambda (entry)
       (let ((beg (nth 0 entry))
	     (end (nth 1 entry))
	     (color (nth 2 entry))
	     (help-echo (nth 3 entry)))
	 (if help-echo
	     (org-ov-highlight-note beg end color help-echo)
	   (org-ov-highlight beg end color))))
     hls))
  (add-hook 'after-save-hook 'org-ov-highlight-save nil t))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(progn (org-ov-highlight-load)))


;; * Feedback functions
;;;###autoload
(defun org-ov-highlight-typo ()
  "Add a typo highlight."
  (interactive)
  (let* ((r1 (progn (re-search-backward "\\<") (set-mark (point)) (point)))
	 (r2 (progn (re-search-forward "\\>") (point))))
    (org-ov-highlight-note r1 r2 "PaleVioletRed1" "typo")))


;;;###autoload
(defun org-ov-highlight-feedback ()
  "Add a feedback highlight."
  (interactive)
  (let (r1 r2 comment)
    (if (region-active-p)
	(setq r1 (region-beginning)
	      r2 (region-end))
      ;; No region, so we make one
      (setq  r1 (progn (re-search-backward "\\<") (set-mark (point)) (point))
	     r2 (progn (re-search-forward "\\>") (point))))

    (setq comment (read-input "Comment: "))
    (org-ov-highlight-note r1 r2 "LightBlue1" comment)))

(provide 'org-ov-highlighter)

;;; org-ov-highlighter.el ends here
