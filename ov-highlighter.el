;;; ov-highlighter.el --- Highlight text with overlays  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/scimax/ov-highlighter.el

;; Version: 0.1.0
;; Keywords:  highlight
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
;; ov-highlighter provides a lightweight way to highlight text and put notes on
;; them using overlays that are locally stored.

;; There is a hydra menu to make accessing all the commands below convenient:
;; `ov-highlighter/body'. I suggest you bind it to a key like H-h.

;; You can select text, and run these commands to add highlighting to it:
;; `ov-highlight-yellow'
;; `ov-highlight-blue'
;; `ov-highlight-green'
;; `ov-highlight-pink'
;; `ov-highlight-color' to choose a color for the background.
;; `ov-highlight-foreground' to choose the color for the font.

;; There are also some markup overlays: `ov-highlight-bold', and italic,
;; underline, strikethrough, and box overlays.

;; You can increase/decrease the font size with
;; `ov-highlight-increase-font-size' and `ov-highlight-decrease-font-size', and
;; you can also change the font `ov-highlight-font'.

;; `ov-highlight-comment' will highlight the text and add a note to it as a
;; tooltip. You can edit a note by clicking on it.

;; You can list all the highlights with `ov-highlight-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight under the cursor with `ov-highlight-clear'.
;; Remove all the highlights with `ov-highlight-clear-all'.

;; You can convert the highlights to html with `ov-highlight-html' which will
;; convert the buffer to an html file and open it in a browser.

;; Define your own custom highlight functions with `ov-make-highlight'. See the
;; predefined functions for examples.

;; ov-highlighter uses a local save-buffer-hook to update the data when you save
;; the buffer. It also uses local file variables to load the highlights when you
;; open the file. The data is saved in a single line. I don't know what the
;; limitation of this is.

;; Known issues: You cannot cut and paste these highlights. Fixing this would
;; involve changing the kill and yank functions to capture overlays in the kill
;; region, and remake them in the yanked region.

(require 'hydra)
(require 'ov)

;;; Code:

(defmacro ov-make-highlight (label face &rest properties)
  "Create a user-defined highlight function.
The function will be called `ov-highlight-LABEL', and it will
apply FACE to the selected region. FACE can be an anonymous face,
or a function that returns one. PROPERTIES is a list of symbols
and properties. If the property is a function, it will be
evaluated. The function takes no arguments."
  `(defun ,(intern (format "ov-highlight-%s" label)) (beg end)
     ,(format "Apply the face %S to the region selected by BEG and END" face)
     (interactive "r") 
     (flyspell-delete-region-overlays beg end)
     ;; add a local hook to make sure it gets saved
     (add-hook 'before-save-hook 'ov-highlight-save nil t)
     (let ((face ,face)
	   (properties (quote ,properties))
	   (bounds (bounds-of-thing-at-point 'word))) 
       (if (and  (ov-at) (overlay-get (ov-at) 'ov-highlight))
	   ;; add face properties to current overlay face properties.
	   (let* ((current-properties (overlay-properties (ov-at)))
		  (cf (overlay-get (ov-at) 'face))
		  (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					   if v
					   append
					   (list p v))
			cf))
		  prop val) 
	     (while face
	       (setq prop (pop face)
		     val (pop face))
	       (plist-put fp prop val)) 
	     (overlay-put (ov-at) 'face fp)
	     (while properties
	       (setq prop (pop properties)
		     val (pop properties))
	       (overlay-put (ov-at) prop val)))
	 ;; make new overlay
	 (let* ((beg (if (region-active-p) (region-beginning) (car bounds)))
		(end (if (region-active-p) (region-end) (cdr bounds)))
		(ov (make-overlay beg end)))
	   (overlay-put ov 'face (if (functionp face)
				     (funcall face)
				   face)) 
	   (while properties 
	     (setq prop (pop properties)
		   val (pop properties))
	     
	     (overlay-put ov prop (if (functionp val)
				      (funcall val)
				    val)))
	   (overlay-put ov 'ov-highlight t)
	   (set-buffer-modified-p t)
	   (let ((p (point)))
	     (when (mark)
	       (deactivate-mark))
	     (goto-char p))
	   ;; refresh highlights if needed
	   (let ((buf (get-buffer "*ov-highlights*")))
	     (when (and ov-highlight-source
			(buffer-live-p ov-highlight-source)
			buf) 
	       (with-current-buffer buf
		 (ov-highlight-refresh-list))))
	   ov))))) 


;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
;; ** highlighters
;; font effects
(ov-make-highlight "bold" '(:weight bold))
(ov-make-highlight "italic" '(:slant italic))
(ov-make-highlight "underline" '(:underline t))
(ov-make-highlight "strikethrough" '(:strike-through t))
(ov-make-highlight "box" '(:box t))
(ov-make-highlight "redbox" '(:box (:line-width 2 :color "red" :style released-button)))
(ov-make-highlight "comic-sans" '(:family "Comic Sans MS"))

(ov-make-highlight "yellow" '(:background "Yellow"))
(ov-make-highlight "blue" '(:background "LightBlue"))
(ov-make-highlight "pink" '(:background "Pink"))
(ov-make-highlight "green" '(:background "Darkolivegreen1"))

;; A user-selected color
(ov-make-highlight "color" (lambda ()
			     (list :background
				   (plist-get
				    (get-text-property
				     0 'face
				     (completing-read
				      "Color: "
				      (progn
					(save-selected-window
					  (list-colors-display))
					(prog1
					    (with-current-buffer (get-buffer "*Colors*")
					      (mapcar (lambda (line)
							(append (list line)
								(s-split " " line t)))
						      (s-split "\n" (buffer-string))))
					  (kill-buffer "*Colors*")))))
				    :background))))

;; Change font color
(ov-make-highlight "foreground" (lambda ()
				  (list :foreground
					(plist-get (get-text-property
						    0 'face
						    (completing-read
						     "Color: "
						     (progn
						       (save-window-excursion
							 (list-colors-display))
						       (prog1
							   (with-current-buffer
							       (get-buffer "*Colors*")
							     (mapcar (lambda (line)
								       (append
									(list line)
									(s-split " " line t)))
								     (s-split
								      "\n"
								      (buffer-string))))
							 (kill-buffer "*Colors*")))))
						   :background))))

(defvar *ov-window-configuration* nil
  "Stores the window configuration so we can later restore it.")

(ov-make-highlight "comment" '(:background "Orange1")
		   mouse-face highlight
		   local-map (lambda ()
			       (let ((map (make-sparse-keymap))
				     (edit-func
				      (lambda ()
					(interactive)
					(setq *ov-window-configuration*
					      (current-window-configuration)) 
					(let ((cb (current-buffer))
					      (current-note (overlay-get (ov-at) 'help-echo)))
					  (switch-to-buffer "*ov-note*")
					  (erase-buffer)
					  (org-mode)
					  (insert (or current-note ""))
					  (let ((map (make-sparse-keymap))) 
					    (setq header-line-format
						  "Click here or type s-<return> to finish. C-x k to cancel."))
					  (local-set-key
					   (kbd "C-x k")
					   `(lambda ()
					      (interactive)
					      (kill-buffer))) 
					  (local-set-key
					   (kbd "s-<return>")
					   `(lambda ()
					      (interactive)
					      (let ((tooltip (buffer-substring-no-properties
							      (point-min) (point-max))))
						(kill-buffer)
						(set-window-configuration
						 *ov-window-configuration*)
						(setq n*ov-window-configuration* nil)
						(overlay-put (ov-at) 'help-echo tooltip))))))))
				 (define-key map [mouse-1] edit-func)
				 map))
		   help-echo (lambda ()
			       (setq *ov-window-configuration* (current-window-configuration)) 
			       (switch-to-buffer "*ov-note*")
			       (erase-buffer)
			       (org-mode)
			       (let ((map (make-sparse-keymap))) 
				 (setq header-line-format
				       (propertize
					"Enter comment. Type s-<return> to finish. C-x k to cancel."
					'local-map map)))

			       (use-local-map (copy-keymap org-mode-map))
			       
			       ;; Cancel
			       (local-set-key
				(kbd "C-x k")
				`(lambda ()
				   (interactive)
				   (kill-buffer)))

			       ;; Finish comment
			       (local-set-key
				(kbd "s-<return>")
				`(lambda ()
				   (interactive)
				   (let ((tooltip (buffer-substring-no-properties
						   (point-min) (point-max))))
				     (kill-buffer)
				     (set-window-configuration *ov-window-configuration*)
				     (setq *ov-window-configuration* nil)
				     (overlay-put (if (ov-at)
						      (ov-at)
						    (ov-at (- (point) 1)))
						  'help-echo tooltip))))))
;; font color
(ov-make-highlight "red-fg" '(:foreground "red"))

(ov-make-highlight "typo" '(:background "PaleVioletRed1") help-echo "tpyo")

(ov-make-highlight "font" (lambda ()
			    (list :family
				  (completing-read "Font: " (font-family-list)))))

;; ** font size changes
(defun ov-highlight-increase-font-size (&optional arg)
  "Increase the font size of the overlay at point.
When no overlay make one for the region or word at point.
With numeric prefix set font to that size."
  (interactive "P") 
  (let ((bounds (if (region-active-p)
		    (cons (region-beginning) (region-end))
		  (bounds-of-thing-at-point 'word))))
    (flyspell-delete-region-overlays (car bounds) (cdr bounds))
    (add-hook 'before-save-hook 'ov-highlight-save nil t)
    (if (ov-at)
	;; overlay at point, update it
	(progn
	  (let* ((current-properties (overlay-properties (ov-at)))
		 (cf (overlay-get (ov-at) 'face))
		 (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					  if v
					  append
					  (list p v))
		       cf))
		 (ht (or arg (if (plist-get fp :height)
				 (floor (* 1.1 (plist-get fp :height)))
			       (* 1.1 (face-attribute 'default :height)))))
		 prop val) 
	    (plist-put fp :height ht)
	    (message "Set height to %s." ht)
	    (overlay-put (ov-at) 'face fp)))
      (let ((ov (make-overlay (car bounds) (cdr bounds)))
	    (ht (or arg (floor (* 1.1 (face-attribute 'default :height))))))
	(overlay-put ov 'face (list :height ht))
	(message "Set height to %s." ht)
	(overlay-put ov 'ov-highlight t)
	(set-buffer-modified-p t)
	(let ((p (point)))
	  (when (mark)
	    (deactivate-mark))
	  (goto-char p))

	(let ((buf (get-buffer "*ov-highlights*")))
	  (when (and ov-highlight-source
		     (buffer-live-p ov-highlight-source)
		     buf) 
	    (with-current-buffer buf
	      (ov-highlight-refresh-list))))
	ov))))


(defun ov-highlight-decrease-font-size (&optional arg)
  "Decrease the font size of the overlay at point.
When no overlay make one for the region or word at point.
With numeric prefix set font to that size."
  (interactive "P") 
  (let ((bounds (if (region-active-p)
		    (cons (region-beginning) (region-end))
		  (bounds-of-thing-at-point 'word))))
    (flyspell-delete-region-overlays (car bounds) (cdr bounds))
    (add-hook 'before-save-hook 'ov-highlight-save nil t)
    (if (ov-at)
	;; overlay at point, update it
	(progn
	  (let* ((current-properties (overlay-properties (ov-at)))
		 (cf (overlay-get (ov-at) 'face))
		 (fp (if (facep cf) (loop for (p . v) in (face-all-attributes cf)
					  if (not (eq 'unspecified v))
					  append
					  (list p v))
		       cf))
		 (ht (or arg (if (plist-get fp :height)
				 (floor (* 0.9 (plist-get fp :height)))
			       (* 0.9 (face-attribute 'default :height)))))
		 prop val) 
	    (plist-put fp :height ht)
	    (overlay-put (ov-at) 'face fp)))
      (let ((ov (make-overlay (car bounds) (cdr bounds)))
	    (ht (or arg (floor (* 0.9 (face-attribute 'default :height))))))
	(overlay-put ov 'face (list :height ht))
	(message "Set :height to %s." ht)
	(overlay-put ov 'ov-highlight t)
	(set-buffer-modified-p t)
	(let ((p (point)))
	  (when (mark)
	    (deactivate-mark))
	  (goto-char p))
	(let ((buf (get-buffer "*ov-highlights*")))
	  (when (and ov-highlight-source
		     (buffer-live-p ov-highlight-source)
		     buf) 
	    (with-current-buffer buf
	      (ov-highlight-refresh-list))))
	ov))))


;; * Clearing highlights
;;;###autoload
(defun ov-highlight-clear ()
  "Clear highlight at point."
  (interactive)
  (delete-overlay (ov-at))
  (set-buffer-modified-p t)
  (let ((buf (get-buffer "*ov-highlights*")))
    (when buf
      (with-current-buffer buf
	(ov-highlight-refresh-list)))))


;;;###autoload
(defun ov-highlight-clear-all ()
  "Clear all highlighted text.
They are really deleted when you save the buffer."
  (interactive)
  (mapc 'delete-overlay (ov-get-highlight-overlays))
  (set-buffer-modified-p t))


;; * List highlights

(defun ov-get-highlight-overlays ()
  "Return a list of the highlight overlays.
The list is from first to last."
  (reverse (-filter (lambda (ov) (overlay-get ov 'ov-highlight)) 
		    (overlays-in (point-min) (point-max)))))


(defvar ov-highlight-source nil
  "A cons cell of the buffer to get highlights from.")


(defvar ov-highlight-window-configuration nil
  "Stores the window configuration.")


(defun ov-highlight-display ()
  "Display all highlights in the current buffer in a tabulated list form."
  (interactive)
  (save-buffer)
  (let ((buf (current-buffer)))
    (setq ov-highlight-window-configuration (current-window-configuration))
    (switch-to-buffer-other-window
     (get-buffer-create "*ov-highlights*"))
    (ov-highlight-list-mode)
    (setq ov-highlight-source buf)
    (ov-highlight-refresh-list)))


(defun ov-highlight-refresh-list ()
  "Refresh the list of highlights in the buffer."
  (let ((highlights) (entries))
    (with-current-buffer ov-highlight-source
      (setq highlights (ov-get-highlight-overlays))
      (setq entries (loop for ov in highlights 
			  collect
			  (list
			   nil		;id
			   (vector
			    (cons (buffer-substring (ov-beg ov) (ov-end ov))
				  (list 'face (overlay-get ov 'face)
					'ov-position (ov-beg ov)))
			    ;; the help-echo
			    (replace-regexp-in-string
			     "\n" " "
			     (or (overlay-get ov 'help-echo) "")))))))
    (setq tabulated-list-entries entries
	  tabulated-list-format (vector '("Highlight" 40 t) '("Note" 40 t)))
    (tabulated-list-init-header)
    (tabulated-list-print)))


(defun ov-highlight-jump ()
  "In list mode, jump to the highlight."
  (interactive) 
  (let ((pos (get-text-property (line-beginning-position) 'ov-position)))
    (when pos
      (switch-to-buffer-other-window ov-highlight-source)
      (goto-char pos)
      (org-show-entry))))


(defvar ov-highlight-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key tabulated-list-mode-map (kbd "q") (lambda ()
						    (interactive)
						    (kill-buffer)
						    (set-window-configuration
						     ov-highlight-window-configuration)))
    (define-key map (kbd "r") (lambda () (interactive) (ov-highlight-refresh-list)))
    (define-key map (kbd "o") 'ov-highlight-jump)
    (define-key map (kbd "[mouse-1]") 'ov-highlight-jump)
    (define-key map (kbd "<return>") 'ov-highlight-jump)
    (define-key map (kbd "k")
      (lambda ()
	(interactive)
	(let ((p (point)))
	  (save-window-excursion
	    (ov-highlight-jump)
	    (ov-highlight-clear))
	  (goto-char p))))
    (define-key map (kbd "e")
      (lambda ()
	(interactive)
	(ov-highlight-jump)
	(setq *ov-window-configuration* (current-window-configuration)) 
	(let ((cb (current-buffer))
	      (current-note (overlay-get (ov-at) 'help-echo)))
	  (switch-to-buffer "*ov-note*")
	  (erase-buffer)
	  (org-mode)
	  (insert (or current-note ""))
	  (let ((map (make-sparse-keymap))) 
	    (setq header-line-format
		  "Click here or type s-<return> to finish. C-x k to cancel."))
	  (local-set-key
	   (kbd "C-x k")
	   `(lambda ()
	      (interactive)
	      (kill-buffer))) 
	  (local-set-key
	   (kbd "s-<return>")
	   `(lambda ()
	      (interactive)
	      (let ((tooltip (buffer-substring-no-properties (point-min) (point-max))))
		(kill-buffer)
		(set-window-configuration *ov-window-configuration*)
		(setq *ov-window-configuration* nil)
		(overlay-put (ov-at) 'help-echo tooltip)
		(let ((buf (get-buffer "*ov-highlights*")))
		  (when (and ov-highlight-source
			     (buffer-live-p ov-highlight-source)
			     buf) 
		    (with-current-buffer buf
		      (ov-highlight-refresh-list))))))))))
    map)
  "Local keymap for `ov-highlight-list-mode.")


(define-derived-mode ov-highlight-list-mode
  tabulated-list-mode "ov-highlights"
  "Mode for viewing ov-highlights as a tabular list.
\\{ov-highlight-list-mode-map}"
  (setq tabulated-list-sort-key nil) 
  (add-hook 'tabulated-list-revert-hook
	    #'ov-highlight-refresh-list))


;; * Save and load functions
(defun ov-highlight-get-highlights ()
  "Returns a list of (beg end color note) for the overlays."
  (mapcar (lambda (ov)
	    (list (overlay-start ov)
		  (overlay-end ov)
		  (overlay-properties ov)))
	  (ov-get-highlight-overlays)))


(defun ov-highlight-read-data ()
  "Reads the data saved in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (format "^%s.*?ov-highlight-data: \\(.*\\)" comment-start))
    (read (org-link-unescape (match-string 1)))))


(defun ov-highlight-load ()
  "Load and apply highlighted text."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "#  ov-highlight-data: \\(.*\\)" nil t)
      (setq ov-highlight-data (match-string 1)))
    (mapc
     (lambda (entry)
       (let ((beg (nth 0 entry))
	     (end (nth 1 entry))
	     (properties (nth 2 entry)))
	 (flyspell-delete-region-overlays beg end)
	 (let ((ov (make-overlay beg end)))
	   (apply 'ov-put ov properties) 
	   (set-buffer-modified-p t)
	   (let ((p (point)))
	     (when (mark)
	       (deactivate-mark))
	     (goto-char p)) 
	   ov))) 
     (read (org-link-unescape (or ov-highlight-data "")))))
  (add-hook 'before-save-hook 'ov-highlight-save nil t)
  ;; loading marks the buffer as modified, because the overlay functions mark
  ;; it, but it isn't. We mark it unmodified here.
  (set-buffer-modified-p nil))


(defun ov-highlight-save ()
  "Save highlight information.
Data is saved in comment in the document."
  (let ((data (ov-highlight-get-highlights)))

    (save-restriction
      (widen)
      (save-excursion
	(goto-char (point-min))
	(unless (re-search-forward "eval: (ov-highlight-load)" nil 'mv)
	  (add-file-local-variable 'eval '(ov-highlight-load)))

	(goto-char (point-min))
	(if (re-search-forward "#  ov-highlight-data: \\(.*\\)" nil 'mv)
	    (progn
	      (setf (buffer-substring (match-beginning 0) (match-end 0)) "")
	      (insert (format "#  ov-highlight-data: %s" (org-link-escape (format "%S" data)))))
	  (re-search-backward "Local Variables:")
	  (goto-char (line-beginning-position))
	  (insert (format "#  ov-highlight-data: %s\n\n" (org-link-escape (format "%S" data)))))))
    
    (unless data
      ;; cleanup if we have no highlights
      (remove-hook 'before-save-hook 'ov-highlight-save t)
      (delete-file-local-variable 'ov-highlight-data))))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(ov-highlight-load))


;; * The hydra menu

(defhydra ov-highlighter (:color blue :hint nil) 
  "
^Highlight^       ^Markup^       ^Font^        ^Edit^       ^List^
_g_: green        _b_: bold      _[_: decrease  _t_: typo    _l_: list
_p_: pink         _i_: italic    _]_: increase  _m_: comment _k_: clear
_y_: yellow       _u_: underline _F_: Change    ^ ^          _K_: clear all
_c_: choose       _s_: strike
_f_: foreground   _x_: box
"
  ("g" ov-highlight-green "green")
  ("p" ov-highlight-pink "pink") 
  ("y" ov-highlight-yellow "yellow")
  ("c" ov-highlight "Choose color")
  ("f" ov-highlight-foreground "Foreground")

  ("[" ov-highlight-decrease-font-size "Make size smaller" :color red)
  ("]" ov-highlight-increase-font-size "Make size bigger" :color red)
  ("F" ov-highlight-font "Font")

  ("b" ov-highlight-bold "Bold")
  ("i" ov-highlight-italic "Italic")
  ("u" ov-highlight-underline "Underline")
  ("s" ov-highlight-strikethrough "Strikethrough")
  ("x" ov-highlight-box "Box")

  ;; editmark/feedback options
  ("t" ov-highlight-typo "Typo") 
  ("m" ov-highlight-comment "Comment highlight")
  
  ("l" ov-highlight-display "List highlights")
  ("k" ov-highlight-clear "Clear highlight")
  ("K" ov-highlight-clear-all "Clear all highlights")
  ("q" quit-window "quit"))

;; * markup overlays

(defun ov-bold (beg end)
  "Bold the selected text

It returns the created overlay."
  (interactive "r")
  
  (flyspell-delete-region-overlays beg end)
  ;; add a local hook to make sure it gets saved
  (add-hook 'before-save-hook 'ov-highlight-save nil t)
  
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face `(:weight bold))
    (overlay-put ov 'ov-highlight t)
    (set-buffer-modified-p t)
    (let ((p (point)))
      (when (mark)
	(deactivate-mark))
      (goto-char p)) 
    ov))


;; * Copy, cut and paste
(defvar ov-highlight-copy-data  '()
  "p-list of overlays to recreate on copying and pasting.")

(defun ov-highlight-copy (beg end)
  "Copy the region from BEG to END with overlays for pasting with `ov-highlight-paste'.
Notes: we save the starts and ends separately, because the ends
get lost when you cut overlays."
  (interactive "r")
  (let ((ovs (loop for ov in (overlays-in beg end)
		   if (overlay-get ov 'ov-highlight)
		   collect (copy-overlay ov))))
    (setq ov-highlight-copy-data (list
				  :point beg
				  :text (buffer-substring beg end)
				  :overlays ovs 
				  :starts (mapcar #'ov-beg ovs)
				  :ends (mapcar #'ov-end ovs)))))


(defun ov-highlight-paste ()
  "Paste the data from `ov-highlight-copy-data' at point."
  (interactive)
  (let ((p (point)))
    (let* ((text (plist-get ov-highlight-copy-data :text))
	   (ovs (plist-get ov-highlight-copy-data :overlays))
	   (starts (plist-get ov-highlight-copy-data :starts))
	   (ends (plist-get ov-highlight-copy-data :ends)))
      (save-excursion (insert text))
      (loop for ov in ovs for beg in starts for end in ends
	    if (overlay-get ov 'ov-highlight)
	    do
	    (let* ((delta (- beg (plist-get ov-highlight-copy-data :point)))
		   (nov (make-overlay (+ p delta) (+ p delta (- end beg)))))
	      (apply #'ov-put nov (overlay-properties ov))
	      (delete-overlay ov))))
    (setq ov-highlight-copy-data nil)))


;; *** Advices - copy

(defun ov-highlight-copy-advice (orig-func &rest args)
  "Advise copy to enable ov-highlights to be copied."
  (let ((beg (nth 0 args))
	(end (nth 1 args)))
    (if (mapcar (lambda (ov) (overlay-get ov 'ov-highlight)) (overlays-in beg end))
	(ov-highlight-copy beg end)
      (apply orig-func args))))

(advice-add 'kill-ring-save :around 'ov-highlight-copy-advice)


;; *** cut advice

(defun ov-highlight-cut-advice (orig-func &rest args)
  "Advise cut so we get ov-highlights."
  (let ((beg (nth 0 args))
	(end (nth 1 args))) 
    (if (mapcar (lambda (ov) (overlay-get ov 'ov-highlight)) (overlays-in beg end))
	(progn (ov-highlight-copy beg end)
	       (ov-clear beg end)
	       (setf (buffer-substring beg end) "")) 
      (apply orig-func args))))

(advice-add 'kill-region :around 'ov-highlight-cut-advice)

;; *** Paste advice

(defun ov-highlight-paste-advice (orig-func &rest args)
  "Advise paste so we can get ov-highlights."
  (if (not (null ov-highlight-copy-data))
      (ov-highlight-paste) 
    (apply orig-func args)))

(advice-add 'yank :around 'ov-highlight-paste-advice)

;; *** kill line advice
(defun ov-highlight-kill-line-advice (orig-func &rest args)
  "Advise kill line so we get ov-highlights."
  (if (mapcar (lambda (ov) (overlay-get ov 'ov-highlight))
	      (overlays-in (line-beginning-position)
			   (line-end-position)))
      (progn
	(ov-highlight-copy (line-beginning-position) (line-end-position))
	(ov-clear (line-beginning-position) (line-end-position))
	(setf (buffer-substring (line-beginning-position) (line-end-position)) ""))
    (apply orig-func args)))

(advice-add 'kill-visual-line :around 'ov-highlight-kill-line-advice)


;; * HTML

(defun ov-highlighter-html (file)
  "Convert the buffer to html using htmlize and write to FILE."
  (interactive (list (read-file-name "File: " nil (let ((bfn (buffer-file-name)))
						    (concat (file-name-base) ".html")))))
  (let ((buf (htmlize-buffer)))
    (with-current-buffer buf
      (write-file file (buffer-string)))
    (kill-buffer buf)
    (browse-url file)))

;; * The End
(provide 'ov-highlighter)

;;; ov-highlighter.el ends here
