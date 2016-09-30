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
;; `ov-highlight' will prompt you for a color, and highlight with it.
;; These convenience functions skip the color prompt.
;; `ov-highlight-yellow'
;; `ov-highlight-blue'
;; `ov-highlight-green'
;; `ov-highlight-pink'

;; `ov-highlight-note' will prompt you for a color, highlight the text and add a
;; Note to it as a tooltip. You can edit a note with `ov-highlight-note-edit'.

;; You can list all the highlights with `ov-highlight-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight under the cursor with `ov-highlight-clear'.
;; Remove all the highlights with `ov-highlight-clear-all'.

;; ov-highlighter uses a local save-buffer-hook to update the data when you
;; save the buffer. It also uses local file variables to load the highlights
;; when you open the file.

;; Known issues: You cannot cut and paste these highlights. Fixing this would
;; involve changing the kill and yank functions to capture overlays in the kill
;; region, and remake them in the yanked region.

(require 'hydra)
(require 'ov)

;;; Code:
;; * Highlight text and functions

(defun ov-highlight-color-chooser ()
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
(defun ov-highlight (beg end &optional color)
  "Highlight region from BEG to END with COLOR.
COLOR is selected from `ov-highlight-color-chooser' when run interactively.
It returns the created overlay."
  (interactive "r")
  (unless color
    (setq color (ov-highlight-color-chooser)))

  ;; add a local hook to make sure it gets saved
  (add-hook 'before-save-hook 'ov-highlight-save nil t)
  
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face `(:background ,color))
    (overlay-put ov 'ov-highlight t)
    (set-buffer-modified-p t)
    (let ((p (point)))
      (when (mark)
	(deactivate-mark))
      (goto-char p)) 
    ov))


;;;###autoload
(defun ov-highlight-yellow ()
  "Highlight region in yellow."
  (interactive)
  (ov-highlight (region-beginning) (region-end) "Yellow"))


;;;###autoload
(defun ov-highlight-blue ()
  "Highlight region in blue."
  (interactive)
  (ov-highlight (region-beginning) (region-end) "LightBlue"))


;;;###autoload
(defun ov-highlight-pink ()
  "Highlight region in pink."
  (interactive)
  (ov-highlight (region-beginning) (region-end) "Pink"))


;;;###autoload
(defun ov-highlight-green ()
  "Highlight region in green."
  (interactive)
  (ov-highlight (region-beginning) (region-end) "Darkolivegreen1"))


(defvar *ov-window-configuration* nil
  "Stores the window configuration so we can later restore it.")


;;;###autoload
(defun ov-highlight-note (beg end &optional color note continue)
  "Highlight selected text from BEG to END with COLOR.
You will be prompted for NOTE in an org-mode buffer. CONTINUE is
not for you to use. It is activated when you close the note
buffer."
  (interactive "r")

  (when (and (not (region-active-p))
	     (looking-at "^$"))
    (insert " ")
    (setq beg (line-beginning-position)
	  end (line-end-position)))
  
  (add-hook 'before-save-hook 'ov-highlight-save nil t)
  (if continue
      ;; this is coming from a special buffer 
      (let ((ov (ov-at))
	    (map (make-sparse-keymap))
	    (edit-func (lambda () (interactive)
			 (ov-highlight-note-edit))))
	(define-key map [mouse-1] edit-func)
	(unless ov (setq ov (ov-highlight beg end color)))
	(overlay-put ov 'help-echo note)
	(overlay-put ov 'mouse-face 'highlight)
	(overlay-put ov 'local-map map) 
	(set-buffer-modified-p t)
	ov)
    ;; We initiate the special buffer
    (unless color (setq color (ov-highlight-color-chooser)))

    (setq *ov-window-configuration* (current-window-configuration))
    
    (let ((cb (current-buffer)))
      (switch-to-buffer-other-window "*ov-note*")
      (erase-buffer)
      (org-mode)
      (let ((map (make-sparse-keymap))) 
	(define-key map (kbd "<header-line> <down-mouse-1>")
	  `(lambda ()
	     (interactive) 
	     (ov-highlight-finish-comment
	      ,cb ,beg ,end ,color
	      (buffer-substring-no-properties (point-min) (point-max)))))
	(setq header-line-format
	      (propertize
	       "Enter comment. Click here or type s-<return> to finish. C-x k to cancel."
	       'mouse-face 'highlight
	       'local-map map)))

      (use-local-map (copy-keymap org-mode-map))
      
      ;; Cancel
      (local-set-key
       (kbd "C-x k")
       `(lambda ()
	  (interactive)
	  (kill-buffer)
	  (ov-highlight-finish-comment ,cb ,beg ,end ,color "")))

      ;; Finish comment
      (local-set-key
       (kbd "s-<return>")
       `(lambda ()
	  (interactive) 
	  (ov-highlight-finish-comment
	   ,cb ,beg ,end ,color
	   (buffer-substring-no-properties (point-min) (point-max))))))))


(defun ov-highlight-finish-comment (buffer beg end color comment)
  "Callback function when you are finished editing a note."
  (when (get-buffer "*ov-note*") (kill-buffer "*ov-note*"))
  (when buffer (switch-to-buffer buffer)
	(when (not (string= "" comment))
	  (kill-new comment)
	  (ov-highlight-note beg end color comment t)
	  (set-buffer-modified-p t)))
  (set-window-configuration *ov-window-configuration*))


;;;###autoload
(defun ov-highlight-note-edit ()
  "Open tooltip of highlight at point in a buffer for editing."
  (interactive)
  (setq *ov-window-configuration* (current-window-configuration))
  (let ((cb (current-buffer))
	(current-note (overlay-get (ov-at) 'help-echo))
	(beg (overlay-start (ov-at)))
	(end (overlay-end (ov-at)))
	(color (plist-get  (overlay-get (ov-at) 'face) :background)))
    (switch-to-buffer "*ov-note*")
    (erase-buffer)
    (org-mode)
    (insert (or current-note ""))
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<header-line> <down-mouse-1>")
	`(lambda ()
	   (interactive) 
	   (ov-highlight-finish-comment
	    ,cb ,beg ,end ,color
	    (buffer-substring-no-properties (point-min) (point-max)))))
      (setq header-line-format
	    (propertize  "Enter comment. Click here or type s-<return> to finish. C-x k to cancel."
			 'mouse-face 'highlight
			 'local-map map)))
    (local-set-key
     (kbd "s-<return>")
     `(lambda ()
	(interactive) 
	(ov-highlight-finish-comment
	 ,cb ,beg ,end ,color
	 (buffer-substring-no-properties (point-min) (point-max)))))))


;; * Editmarks/Feedback functions
;;;###autoload
(defun ov-highlight-typo ()
  "Add a typo highlight."
  (interactive)
  (let* ((r1 (progn (re-search-backward "\\<") (set-mark (point)) (point)))
	 (r2 (progn (re-search-forward "\\>") (point))))
    (ov-highlight-note r1 r2 "PaleVioletRed1" "typo" t)))


;;;###autoload
(defun ov-highlight-comment ()
  "Add a feedback highlight."
  (interactive)
  (let (r1 r2 comment)
    (cond
     ((region-active-p)
      (setq r1 (region-beginning)
	    r2 (region-end)))
     ;; Empty line
     ((looking-at "^$")
      (insert " ")
      (setq r1 (line-beginning-position)
	    r2 (line-end-position))) 
     ((thing-at-point 'word)
      (let ((bounds (bounds-of-thing-at-point 'word)))
	(setq r1 (car bounds)
	      r2 (cdr bounds))))
     ;; comment on whatever is between spaces on each side of point. 
     (t
      (setq r1 (re-search-backward " ")
	    r2 (re-search-forward " "))))
    
    (ov-highlight-note r1 r2 "Orange1")))


;; * Clearing highlights
;;;###autoload
(defun ov-highlight-clear ()
  "Clear highlight at point."
  (interactive)
  (delete-overlay (ov-at))
  (set-buffer-modified-p t))


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
			    (or (replace-regexp-in-string "\n" " " (overlay-get ov 'help-echo)) "nil"))))))
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
      (goto-char pos))))


(define-derived-mode ov-highlight-list-mode
  tabulated-list-mode "ov-highlights"
  "Mode for viewing ov-highlights as a tabular list."
  (setq tabulated-list-sort-key nil)

  (define-key tabulated-list-mode-map (kbd "q") (lambda () (interactive) (kill-buffer) (set-window-configuration ov-highlight-window-configuration)))
  (define-key tabulated-list-mode-map (kbd "r") (lambda () (interactive) (ov-highlight-refresh-list)))
  (define-key tabulated-list-mode-map (kbd "o") 'ov-highlight-jump)
  (define-key tabulated-list-mode-map (kbd "[mouse-1]") 'ov-highlight-jump)
  (define-key tabulated-list-mode-map (kbd "<return>") 'ov-highlight-jump)
  (define-key tabulated-list-mode-map (kbd "k")
    (lambda ()
      (interactive)
      (save-window-excursion
	(ov-highlight-jump)
	(ov-highlight-clear))
      (ov-highlight-refresh-list)))
  (define-key tabulated-list-mode-map (kbd "e")
    (lambda ()
      (interactive)
      (ov-highlight-jump)
      (ov-highlight-note-edit)))
  
  (add-hook 'tabulated-list-revert-hook
	    #'ov-highlight-refresh-list))


;; * Save and load functions
(defun ov-highlight-get-highlights ()
  "Returns a list of (beg end color note) for the overlays."
  (mapcar (lambda (ov)
	    (list (overlay-start ov)
		  (overlay-end ov)
		  (plist-get (overlay-get ov 'face) :background)
		  (overlay-get ov 'help-echo)))
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
  (mapc
   (lambda (entry)
     (let ((beg (nth 0 entry))
	   (end (nth 1 entry))
	   (color (nth 2 entry))
	   (help-echo (nth 3 entry)))
       (if help-echo
	   (ov-highlight-note beg end color help-echo t)
	 (ov-highlight beg end color))))
   (ov-highlight-read-data))
  (add-hook 'before-save-hook 'ov-highlight-save nil t))


(defun ov-highlight-save ()
  "Save highlight information.
Data is saved in comment in the document."
  (save-restriction
    (widen)
    (save-excursion
      (let ((data (ov-highlight-get-highlights)))
	(if data
	    ;; save results
	    (progn
	      ;; first make sure we have a Local variable section, and add one if
	      ;; not.
	      (unless
		  (save-excursion
		    (goto-char (point-min))
		    (re-search-forward
		     (format "^%s.*eval: (ov-highlight-load)" comment-start) nil t))
		(add-file-local-variable 'eval '(ov-highlight-load)))

	      ;; Now search down to either the data line, or add it above the Local
	      ;; Variables.
	      (save-excursion
		(goto-char (point-min))
		(if (re-search-forward
		     (format "^%s.*?ov-highlight-data: \\(.*\\)" comment-start)
		     nil 'mv)
		    ;; We found old data. Let's replace it.
		    (setf (buffer-substring (match-beginning 1) (match-end 1))
			  (org-link-escape (let ((print-length nil)
						 (eval-expression-print-length nil))
					     (prin1-to-string data))))
		  ;; No data found. Find the place to put it.
		  (progn
		    (re-search-backward
		     (format "^%s.*Local Variables" comment-start))
		    (beginning-of-line)
		    (insert (format
			     "%s ov-highlight-data: %s\n\n"
			     (if (eq major-mode 'emacs-lisp-mode)
				 ";;"
			       comment-start)
			     (org-link-escape (let ((print-length nil)
						    (eval-expression-print-length nil))
						(prin1-to-string data)))))))))
	  ;; cleanup if we have no highlights
	  (remove-hook 'before-save-hook 'ov-highlight-save t)
	  (save-excursion
	    (goto-char (point-min))
	    (when (re-search-forward
		   (format "^%s.*?ov-highlight-data: \\(.*\\)" comment-start)
		   nil t)
	      (beginning-of-line)
	      (kill-line))
	    (goto-char (point-min))
	    (when
		(and (re-search-forward "eval: (ov-highlight-load)" nil t)
		     ;; obfuscated pattern to avoid adding local variables here.
		     (re-search-backward (concat "Local" " Variables:"))
		     (re-search-forward "End:"))
	      (re-search-backward "eval: (ov-highlight-load)")
	      (beginning-of-line)
	      (kill-line)
	      (delete-char -1))))))))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(ov-highlight-load))


;; * The hydra menu

(defhydra ov-highlighter (:color blue) "highlighter"
  ("b" ov-highlight-blue "blue")
  ("g" ov-highlight-green "Green")
  ("p" ov-highlight-pink "Pink")
  ;; define as many special colors as you like.
  ("s" (ov-highlight (region-beginning) (region-end) "Lightsalmon1") "Salmon")
  ("y" ov-highlight-yellow "yellow")
  ("c" ov-highlight "Choose color")
  ("n" (ov-highlight-note (region-beginning) (region-end) "Thistle") "Note")
  ("N" ov-highlight-note "Note (c)") 
  ("e" ov-highlight-note-edit "Edit note")

  ;; editmark/feedback options
  ("t" ov-highlight-typo "Typo") 
  ("m" ov-highlight-comment "Comment highlight")
  
  ("l" ov-highlight-display "List highlights")
  ("k" ov-highlight-clear "Clear highlight")
  ("K" ov-highlight-clear-all "Clear all highlights"))


;; * The End
(provide 'ov-highlighter)

;;; ov-highlighter.el ends here
