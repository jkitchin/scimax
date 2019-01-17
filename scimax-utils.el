;;; scimax-utils.el --- Utility functions scimax cannot live without

;;; Commentary:
;;

;;; Code:

;; * Hotspots
(defcustom scimax-user-hotspot-commands '()
  "A-list of hotspots to jump to in `hotspots'.
These are shortcut to commands.
\(\"label\" . command)"
  :group 'scimax)

(defcustom scimax-user-hotspot-locations '()
  "A-list of hotspot locations to jump to in  `hotspots'.
\(\"label\" . \"Path to file\").

These are like bookmarks."
  :group 'scimax)


;;;###autoload
(defun hotspots (arg)
  "Helm interface to hotspot locations.
This includes user defined
commands (`scimax-user-hotspot-commands'),
locations (`scimax-user-hotspot-locations'), org agenda files,
recent files and bookmarks. You can set a bookmark also."
  (interactive "P")
  (helm :sources `(((name . "Commands")
		    (candidates . ,scimax-user-hotspot-commands)
		    (action . (("Open" . (lambda (x) (funcall x))))))
		   ((name . "My Locations")
		    (candidates . ,scimax-user-hotspot-locations)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   ((name . "My org files")
		    (candidates . ,org-agenda-files)
		    (action . (("Open" . (lambda (x) (find-file x))))))
		   helm-source-recentf
		   helm-source-bookmarks
		   helm-source-bookmark-set)))


(add-to-list 'safe-local-eval-forms
	     '(progn (require 'emacs-keybinding-command-tooltip-mode) (emacs-keybinding-command-tooltip-mode +1)))

;;;###autoload
(defun scimax-help ()
  "Open the ‘scimax’ manual in org-mode."
  (interactive)
  (find-file (expand-file-name
              "scimax.org"
	      scimax-dir)))


;;;###autoload
(defun scimax-info ()
  "Open the info manual."
  (info "(scimax)")
  (require 'emacs-keybinding-command-tooltip-mode)
  (emacs-keybinding-command-tooltip-mode +1))



;; * utilities
;;;###autoload
(defun kill-all-buffers ()
  "Kill all buffers.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))


;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers but this one.  Leave one frame open."
  (interactive)
  (mapc 'kill-buffer
	(delq (current-buffer) (buffer-list)))
  (delete-other-windows))


;;;###autoload
(defun unfill-paragraph ()
  "Unfill paragraph at or after point."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

;; * Version control
;; Some new bindings to add to vc-prefix-map
(define-key 'vc-prefix-map "t" 'magit-status)


(define-key 'vc-prefix-map "p" (lambda () (interactive) (vc-git-push nil)))
(define-key 'vc-prefix-map "P" (lambda () (interactive) (vc-git-pull nil)))


;; * Misc


;; case on regions
(defun sentence-case-region (r1 r2)
  "Capitalize the word at point, and the first word of each
sentence in the region."
  (interactive "r")
  (save-excursion
    (goto-char r1)
    (capitalize-word 1)
    (while (< (point) r2)
      (forward-sentence)
      (capitalize-word 1))))


(global-set-key (kbd "M-<backspace>") 'backward-kill-sentence)

;; * avy jump commands

(defun avy-jump-to-word-in-line (&optional arg)
  "Jump to a word in the current line."
  (interactive)
  (avy-with word-jump
    (avy--process
     (let ((p '())
	   (e (line-end-position)))
       (save-excursion
	 (goto-char (line-beginning-position))
	 (push (point) p)
	 (while (< (point) e)
	   (forward-word)
	   (save-excursion
	     (backward-word)
	     (push (point) p)))
	 (reverse p)))
     (avy--style-fn avy-style))))

(defun avy-jump-to-sentence ()
  "Jump to a sentence with avy."
  (interactive)
  (avy-with my-jumper
    (avy--process
     (let (p
	   (e (window-end)))
       (save-excursion
	 (goto-char (window-start))
	 (push (point) p)
	 (while (< (point) e)
	   (forward-sentence)
	   (save-excursion
	     (backward-sentence)
	     (push (point) p)))
	 (reverse p)))
     (avy--style-fn avy-style))))

(defun avy-jump-to-paragraph ()
  "Jump to a paragraph with avy."
  (interactive)
  (avy-with my-jumper
    (avy--process
     (let (p
	   (e (window-end)))
       (save-excursion
	 (goto-char (window-start))
	 (push (point) p)
	 (while (< (point) e)
	   (forward-paragraph)
	   (save-excursion
	     (backward-paragraph)
	     (push (+ 1 (point)) p)))
	 (reverse p)))
     (avy--style-fn avy-style))))


;; * profile me
(unless (memq system-type '(windows-nt ms-dos))

  (require 'esup)

  (defun scimax-profile ()
    "Run `esup' on the scimax init file to profile it."
    (esup (expand-file-name "init.el" scimax-dir))))



(defmacro with-no-new-buffers (&rest body)
  "Run BODY, and kill any new buffers created.
Returns whatever BODY would return."
  (let ((current-buffers (buffer-list)))
    `(prog1
	 (progn
	   ,@body)
       (mapc (lambda (buf)
	       (unless (-contains? ',current-buffers buf)
		 (kill-buffer buf)))
	     (buffer-list)))))


;; * f-strings

(defmacro f-string (fmt)
  "Like `s-format' but with format fields in it.
FMT is a string to be expanded against the current lexical
environment. It is like what is used in `s-lex-format', but has
an expanded syntax to allow format-strings. For example:
${user-full-name 20s} will be expanded to the current value of
the variable `user-full-name' in a field 20 characters wide.
  (let ((f (sqrt 5)))  (f-string \"${f 1.2f}\"))
  will render as: 2.24
This function is inspired by the f-strings in Python 3.6, which I
enjoy using a lot.

You can also try putting expressions in for formatting, e.g.:
 (let ((a 11)) (f-string \"The sqrt of ${a} is ${(sqrt a) 1.2f}.\"))
 will render as \"The sqrt of 11 is 3.32\".
"
  (let* ((matches (s-match-strings-all"${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" fmt))
         (agetter (cl-loop
		   for (m0 m1 m2 m3) in matches
		   collect
		   `(cons ,m3
			  ,(if (s-starts-with? "(" m3)
			       ;; This means an expression is used
			       (with-temp-buffer
				 (insert m3)
				 (goto-char (point-min))
				 (let ((expr (read (current-buffer)))
				       (fmt (s-trim (buffer-substring (point) (point-max)))))
				   `(format
				     (format "%%%s" (if (string= ,fmt "")
							(if s-lex-value-as-lisp "S" "s")
						      ,fmt))
				     ,expr)))

			     `(format
			       (format "%%%s" (if (string= ,m2 "")
						  (if s-lex-value-as-lisp "S" "s")
						,m2))
			       (symbol-value (intern ,m1))))))))

    `(s-format ,fmt 'aget (list ,@agetter))))



;; * The end
(provide 'scimax-utils)

;;; scimax-utils.el ends here
