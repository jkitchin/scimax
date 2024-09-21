;;; scimax-utils.el --- Utility functions scimax cannot live without

;;; Commentary:
;;

;;; Code:

(add-to-list 'safe-local-eval-forms
	     '(progn (require 'emacs-keybinding-command-tooltip-mode)
		     (emacs-keybinding-command-tooltip-mode +1)))


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


(defun kill-buffer-no-hook ()
  "Kill buffer with no kill-buffer-hook."
  (interactive)
  (let ((kill-buffer-hook '()))
    (kill-buffer)))


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
    (avy-process
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
    (avy-process
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
    (avy-process
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
  (defun scimax-profile ()
    "Run `esup' on the scimax init file to profile it."
    (interactive)
    (require 'esup)
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


(defun f-string (fmt)
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
  (let* ((matches (s-match-strings-all "${\\(?3:\\(?1:[^} ]+\\) *\\(?2:[^}]*\\)\\)}" fmt))
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

    (eval
     `(s-format ,fmt 'aget (list ,@agetter)))))

;; * scimax describe
(defun scimax-describe ()
  "Open an org-buffer describing the scimax setup."
  (interactive)
  (pop-to-buffer "*scimax*")
  (erase-buffer)
  (org-mode)
  (insert (f-string "* System info

- System type :: ${system-type}
- System configuration :: ${system-configuration}
- Window system :: ${window-system}
- Emacs version :: ${emacs-version}
  - Package user dir :: ${package-user-dir}
  - User dir :: ${user-emacs-directory}
  - imagemagick support :: ${(image-type-available-p 'imagemagick)}
  - image types :: ${(imagemagick-types)}
  - gnutls available :: ${(gnutls-available-p)}
- Org-version :: ${(org-version)}\n")
	  "\n- exec-path (this is what Emacs knows)\n"

	  (cl-loop for path in exec-path concat
		   (f-string "  - ${path}\n"))

	  "\n- system path (this is what your shell knows)\n"
	  (cl-loop for p in (split-string (getenv "PATH") ":")
		   concat
		   (format " - %s\n" p)))

  (let* ((default-directory scimax-dir)
	 (branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
	 (commit (string-trim (shell-command-to-string "git rev-parse HEAD")))
	 (remote (string-trim (shell-command-to-string "git config --get remote.origin.url"))))
    (insert (f-string "\n* scimax

- scimax-dir :: ${scimax-dir}")
	    (f-string "
  - git branch :: ${branch}
  - git commit :: ${commit}
  - git remote :: ${remote}\n"))

    (insert "\n* Executables\n"
	    (f-string "
- emacs ::  ${(executable-find \"emacs\")}
- git :: ${(executable-find \"git\")}
- python :: ${(executable-find \"python\")}")
	    (f-string "
- emacs ::  ${(executable-find \"emacs\")}
- git :: ${(executable-find \"git\")}
    - git version :: ${(string-trim (shell-command-to-string \"git --version\"))}
- ssh :: ${(executable-find \"ssh\")}
  - ssh version :: ${(string-trim (shell-command-to-string \"ssh -V\"))}
- python :: ${(executable-find \"python\")}
- latex :: ${(executable-find \"latex\")}
  - For more latex info click [[elisp:scimax-latex-setup]]
- Searching tools
  - grep :: ${(executable-find \"grep\")}
  - ag ::  ${(executable-find \"ag\")}
  - pt ::  ${(executable-find \"pt\")}
  - find ::  ${(executable-find \"find\")}
  - locate ::  ${(executable-find \"locate\")}
- Graphics
  - convert :: ${(executable-find \"convert\"}
  - mogrify :: ${(executable-find \"mogrify\"}
"))

    ;; the end
    (goto-char (point-min))))


(defun scimax-github ()
  "Open the GitHUB repo."
  (interactive)
  (browse-url "https://github.com/jkitchin/scimax"))


(defun scimax-github-issues ()
  "Open the GitHUB repo issues page."
  (interactive)
  (browse-url "https://github.com/jkitchin/scimax/issues"))

;; * screenshots

;; adapted from [[https://vmtyler.com/applescript-markdown-ready-screenshots/][AppleScript Markdown-Ready Screenshots | VMTyler.com]]
(defun screenshot (&optional arg)
  "Take a screenshot and insert org link.
screencapture starts in window capture mode. press space bar to
toggle it to mouse select. with prefix arg, minimize emacs first.
with double prefix arg, prompt for filename. Only works on macOS."
  (interactive "P")
  (when arg
    (suspend-frame))

  (unless (f-directory? "screenshots")
    (make-directory "screenshots"))
  (sit-for 0.2)

  (let ((fname (if (<= (prefix-numeric-value arg) 4)
		   (concat (format-time-string "date-%d-%m-%Y-time-%H-%M-%S" (current-time)) ".png")
		 (read-file-name "filename to save in: "))))
    (do-applescript
     (mapconcat
      'identity
      (list (format "set screenshotFilePath to \"%s\"" (expand-file-name fname "screenshots"))
	    "do shell script \"screencapture \" & \"-i \" & \" \" & quoted form of screenshotFilePath"
	    (concat "set result to \"[[./" fname "]]\"")
	    "set the clipboard to result")
      "\n"))
    (insert (format "\n\n#+attr_org: :width %s\n[[./%s]]\n\n"
		    (min 800
			 ;; sometimes identify returns 0, and I want 800 in that case.
			 (max 800 (string-to-number
				   (cl-first
				    (split-string 
				     (cl-third
				      (split-string
				       (string-trim
					(shell-command-to-string
					 (format "identify %s" fname)))))
				     "x")))))
		    (concat "screenshots/"
			    fname)))
    (org-redisplay-inline-images)
    (raise-frame)))


(defun pngpaste (&optional arg)
  "Paste the clipboard image into org-mode.
Relies on https://github.com/jcsalterego/pngpaste. With prefix
ARG prompt for filename, else generate one. images are saved in
./screenshots. That directory is created if necessary. "
  (interactive "P")
  (setq png
	(if arg
	    (read-file-name "PNG: ")
	  (format-time-string "./screenshots/%Y-%m-%d-%H-%M-%S.png" (current-time))))
  (unless (file-directory-p "./screenshots")
    (make-directory "./screenshots"))
  (when (eq 0 (shell-command (format "pngpaste %s" png)))
    (insert (format "#+attr_org: :width %s\n"
		    (min 800 (string-to-number
			      (cl-first
			       (split-string 
				(cl-third
				 (split-string
				  (string-trim
				   (shell-command-to-string
				    (format "identify %s" png)))))
				"x"))))))
    (insert (format "[[%s]]\n" (if (file-name-absolute-p png)
				   png
				 (concat "./" png))))
    ;; redraw so you can see them
    (org-redisplay-inline-images)))


(defun tesseract (&optional arg)
  "Take a screenshot and insert org link.
with prefix arg, minimize emacs first.
With a double prefix, prompt for the filename.
Only works on macOS."
  (interactive "P")
  (when arg
    (suspend-frame))

  (unless (f-directory? "screenshots")
    (make-directory "screenshots"))
  (sit-for 0.2)

  (let* ((fname (if (<= (prefix-numeric-value arg) 4)
		    (concat (format-time-string "./screenshots/date-%d-%m-%Y-time-%H-%M-%S" (current-time)) ".png")
		  (read-file-name "filename to save in: ")))
	 (tmptext (make-temp-file "tesseract-"))
	 (applescript (mapconcat
		       'identity
		       (list (format "set screenshotFilePath to \"%s\"" (expand-file-name fname))
			     "do shell script \"screencapture \" & \"-s\" & \" \" & quoted form of screenshotFilePath"
			     (concat "set result to \"[[" fname "]]\"")
			     "set the clipboard to result")
		       "\n"))
	 (cmd (format-spec "tesseract %s %f && cat %f.txt"
			   `((?s . ,fname)
			     (?f . ,tmptext)))))

    (do-applescript applescript)

    (insert (s-trim (shell-command-to-string cmd)))

    (insert (format "\n#+attr_org: :width 600\n[[./%s]]\n\n" fname))
    (org-redisplay-inline-images)
    (raise-frame)))

;; * The end
(provide 'scimax-utils)

;;; scimax-utils.el ends here
