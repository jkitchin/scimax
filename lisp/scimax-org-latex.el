;;; scimax-org-latex.el --- scimax customizations for latex in org-mode

;;; Commentary:
;; These are scimax modifications to how org generates Latex or Latex equation fragments.

(require 'ox-latex)

(defvar scimax-toggle-latex-fragment-func
  'org-latex-preview
  "Function to toggle latex fragments.
`org-toggle-latex-fragment' is obsolete in 9.3 and is replaced by
`org-latex-preview'.")


;; * Font-lock
;; ** Latex fragments
;;; Code:

(setq org-highlight-latex-and-related '(latex script entities))
(set-face-foreground 'org-latex-and-related "blue")


;; * Latex Export settings

;; Interpret "_" and "^" for export when braces are used.
(setq org-export-with-sub-superscripts '{})

(setq org-latex-default-packages-alist
      '(("AUTO" "inputenc" t)

	;; this is for having good fonts
	("" "lmodern" nil)

	;; This is for handling accented characters
	("T1" "fontenc" t)

	;; This makes standard margins
	("top=1in, bottom=1.in, left=1in, right=1in" "geometry" nil)
	("" "graphicx" t)
	("" "longtable" nil)
	("" "float" nil)
	("" "wrapfig" nil)	  ;makes it possible to wrap text around figures
	("" "rotating" nil)
	("normalem" "ulem" t)

	;; These provide math symbols
	("" "amsmath" t)
	("" "textcomp" t)
	("" "marvosym" t)
	("" "wasysym" t)
	("" "amssymb" t)
	("" "amsmath" t)
	("theorems, skins" "tcolorbox" t)

	;; used for marking up chemical formulas
	("version=3" "mhchem" t)

	("numbers,super,sort&compress" "natbib" nil)
	("" "natmove" nil)

	("" "url" nil)
	;; this is used for syntax highlighting of code
	("cache=false" "minted" nil)

	;; this allows you to use underscores in places like filenames. I still
	;; wouldn't do it.
	("strings" "underscore" nil)
	("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	 "hyperref" nil)

	;; enables you to embed files in pdfs
	("" "attachfile" nil)

	;; set default spacing
	("" "setspace" nil)

	))

;; do not put in \hypersetup. Use your own if you want it e.g.
;; \hypersetup{pdfkeywords={%s},\n pdfsubject={%s},\n pdfcreator={%}}
(setq org-latex-hyperref-template nil)

;; this is for code syntax highlighting in export. you need to use
;; -shell-escape with latex, and install pygments.
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
      '(("frame" "lines")
	("fontsize" "\\scriptsize")
	("linenos" "")))

;; avoid getting \maketitle right after begin{document}
;; you should put \maketitle if and where you want it.
(setq org-latex-title-command "")

(setq org-latex-prefer-user-labels t)

;; ** Custom new classes
;; customized article. better margins
(add-to-list 'org-latex-classes
	     '("article-1"                          ;class-name
	       "\\documentclass{article}
\\usepackage[top=1in, bottom=1.in, left=1in, right=1in]{geometry}
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; This is for when you don't want any default packages, and you want
;; to declare them all yourself.
(add-to-list 'org-latex-classes
	     '("article-no-defaults"                          ;class-name
	       "\\documentclass{article}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]" ;;header-string
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*a{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))




;; * Fragment overlays

(defun scimax-org-latex-fragment-tooltip (beg end image imagetype)
  "Add the fragment tooltip to the overlay and set click function to toggle it."
  (overlay-put (ov-at) 'help-echo
	       (concat (buffer-substring beg end)
		       "\nmouse-1 to toggle."))
  (overlay-put (ov-at) 'local-map (let ((map (make-sparse-keymap)))
				    (define-key map (kbd "C-c C-x C-l")
				      (lambda (interactive)
					(funcall scimax-toggle-latex-fragment-func)))
				    (define-key map [mouse-1]
				      `(lambda ()
					 (interactive)
					 (org-remove-latex-fragment-image-overlays ,beg ,end)))
				    map)))


(defun scimax-toggle-org-latex-fragment-tooltip ()
  "Toggle if latex fragment tooltips are used."
  (if  (not (get 'scimax-org-latex-fragment-tooltip 'enabled))
      (progn
	
	(advice-add 'org--make-preview-overlay :after 'scimax-org-latex-fragment-tooltip)
	(put 'scimax-org-latex-fragment-tooltip 'enabled t)
	(message "LaTeX fragment tooltips are enabled."))
    (advice-remove 'org--make-preview-overlay 'scimax-org-latex-fragment-tooltip)
    (put 'scimax-org-latex-fragment-tooltip 'enabled nil)
    (message "LaTeX fragment tooltips are disabled.")))


;; * Fragment justification
(defun scimax-org-latex-fragment-justify (justification)
  "Justify the latex fragment at point with JUSTIFICATION.
JUSTIFICATION is a symbol for 'left, 'center or 'right."
  (interactive
   (list (intern-soft
          (completing-read "Justification (left): " '(left center right)
                           nil t nil nil 'left))))
  (let* ((ov (ov-at))
	 (beg (ov-beg ov))
	 (end (ov-end ov))
	 (shift (- beg (line-beginning-position)))
	 (img (overlay-get ov 'display))
	 (img (and (and img (consp img) (eq (car img) 'image)
			(image-type-available-p (plist-get (cdr img) :type)))
		   img))
	 space-left offset)
    (when (and img
	       ;; This means the equation is at the start of the line
	       (= beg (line-beginning-position))
	       (or
		(string= "" (s-trim (buffer-substring end (line-end-position))))
		(eq 'latex-environment (car (org-element-context)))))
      (setq space-left (- (window-max-chars-per-line) (car (image-size img)))
	    offset (floor (cond
			   ((eq justification 'center)
			    (- (/ space-left 2) shift))
			   ((eq justification 'right)
			    (- space-left shift))
			   (t
			    0))))
      (when (>= offset 0)
	(overlay-put ov 'before-string (make-string offset ?\ ))))))

(defun scimax-org-latex-fragment-justify-advice (beg end image imagetype)
  "After advice function to justify fragments."
  (scimax-org-latex-fragment-justify (or (plist-get org-format-latex-options :justify) 'left)))


(defun scimax-toggle-latex-fragment-justification ()
  "Toggle if LaTeX fragment justification options can be used."
  (interactive)
  (if (not (get 'scimax-org-latex-fragment-justify-advice 'enabled))
      (progn
	(advice-add 'org--make-preview-overlay :after 'scimax-org-latex-fragment-justify-advice)
	(put 'scimax-org-latex-fragment-justify-advice 'enabled t)
	(message "Latex fragment justification enabled"))
    (advice-remove 'org--make-preview-overlay 'scimax-org-latex-fragment-justify-advice)
    (put 'scimax-org-latex-fragment-justify-advice 'enabled nil)
    (message "Latex fragment justification disabled")))


;; ** numbering latex equations

;; Numbered equations all have (1) as the number for fragments with vanilla
;; org-mode. This code injects the correct numbers into the previews so they
;; look good.
(defun scimax-org-renumber-environment (orig-func &rest args)
  "A function to inject numbers in LaTeX fragment previews."
  (let ((results '())
	(counter -1)
	(numberp))
    (setq results (cl-loop for (begin .  env) in
			(org-element-map (org-element-parse-buffer) 'latex-environment
			  (lambda (env)
			    (cons
			     (org-element-property :begin env)
			     (org-element-property :value env))))
			collect
			(cond
			 ((and (string-match "\\\\begin{equation}" env)
			       (not (string-match "\\\\tag{" env)))
			  (cl-incf counter)
			  (cons begin counter))
			 ((string-match "\\\\begin{align}" env)
			  (prog2
			      (cl-incf counter)
			      (cons begin counter)
			    (with-temp-buffer
			      (insert env)
			      (goto-char (point-min))
			      ;; \\ is used for a new line. Each one leads to a number
			      (cl-incf counter (count-matches "\\\\$"))
			      ;; unless there are nonumbers.
			      (goto-char (point-min))
			      (cl-decf counter (count-matches "\\nonumber")))))
			 (t
			  (cons begin nil)))))

    (when (setq numberp (cdr (assoc (point) results)))
      (setf (car args)
	    (concat
	     (format "\\setcounter{equation}{%s}\n" numberp)
	     (car args)))))

  (apply orig-func args))


(defun scimax-toggle-latex-equation-numbering ()
  "Toggle whether LaTeX fragments are numbered."
  (interactive)
  (if (not (get 'scimax-org-renumber-environment 'enabled))
      (progn
	(advice-add 'org-create-formula-image :around #'scimax-org-renumber-environment)
	(put 'scimax-org-renumber-environment 'enabled t)
	(message "Latex numbering enabled"))
    (advice-remove 'org-create-formula-image #'scimax-org-renumber-environment)
    (put 'scimax-org-renumber-environment 'enabled nil)
    (message "Latex numbering disabled.")))


(defun scimax-org-inject-latex-fragment (orig-func &rest args)
  "Advice function to inject latex code before and/or after the equation in a latex fragment.
You can use this to set \\mathversion{bold} for example to make
it bolder. The way it works is by defining
:latex-fragment-pre-body and/or :latex-fragment-post-body in the
variable `org-format-latex-options'. These strings will then be
injected before and after the code for the fragment before it is
made into an image."
  (setf (car args)
	(concat
	 (or (plist-get org-format-latex-options :latex-fragment-pre-body) "")
	 (car args)
	 (or (plist-get org-format-latex-options :latex-fragment-post-body) "")))
  (apply orig-func args))


(defun scimax-toggle-inject-latex ()
  "Toggle whether you can insert latex in fragments."
  (interactive)
  (if (not (get 'scimax-org-inject-latex-fragment 'enabled))
      (progn
	(advice-add 'org-create-formula-image :around #'scimax-org-inject-latex-fragment)
	(put 'scimax-org-inject-latex-fragment 'enabled t)
	(message "Inject latex enabled"))
    (advice-remove 'org-create-formula-image #'scimax-org-inject-latex-fragment)
    (put 'scimax-org-inject-latex-fragment 'enabled nil)
    (message "Inject latex disabled")))


;; * automatic latex toggling
;; https://ivanaf.com/Automatic_Latex_Fragment_Toggling_in_org-mode.html This
;; makes fragments automatically appear when your cursor leaves them, and
;; disappear when your cursor enters them.

(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")


(defun my/org-latex-fragment--get-current-latex-fragment ()
  "Return the overlay associated with the image under point."
  (car (--select (eq (overlay-get it 'org-overlay-type) 'org-latex-overlay) (overlays-at (point)))))


(defun my/org-in-latex-fragment-p ()
  "Return the point where the latex fragment begins, if inside
  a latex fragment. Else return false"
  (let* ((el (org-element-context))
         (el-type (car el)))
    (and (or (eq 'latex-fragment el-type) (eq 'latex-environment el-type))
         (org-element-property :begin el))))


(defun org-latex-fragment-toggle-auto ()
  "Run an idle timer to toggle fragments."
  ;; Wait for the s
  (interactive)
  (while-no-input
    (run-with-idle-timer 0.05 nil 'org-latex-fragment-toggle-helper)))


(defun org-latex-fragment-toggle-helper ()
  "Toggle a latex fragment image "
  (condition-case nil
      (and (eq 'org-mode major-mode)
           (let* ((begin (my/org-in-latex-fragment-p)))
             (cond
              ;; were on a fragment and now on a new fragment
              ((and
                ;; fragment we were on
                org-latex-fragment-last
                ;; and are on a fragment now
                begin
                ;; but not on the last one this is a little tricky. as you edit the
                ;; fragment, it is not equal to the last one. We use the begin
                ;; property which is less likely to change for the comparison.
                (not (= begin
                        org-latex-fragment-last)))
               ;; go back to last one and put image back
               (save-excursion
                 (goto-char org-latex-fragment-last)
                 (when (my/org-in-latex-fragment-p)
		   (funcall scimax-toggle-latex-fragment-func))
                 ;; now remove current imagea
                 (goto-char begin)
                 (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                   (when ov
                     (delete-overlay ov)))
                 ;; and save new fragment
                 (setq org-latex-fragment-last begin)))

              ;; were on a fragment and now are not on a fragment
              ((and
                ;; not on a fragment now
                (not begin)
                ;; but we were on one
                org-latex-fragment-last)
               ;; put image back on
               (save-excursion
                 (goto-char org-latex-fragment-last)
                 (when (my/org-in-latex-fragment-p)
		   (funcall scimax-toggle-latex-fragment-func)))

               ;; unset last fragment
               (setq org-latex-fragment-last nil))

              ;; were not on a fragment, and now are
              ((and
                ;; we were not one one
                (not org-latex-fragment-last)
                ;; but now we are
                begin)
               (save-excursion
                 (goto-char begin)
                 ;; remove image
                 (let ((ov (my/org-latex-fragment--get-current-latex-fragment)))
                   (when ov
                     (delete-overlay ov)))
                 (setq org-latex-fragment-last begin)))
              ;; else not on a fragment
              ((not begin)
               (setq org-latex-fragment-last nil)))))
    (error nil)))



(defun scimax-latex-toggle-auto-fragment ()
  "Turn toggling fragments automatically to on.
Note: When scrolling, this sometimes causes problems where you
get a lot of Quit messages. Turning this on globally is probably
the wrong thing to do since we only need it in an org file, so we
make the hook local. "
  (interactive)
  (if (not (get 'org-latex-fragment-toggle-auto 'enabled))
      (progn
	(add-hook 'post-command-hook 'org-latex-fragment-toggle-auto nil t)
	(put 'org-latex-fragment-toggle-auto 'enabled t)
	(message "Auto fragment enabled."))
    (remove-hook 'post-command-hook 'org-latex-fragment-toggle-auto t)
    (put 'org-latex-fragment-toggle-auto 'enabled nil)
    (message "Auto fragment disabled.")))


(byte-compile 'org-latex-fragment-toggle-helper)
(byte-compile 'org-latex-fragment-toggle-auto)

(provide 'scimax-org-latex)

;;; scimax-org-latex.el ends here
