;;; scimax-org-src-blocks.el --- Scimax customizations for src blocks.

;;; Commentary:
;; This makes the background color of src blocks be customizable based on
;; patches from Rasmus <rasmus@gmx.us>. I like this because it makes it simple
;; to see what a src block language is by color, and makes them stand out more
;; than the default color.
;;
;; You define a face like org-block-<lang> that defines the face for the
;; background. You can toggle this on and off with
;; `scimax-org-toggle-colored-src-blocks'.
;;
;; This library fixes the fontification of <> in src-blocks so that you can use
;; them as operators.
;;
;;

;; * Colored src blocks

(defface org-block-emacs-lisp
  `((t (:background "LightCyan1")))
  "Face for elisp src blocks")


(defface org-block-python
  `((t (:background "DarkSeaGreen1")))
  "Face for python blocks")


(defface org-block-ipython
  `((t (:background "thistle1")))
  "Face for python blocks")


(defface org-block-jupyter-hy
  `((t (:background "light goldenrod yellow")))
  "Face for hylang blocks")


(defface org-block-sh
  `((t (:background "gray90")))
  "Face for python blocks")


(defun scimax-org-toggle-colored-src-blocks ()
  "Toggle whether colored src-blocks in org-mode are used."
  (interactive)
  (if (not (get 'scimax-org-src-font-lock-fontify-block 'enabled))
      (progn
	(advice-add 'org-src-font-lock-fontify-block :override #'scimax-org-src-font-lock-fontify-block)
	(advice-add 'org-fontify-meta-lines-and-blocks-1 :override #'scimax-org-fontify-meta-lines-and-blocks-1)
	(put 'scimax-org-src-font-lock-fontify-block 'enabled t)
	(message "Colored src-blocks enabled."))
    (advice-remove 'org-src-font-lock-fontify-block  #'scimax-org-src-font-lock-fontify-block)
    (advice-remove 'org-fontify-meta-lines-and-blocks-1 #'scimax-org-fontify-meta-lines-and-blocks-1)
    (put 'scimax-org-src-font-lock-fontify-block 'enabled nil)
    (message "Colored src-blocks disabled.")))


;; This function overwrites the org-src function to make src blocks be colored again.
(defun scimax-org-src-font-lock-fontify-block (lang start end)
  "Fontify code block.
LANG is the language of the block.  START and END are positions of
the block.  This function is called by Emacs automatic
fontification, as long as `org-src-fontify-natively' is non-nil.

jkitchin: I modified this function so the src blocks will have
different colors defined in the faces above. If you define a face
named `org-block-lang', then lang blocks will use that facefor
the background. This function is in an override advice. You can
remove this with `(advice-remove 'org-src-font-lock-fontify-block
#'scimax-org-src-font-lock-fontify-block)'
"
  (let ((lang-mode (cond
		    ;; this is for org 9.2
		    ((fboundp 'org-src--get-lang-mode)
		     (org-src--get-lang-mode lang))
		    ;; this is for org 9.3
		    ((fboundp 'org-src-get-lang-mode)
		     (org-src-get-lang-mode lang)))))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
	    (modified (buffer-modified-p))
	    (org-buffer (current-buffer))
	    (block-faces (let ((face-name (intern (format "org-block-%s" lang))))
			   (append (and (facep face-name) (list face-name))
				   '(org-block)))))
	(remove-text-properties start end '(face nil))
	(with-current-buffer
	    (get-buffer-create
	     (format " *org-src-fontification:%s*" lang-mode))
	  (erase-buffer)
	  (insert string " ") ;; so there's a final property change
	  (unless (eq major-mode lang-mode) (funcall lang-mode))
	  (org-font-lock-ensure)
	  (let ((pos (point-min)) next)
	    (while (setq next (next-single-property-change pos 'face))
	      (let ((new-face (get-text-property pos 'face)))
		(put-text-property
		 (+ start (1- pos)) (1- (+ start next)) 'face
		 (list :inherit (append (and new-face (list new-face))
					block-faces))
		 org-buffer))
	      (setq pos next))
	    ;; Add the face to the remaining part of the font.
	    (put-text-property (1- (+ start pos))
			       end 'face
			       (list :inherit block-faces) org-buffer)))
	(add-text-properties
	 start end
	 '(font-lock-fontified t fontified t font-lock-multiline t))
	(set-buffer-modified-p modified)))))


(defun scimax-org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks.

jkitchin: This function has been modified to support colored src
blocks. See `scimax-org-src-font-lock-fontify-block'. It looks
like I add some extra properties like the src_block start and end
points."
  (let ((case-fold-search t))
    (if (re-search-forward
	 "^\\([ \t]*#\\(\\(\\+[a-zA-Z]+:?\\| \\|$\\)\\(_\\([a-zA-Z]+\\)\\)?\\)[ \t]*\\(\\([^ \t\n]*\\)[ \t]*\\(.*\\)\\)\\)"
	 limit t)
	(let ((beg (match-beginning 0))
	      (block-start (match-end 0))
	      (block-end nil)
	      (lang (match-string 7))
	      (beg1 (line-beginning-position 2))
	      (dc1 (downcase (match-string 2)))
	      (dc3 (downcase (match-string 3)))
	      end end1 quoting block-type ovl)
	  (cond
	   ((and (match-end 4) (equal dc3 "+begin"))
	    ;; Truly a block
	    (setq block-type (downcase (match-string 5))
		  quoting (member block-type org-protecting-blocks))
	    (when (re-search-forward
		   (concat "^[ \t]*#\\+end" (match-string 4) "\\>.*")
		   nil t) ;; on purpose, we look further than LIMIT
	      (setq end (min (point-max) (match-end 0))
		    end1 (min (point-max) (1- (match-beginning 0))))
	      (setq block-end (match-beginning 0))
	      (when quoting
		(org-remove-flyspell-overlays-in beg1 end1)
		(remove-text-properties beg end
					'(display t invisible t intangible t)))
	      (add-text-properties
	       beg end '(font-lock-fontified t font-lock-multiline t))
	      (add-text-properties beg beg1 '(face org-meta-line))
	      (org-remove-flyspell-overlays-in beg beg1)
	      (add-text-properties	; For end_src
	       end1 (min (point-max) (1+ end)) '(face org-meta-line))
	      (org-remove-flyspell-overlays-in end1 end)
	      (cond
	       ((and lang (not (string= lang "")) org-src-fontify-natively)
		(org-src-font-lock-fontify-block lang block-start block-end)
		(add-text-properties beg1 block-end (list
						     'src-block-begin beg1
						     ;; the end is at the
						     ;; beginning of the
						     ;; #+END_SRC line, and I
						     ;; want it to be at the end
						     ;; of the last line in the
						     ;; block, so I subtract one
						     ;; here.
						     'src-block-end (- block-end 1)
						     'src-block t
						     'lang (substring-no-properties lang))))
	       (quoting
		(add-text-properties beg1 (min (point-max) (1+ end1))
				     (let ((face-name (intern (format "org-block-%s" lang))))
				       (append (and (facep face-name) (list face-name))
					       '(face org-block))))) ; end of source block
	       ((not org-fontify-quote-and-verse-blocks))
	       ((string= block-type "quote")
		(add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-quote)))
	       ((string= block-type "verse")
		(add-text-properties beg1 (min (point-max) (1+ end1)) '(face org-verse))))
	      (add-text-properties beg beg1 '(face org-block-begin-line))
	      (add-text-properties (min (point-max) (1+ end)) (min (point-max) (1+ end1))
				   '(face org-block-end-line))
	      t))
	   ((member dc1 '("+title:" "+author:" "+email:" "+date:"))
	    (org-remove-flyspell-overlays-in
	     (match-beginning 0)
	     (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	    (add-text-properties
	     beg (match-end 3)
	     (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
		 '(font-lock-fontified t invisible t)
	       '(font-lock-fontified t face org-document-info-keyword)))
	    (add-text-properties
	     (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	     (if (string-equal dc1 "+title:")
		 '(font-lock-fontified t face org-document-title)
	       '(font-lock-fontified t face org-document-info))))
	   ((equal dc1 "+caption:")
	    (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	    (remove-text-properties (match-beginning 0) (match-end 0)
				    '(display t invisible t intangible t))
	    (add-text-properties (match-beginning 1) (match-end 3)
				 '(font-lock-fontified t face org-meta-line))
	    (add-text-properties (match-beginning 6) (+ (match-end 6) 1)
				 '(font-lock-fontified t face org-block))
	    t)
	   ((member dc3 '(" " ""))
	    (org-remove-flyspell-overlays-in beg (match-end 0))
	    (add-text-properties
	     beg (match-end 0)
	     '(font-lock-fontified t face font-lock-comment-face)))
	   (t ;; just any other in-buffer setting, but not indented
	    (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	    (remove-text-properties (match-beginning 0) (match-end 0)
				    '(display t invisible t intangible t))
	    (add-text-properties beg (match-end 0)
				 '(font-lock-fontified t face org-meta-line))
	    t))))))


;; * Fixing <> fontlock in src blocks
;;
;; this was broken so that if you had < or > in a src block it would break
;; parens matching.

;; https://emacs.stackexchange.com/questions/50216/org-mode-code-block-parentheses-mismatch
(defun scimax-org-mode-<>-syntax-fix (start end)
  "Change syntax of characters ?< and ?> to symbol within source code blocks."
  (let ((case-fold-search t))
    (when (eq major-mode 'org-mode)
      (save-excursion
        (goto-char start)
        (while (re-search-forward "<\\|>" end t)
          (when (save-excursion
                  (and
                   (re-search-backward "[[:space:]]*#\\+\\(begin\\|end\\)_src\\_>" nil t)
                   (string-equal (match-string 1) "begin")))
            ;; This is a < or > in an org-src block
            (put-text-property (point) (1- (point))
                               'syntax-table (string-to-syntax "_"))))))))


(defun scimax-fix-<>-syntax ()
  "Fix syntax of <> in code blocks.
This function should be added to `org-mode-hook' to make it work."
  (setq syntax-propertize-function 'scimax-org-mode-<>-syntax-fix)
  (syntax-propertize (point-max)))

(add-hook 'org-mode-hook
	  #'scimax-fix-<>-syntax)

(provide 'scimax-org-src-blocks)

;;; scimax-org-src-blocks.el ends here
