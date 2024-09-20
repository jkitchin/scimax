;;; scimax-ob-flycheck.el --- Add flycheck to org-babel src-blocks.  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; The idea in this library is to get flycheck to annotate orb-babel-src-blocks
;; in an org-file to better support literate programming. This allows you to get
;; feedback. This is mostly a proof of concept library. The gist of how it works
;; is a copy of the buffer is made that only has the code in it in the same line
;; positions as the org-file, that buffer is fly-checked, then the overlays are
;; transferred back to the org-file. This is done each time the file is saved.
;;
;; Only languages in `obf-file-extensions' are supported. You have to add
;; entries with (language . extension) to this list for new languages. The
;; extension is what Emacs uses to tell what language is used for flycheck.
;;
;; The temp-files should be cleaned up after each update, and when you kill the buffer.
;; 
;; To use this module, run M-x `scimax-ob-flycheck-mode' in the buffer.


;;; Code
(require 'f)
(require 's)
(require 'ov)


(defun obf-get-src-languages ()
  "Return a list of the languages in this file."
  (let ((langs '()))
    (org-babel-map-src-blocks (buffer-file-name)
      (cl-pushnew lang langs :test 'string=))
    langs))


(defvar obf-file-extensions
  '(("emacs-lisp" . ".el")
    ("python" . ".py")
    ("jupyter-python" . ".py")
    ("ipython" . ".py"))
  "An a-list of (language . extension).
When we create the proxy files we need an extension for each block.")


(defun obf-proxy-filename (lang)
  "Generate the proxy filename for LANG.
The file names look like obf-<md5 hash>.ext"
  (s-concat
   "obf-"
   (if-let (bf (buffer-file-name))
       (md5 (expand-file-name bf))
     "scratch")
   (cdr (assoc lang obf-file-extensions))))


(defun obf-transfer-flycheck-overlays ()
  "Transfer flycheck overlays from proxy-buffer to the org-buffer."
  (let ((ovs '())
	(props)
	(lang obf-lang)
	(proxy-file (buffer-file-name)))
    ;; get list of overlays to transfer
    (cl-loop for ov in (ov-all) do
	     (when (overlay-get ov 'flycheck-overlay)
	       (push (list (ov-beg ov) (ov-end ov) ov) ovs)))
    ;; switch to org buffer
    (with-current-buffer obf-buffer
      ;; I don't recall why I turn flycheck off here, if you do this, it deletes
      ;; overlays in other blocks.
      ;; (flycheck-mode -1)
      (save-excursion
	(cl-loop for (start end ov) in ovs do
		 (when start
		   (goto-char start)
		   (when (and (get-text-property (point) 'src-block)
			      (string= lang (car (org-babel-get-src-block-info)))
			      (not (s-contains? "#\\+END_SRC" (buffer-substring
							       (line-beginning-position)
							       (line-end-position)))))
		     (setq newov (make-overlay start end))
		     (setq props (overlay-properties ov))
		     (setf (flycheck-error-buffer
			    (elt props
				 (+ 1 (-find-index (lambda (a) (eq a 'flycheck-error)) props))))
			   (current-buffer))
		     (setf (flycheck-error-filename
			    (elt props
				 (+ 1 (-find-index (lambda (a) (eq a 'flycheck-error)) props))))
			   (buffer-file-name (current-buffer)))
		     (ov-set newov props))))))
    ;; We don't need it once the overlays are transferred.
    (delete-file proxy-file)))


(defun obf-generate-proxy-files ()
  "Generate the proxy-files for each language in the current buffer."
  (let ((org-content (buffer-string))
	(cb (current-buffer))
	proxy-file
	proxy-buffer)
    (save-buffer)
    (cl-loop for lang in (obf-get-src-languages) do
	     (setq proxy-file (obf-proxy-filename lang))
	     (with-temp-file proxy-file
	       (insert org-content)
	       (org-mode)
	       (goto-char (point-min))
	       (while (and (not (eobp)))
		 (if (and (org-in-src-block-p)
			  (string= lang (car (org-babel-get-src-block-info 'light))))
		     (let* ((src (org-element-context))
			    (end (org-element-property :end src))
			    (len (length (buffer-substring
					  (line-beginning-position)
					  (line-end-position))))
			    newend)
		       (cl--set-buffer-substring
			(line-beginning-position)
			(line-end-position)
			(make-string len ?\s))
		       
		       ;; Now skip to end, and go back to then src delimiter and eliminate that line.
		       (goto-char end)
		       (forward-line (- (* -1 (org-element-property :post-blank src)) 1))
		       (cl--set-buffer-substring
			(line-beginning-position)
			(line-end-position)
			(make-string (length (buffer-substring
					      (line-beginning-position)
					      (line-end-position)))
				     ?\s)))
		   (cl--set-buffer-substring
		    (line-beginning-position)
		    (line-end-position)
		    (make-string (length (buffer-substring
					  (line-beginning-position)
					  (line-end-position)))
				 ?\s)))
		 (forward-line 1)))
	     (save-buffer)
	     ;; Now, make sure it is open and getting checked
	     (setq proxy-buffer (or (find-buffer-visiting proxy-file)
				    (find-file-noselect proxy-file)))
	     (with-current-buffer proxy-buffer
	       (revert-buffer :ignore-auto :noconfirm)
	       ;; put the original buffer into a local variable for use later
	       (make-local-variable 'obf-buffer)
	       (make-local-variable 'obf-lang)
	       (setq obf-lang (org-no-properties lang))
	       (setq obf-buffer cb)
					; Make sure we have the hook function
					; setup, then trigger a check.
	       (add-hook 'flycheck-after-syntax-check-hook 'obf-transfer-flycheck-overlays t t)
	       (flycheck-mode +1)
	       (flycheck-buffer)))))


(defun obf-delete-proxy-files ()
  "Delete all the proxy-files.
If you delete all the language blocks, this will leave some behind."
  (cl-loop for lang in (obf-get-src-languages) do
	   (kill-buffer (find-file-noselect (obf-proxy-filename lang)))
	   (when (file-exists-p (obf-proxy-filename lang))
	     (delete-file (obf-proxy-filename lang)))))


(define-minor-mode scimax-ob-flycheck-mode
  "Minor mode to put flycheck overlays on src-blocks."
  :lighter " obf"
  (if scimax-ob-flycheck-mode
      ;; turn it on
      (progn
	(flycheck-mode -1)
	(add-hook 'org-mode-hook 'obf-generate-proxy-files t t)
	(add-hook 'kill-buffer-hook 'obf-delete-proxy-files t t)
	(add-hook 'after-save-hook 'obf-generate-proxy-files t t)
	(obf-generate-proxy-files))

    ;; turn it off
    ;; clear current overlays
    (ov-clear)
    ;; close and delete proxy-files
    (obf-delete-proxy-files)
    (remove-hook 'org-mode-hook 'obf-generate-proxy-files t)
    (remove-hook 'after-save-hook 'obf-generate-proxy-files t)))

(provide 'scimax-ob-flycheck)

;;; scimax-ob-flycheck.el ends here
