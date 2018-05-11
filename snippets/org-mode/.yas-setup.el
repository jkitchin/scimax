(require 'yasnippet)

(defun scimax-get-src-header-val-snippet ()
  "Returns a string for a header value snippet using completion."
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (header-vals (org-babel-combine-header-arg-lists
		       org-babel-common-header-args-w-values
		       (when (boundp lang-headers) (eval lang-headers t))))
	 (header (completing-read "Header: " (mapcar 'symbol-name (mapcar 'car header-vals))))
	 (vals (cdr (assoc (intern-soft header) headers-vals)))
	 (val (completing-read "Value: " vals)))
    (format ":${1:%s} ${2:%s} " header val)))


(defun scimax-get-src-block-with-lang-snippet ()
  "Return a string for a src-block with language snippet.
I was not able to use a simple choice in a field for this,
because it seems org was trying to add syntax highlighting to the
block before there was a language defined."
  (let ((lang (completing-read "Lang: " (mapcar 'car org-babel-load-languages))))
    (format "#+BEGIN_SRC %s
$0
#+END_SRC" lang)))
