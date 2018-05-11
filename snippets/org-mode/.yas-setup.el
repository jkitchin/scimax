(require 'yasnippet)

(defun org-babel-headers ()
  (let* ((info (org-babel-get-src-block-info 'light))
	 (lang (car info))
	 (begin (nth 5 info))
	 (lang-headers (intern (concat "org-babel-header-args:" lang)))
	 (header-vals (org-babel-combine-header-arg-lists
		       org-babel-common-header-args-w-values
		       (when (boundp lang-headers) (eval lang-headers t))))
	 (headers (mapcar 'symbol-name (mapcar 'car header-vals)))) 
    headers))


(defun org-babel-header-vals (header)
  '("5" "e" "2")
  )
