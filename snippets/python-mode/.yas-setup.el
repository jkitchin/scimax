(defun python-def-args ()
  (interactive)
  (let (arg-string
	arg-pairs
	arg-tuples)
    (when (re-search-backward "def .*(\\(.*\\)):" nil t)
      (setq arg-string (match-string-no-properties 1)
	    arg-pairs (mapcar 's-trim (split-string arg-string ","))
	    arg-tuples (mapcar (lambda (pair)
				 (mapcar 's-trim
					 (split-string pair "=")))
			       arg-pairs))
      (cl-loop for (arg val) in arg-tuples concat
	       (if val
		   (format "%s : default=%s\n\n" arg val)
		 (format "%s : \n\n" arg))))))
