(defun python-def-args ()
  (interactive)
  (let (arg-string
	arg-pairs
	arg-tuples
	results)
    (when (re-search-backward "def .*(\\(.*\\)):" nil t)
      (setq arg-string (match-string-no-properties 1)
	    arg-pairs (mapcar 's-trim (split-string arg-string ","))
	    arg-tuples (mapcar (lambda (pair)
				 (mapcar 's-trim
					 (split-string pair "=")))
			       arg-pairs))
      (setq arg-tuples (-filter (lambda (el) (not (string= (car el) "self")))
				arg-tuples))
      (setq results
	    (cl-loop for (arg val) in arg-tuples collect
		     (if val
			 (format "%s : optional, default=%s" arg val)
		       (format "%s : " arg))))
      (s-join "\n\n" results))))
