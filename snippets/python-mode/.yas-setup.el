(defun scimax-get-numpy-docstring-snippet ()
  "Returns a snippet for a numpy docstring.
This should be run in a python function."
  (let (arg-string
	arg-pairs
	arg-tuples
	results
	(i 1))
    (save-excursion
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
			   (format "%s : ${%s:type} optional, default=%s\n${%s:description}"
				   arg (cl-incf i) val (cl-incf i))
			 (format "%s : ${%s:type}\n${%s:description}"
				 arg (cl-incf i) (cl-incf i)))))
	(format "\"\"\"${1:One line description}

Parameters
----------

%s

Returns
-------
${%s:return}
\"\"\"" (s-join "\n\n" results) (cl-incf i))))))


(defun scimax-get-google-docstring-snippet ()
  "Returns a snippet for a google docstring.
This should be run in a python function."
  (let (arg-string
	arg-pairs
	arg-tuples
	results
	(i 1))
    (save-excursion
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
			   (format "$>%s (${%s:type}, optional): ${%s:description}. Defaults to %s."
				   arg (cl-incf i) (cl-incf i) val)
			 (format "%s (${%s:type}): ${%s:description}"
				 arg (cl-incf i) (cl-incf i)))))
	(format "\"\"\"${1:One line description}

Args:
%s

Returns:
${%s:return}
\"\"\"" (s-join "\n" results) (cl-incf i))))))



;; https://github.com/AndreaCrotti/yasnippet-snippets/blob/master/snippets/python-mode/.yas-setup.el
(defun python-split-args (arg-string)
  "Split a python argument string into ((name, default)..) tuples"
  (mapcar (lambda (x)
	    (split-string x "[[:blank:]]*=[[:blank:]]*" t))
          (split-string arg-string "[[:blank:]]*,[[:blank:]]*" t)))

;; https://emacs.stackexchange.com/questions/19422/library-for-automatically-inserting-python-docstring-in-google-style
(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a google docstring format for the python arguments in yas-text."
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
	 (nr 0)
         (formatted-args
	  (mapconcat
	   (lambda (x)
	     (concat "   " (nth 0 x)
		     (if make-fields (format " ${%d:arg%d}" (cl-incf nr) nr))
		     (if (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
	   args
	   indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
		  (list "" "Args:" formatted-args)
		  indent)
       "\n"))))
