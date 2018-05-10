(require 'yasnippet)
(defvar yas-text)

(defun elisp-args-to-docstring ()
  "Return a docstring formatted for the elisp arguments in yas-text."
  (let ((args (split-string yas-text " " t)))
    (mapconcat 'identity
	       (mapcar 'upcase args)
	       "\n")))




