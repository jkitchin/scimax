;;; scimax-jupyter.el --- scimax customization for emacs-jupyter

;;; Commentary:
;;

(require 'jupyter)
(add-to-list 'org-babel-load-languages '(jupyter . t) t)
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)



;; see [[nb:scimax::scimax-org-babel-ipython-upstream.el::c3312]]
;; for more definitions.
(cl-loop for (key . def) in '(("C-<return>" . org-ctrl-c-ctrl-c)
			      ("M-<return>" . (lambda ()
						(interactive)
						(scimax-execute-and-next-block t)))
			      ("S-<return>" . scimax-execute-and-next-block)

			      ("H-w" . org-babel-previous-src-block)
			      ("H-s" . org-babel-next-src-block)
			      ("H-j" . scimax-jump-to-block)

			      ("H--" . scimax-split-src-block)
			      ("H-m" . scimax-ob-merge-blocks)
			      ("H-l" . org-babel-remove-result)
			      ("H-L" . scimax-ob-clear-all-results)

			      )
	 do (jupyter-org-define-key (kbd key) def))


(provide 'scimax-jupyter)

;;; scimax-jupyter.el ends here
