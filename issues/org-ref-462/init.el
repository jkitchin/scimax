;;; org-mode
(require 'package)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

(add-to-list
 'package-archives
 '("org"         . "http://orgmode.org/elpa/")
 t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (package-refresh-contents))

(setq use-package-always-ensure t)

(use-package org-plus-contrib)
(use-package org-ref)

(require 'org)
(setq org-export-allow-bind-keywords t)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-confirm-babel-evaluate nil)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
(require 'ox-latex)
(add-to-list 'org-latex-classes
	     '("svjour3"
	       "\\documentclass{svjour3}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
	     '("wlscirep"
	       "\\documentclass{wlscirep}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
	     '("elsarticle"
	       "\\documentclass{elsarticle}"
	       ("\\section{%s}" . "\\section*{%s}")
	       ("\\subsection{%s}" . "\\subsection*{%s}")
	       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	       ("\\paragraph{%s}" . "\\paragraph*{%s}")
	       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(setq org-list-allow-alphabetical t)

;;; pretty-print code when exporting to LaTeX/PDF
(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

;;; org-ref
(setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography "~/Dropbox/bibliography/references.bib"
      org-ref-pdf-directory "~/Dropbox/bibliography/pdf")
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))
(require 'org-ref)

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(TeX-PDF-mode t)
;;  '(TeX-view-program-selection
;;    (quote
;;     (((output-dvi style-pstricks)
;;       "dvips and gv")
;;      (output-dvi "xdvi")
;;      (output-pdf "xdg-open")
;;      (output-html "xdg-open"))))
;;  '(ansi-color-faces-vector
;;    [default default default italic underline success warning error])
;;  '(ansi-color-names-vector
;;    ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
;;  '(custom-enabled-themes (quote (tsdh-light)))
;;  '(doc-view-resolution 300)
;;  '(fringe-mode 0 nil (fringe))
;;  '(inhibit-startup-screen t)
;;  '(org-agenda-files
;;    (quote
;;     ("~/Dropbox/Science/Active/learnmot/learnmot.org" "~/Dropbox/Admin/USA/Jobs/UCSD2017/ResearchStatement.org" "~/Dropbox/Org/externalInhibition.org" "~/Dropbox/Science/Active/PresidentNames/PresidentNames.org" "~/Dropbox/Science/Active/Thinking/TheoryDevelopment/2015-ChainingPaper/RSocOpenSci/Code/Chaining.org" "~/Dropbox/Science/Active/solvingModels/solvingModelsNew.org" "~/Dropbox/Science/Active/solvingModels/solvingModels.org" "~/Dropbox/Admin/USA/Taxes/2015/Receipts.org")))
;;  '(org-babel-load-languages
;;    (quote
;;     ((sh . t)
;;      (C . t)
;;      (R . t)
;;      (emacs-lisp . t)
;;      (latex . t))))
;;  '(org-file-apps
;;    (quote
;;     ((auto-mode . emacs)
;;      ("\\.mm\\'" . default)
;;      ("\\.x?html?\\'" . default)
;;      ("\\.pdf\\'" . "mupdf %s"))))
;;  '(org-format-latex-options
;;    (quote
;;     (:foreground default :background default :scale 2.5 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
;; 		 ("begin" "$1" "$" "$$" "\\(" "\\["))))
;;  '(org-latex-default-packages-alist
;;    (quote
;;     (("AUTO" "inputenc" t)
;;      ("T1" "fontenc" t)
;;      ("" "fixltx2e" nil)
;;      ("" "graphicx" t)
;;      ("" "longtable" nil)
;;      ("" "float" nil)
;;      ("" "wrapfig" nil)
;;      ("" "rotating" nil)
;;      ("normalem" "ulem" t)
;;      ("" "amsmath" t)
;;      ("" "textcomp" t)
;;      ("" "marvosym" t)
;;      ("" "wasysym" t)
;;      ("" "amssymb" t)
;;      ("" "hyperref" nil)
;;      ("" "mathptmx" t)
;;      "\\tolerance=1000")))
;;  '(org-latex-table-caption-above nil)
;;  '(org-ref-cite-types
;;    (quote
;;     ("cite" "nocite" "citet" "citet*" "citep" "citep*" "citealt" "citealt*" "citealp" "citealp*" "citenum" "citetext" "citeauthor" "citeauthor*" "citeyear" "citeyear*" "Citet" "Citep" "Citealt" "Citealp" "Citeauthor" "Cite" "parencite" "Parencite" "footcite" "footcitetext" "textcite" "Textcite" "smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite" "Autocite" "autocite*" "Autocite*" "Citeauthor*" "citetitle" "citetitle*" "citedate" "citedate*" "citeurl" "fullcite" "footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite" "fnotecite" "cites" "Cites" "parencites" "Parencites" "footcites" "footcitetexts" "smartcites" "Smartcites" "textcites" "Textcites" "supercites" "autocites" "Autocites" "bibentry" "cites")))
;;  '(org-src-fontify-natively t)
;;  '(org-src-window-setup (quote current-window))
;;  '(org-startup-truncated nil)
;;  '(org-support-shift-select nil)
;;  '(package-selected-packages (quote (org-ref)))
;;  '(preview-default-document-pt 12)
;;  '(preview-scale-function 1.5)
;;  '(scroll-bar-mode nil)
;;  '(tool-bar-mode nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 119 :width normal)))))
