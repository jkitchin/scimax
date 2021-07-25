;;; org-ref-cite.el --- A library to use org-cite like org-ref

;; Copyright(C) 2021 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref-cite
;; Version: 1.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org "9.5") (ivy "0") (hydra "0") (bibtex-completion "0") (ivy-bibtex "0"))
;; This file is not currently part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;;
;;; Code:

(require 'oc)

(require 'org-ref-cite-advice)
(require 'org-ref-cite-activate)
(require 'org-ref-cite-insert)
(require 'org-ref-cite-follow)
(require 'org-ref-cite-export)
(require 'org-ref-cite-compat)


(org-cite-register-processor 'org-ref-cite
  :activate #'org-ref-cite-activate
  :follow #'org-ref-cite-follow
  :insert #'org-ref-cite-insert-processor
  :export-bibliography #'org-ref-cite-export-bibliography
  :export-citation #'org-ref-cite-export-citation
  :export-finalizer #'org-ref-cite-use-package
  :cite-styles (mapcar 'car org-ref-cite-styles))

(provide 'org-ref-cite)
;;; org-ref-cite.el ends here
