;;; scimax-org-radio-checkbox.el --- Org radio checkboxes
;; * radio checkboxes

;;; Commentary:
;; This library provides a radio checkbox for org-mode. A radio checkbox is a
;; checkbox list where only one box can be checked, i.e. a radio button choice.
;;
;; This code achieves that by using a C-c C-c hook to toggle the item checked,
;; and remove the others. To facilitate using the checked value in code,
;; `scimax-get-radio-list-value' is provided to get the value from a radio
;; checkbox list by its name.

;;; Code:

(defun scimax-in-radio-list-p ()
  "Return radio list if in one, else nil."
  (interactive)
  (let* ((element (org-element-context))
	 (radio-list (cond
		      ;; on an item. easy.
		      ((and (eq 'item (car element))
			    (member
			     ":radio"
			     (org-element-property
			      :attr_org
			      (org-element-property :parent element))))
		       (org-element-property :parent element))
		      ;; on an item paragraph
		      ((and (eq 'paragraph (car element))
			    (eq 'item (car (org-element-property :parent element)))
			    (member
			     ":radio"
			     (org-element-property
			      :attr_org
			      (org-element-property
			       :parent
			       (org-element-property :parent element)))))
		       (org-element-property
			:parent
			(org-element-property :parent element)))
		      ;; not on an item or item paragraph
		      (t
		       nil))))
    radio-list))

(defun scimax-radio-CcCc ()
  "Hook function for C-cC-c to work in radio checklists."
  (interactive)
  (let ((radio-list (scimax-in-radio-list-p))
	(p (point)))
    (when radio-list
      ;; clear all boxes
      (save-excursion
	(mapc (lambda (el)
		(goto-char (car el))
		(when (re-search-forward "\\[X\\]" (line-end-position) t)
		  (replace-match "[ ]")))
	      (org-element-property :structure radio-list))
	;; Now figure out where to put the new X
	(mapc (lambda (el)
		(when (and (> p (car el))
			   (< p (car (last el))))
		  (goto-char (car el))
		  (when (re-search-forward "\\[ \\]" (line-end-position) t)
		    (replace-match "[X]"))))
	      (org-element-property :structure radio-list)))
      ;; return t so the hook ends I think
      t)))

(add-hook 'org-ctrl-c-ctrl-c-hook 'scimax-radio-CcCc)
;; this works with mouse checking.
(add-hook 'org-checkbox-statistics-hook 'scimax-radio-CcCc)


(defun scimax-org-get-plain-list (name)
  "Get the org-element representation of a plain-list named NAME."
  (catch 'found
    (org-element-map
        (org-element-parse-buffer)
        'plain-list
      (lambda (plain-list)
        (when
            (string= name (org-element-property :name plain-list))
          (throw 'found plain-list))))))


(defun scimax-get-radio-list-value (name)
  "Return the value of the checked item in a radio list named NAME."
  (save-excursion
    (cl-loop for el in (org-element-property
                     :structure
                     (scimax-org-get-plain-list name))
          if (string= (nth 4 el) "[X]")
          return (progn
                   (let ((item (buffer-substring (car el) (car (last el)))))
                     (string-match "\\[X\\]\\(.*\\)$" item)
                     (match-string-no-properties 1 item))))))

(provide 'scimax-org-radio-checkbox)

;;; scimax-org-radio-checkbox.el ends here
