;;; org-db-agenda.el --- Agenda from org-db

;;; Commentary:
;; 

(defun org-db-agenda--candidates (before-date)
  "Get headings with deadlines before BEFORE-DATE
Examples:
today Due by today
+1w   Due in a week
+1m   Due in a month
"
  (interactive) 
  (let* ((headings (emacsql org-db [:select [headlines:level headlines:title headlines:tags
							     files:filename headlines:begin
							     headlines:deadline
							     files:last-updated
							     headlines:todo-keyword]
					    :from headlines
					    :inner :join files
					    :on (= files:rowid headlines:filename-id)
					;only get deadlines before now
					    :where (and
						    (= headlines:todo-keyword "TODO")						    
						    (< headlines:deadline $s1))
					    :order :by headlines:deadline :desc]
			    (float-time (org-read-date t t before-date))))
	 (candidates (cl-loop for (level title tags filename begin deadline last-updated todo-keyword) in headings
			      collect
			      (cons
			       (format "%20s|%100s|%20s|%s|%s"
				       (s-pad-right 20 " " (format-time-string "<%Y-%m-%d %a %H:%M:%S>" (seconds-to-time deadline)))
				       
				       (s-pad-right 100 " " (concat  (make-string level (string-to-char "*")) " "
								     todo-keyword " "
								     title))
				       (s-pad-right 20 " " (or tags ""))
				       filename last-updated)
			       (list
				:file filename
				:last-updated last-updated
				:begin begin
				:title title)))))
    candidates))

(defun org-db-agenda-transformer (candidate)
  (let* ((now (float-time (current-time)))
	 (ts (string-match org-element--timestamp-regexp candidate))
	 (es (when ts (float-time (org-timestamp-to-time (org-timestamp-from-string (match-string 0 candidate)))))))
    (if es
	(if (> es now)
	    (propertize candidate 'face '(:foreground "green4"))
	  (propertize candidate 'face '(:foreground "dark red")))
      candidate)))


(ivy-configure 'org-db-agenda :display-transformer-fn
	       #'org-db-agenda-transformer)


(defun org-db-agenda (before)
  (interactive (list (read-string "Before (e.g. +2w)"  "+2w")))
  (let* ((candidates (org-db-agenda--candidates before)))
    (ivy-read "heading: " candidates
	      :caller 'org-db-agenda
	      :action
	      '(1
		("o" org-db-headings--open "Open to heading.")))))


(provide 'org-db-agenda)

;;; org-db-agenda.el ends here
