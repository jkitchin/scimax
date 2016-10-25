;;; kitchingroup.el --- Kitchingroup utility functions

;;; Commentary:
;; 

(defun kitchingroup-weekly-report (&optional arg) 
  "Create/open this week's progress report.
With a prefix arg, specify the week to open.

This function will create a folder called reports/year-mm-dd and put a weekly-report template inside it, or open the one that exists.

The week beginning is defined by `calendar-week-start-day'. The
report is for the previous week."
  (interactive "P")
  
  (let* ((date (if arg (org-read-date) (format-time-string "%Y-%m-%d")))
	 (f (split-string date "-"))
	 (year (string-to-number (nth 0 f)))
	 (month (string-to-number (nth 1 f)))
	 (day (string-to-number (nth 2 f)))
	 (day-name (calendar-day-name (list month day year)))
	 ;; this is the day number within a week.
	 (day-number (loop for i from 0
			   if (string= day-name (aref calendar-day-name-array i))
			   return i))
	 
	 (week-beginning
	  (if (= day-number calendar-week-start-day)
	      day
	    (- day (- day-number calendar-week-start-day))))

	 (dir (format "reports/%s-%s-%s/" year month week-beginning)))
    
    (unless (file-directory-p (expand-file-name dir))
      (make-directory dir t))

    (let ((default-directory (expand-file-name dir)))
      (ox-manuscript-new-helm "weekly-progress-report"))))

(defalias 'kitchinhub-weekly-report 'kitchingroup-weekly-report)

(provide 'kitchingroup)

;;; kitchingroup.el ends here
