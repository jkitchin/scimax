;;; scimax-ivy.el --- ivy functions for scimax

;;; Commentary:
;; 

(defun ivy-colors ()
  "List colors in ivy."
  (interactive)
  (ivy-read "Color: "
	    (progn
	      (save-selected-window
		(list-colors-display))
	      (prog1
		  (with-current-buffer (get-buffer "*Colors*")
		    (mapcar (lambda (line)
			      (append (list line) (s-split " " line t)))
			    (s-split "\n" (buffer-string))))
		(kill-buffer "*Colors*")))
	    :action
	    '(1
	      ("i" (lambda (line) 
		     (insert (elt line 1)))
	       "Insert name")
	      ("c" (lambda (line)
		     (kill-new (car line)))
	       "Copy name")
	      ("h" (lambda (line) 
		     (insert (car (last line))))
	       "Insert hex")
	      ("r" (lambda (line) 
		     (insert (format "%s" (color-name-to-rgb (elt line 1))))) 
	       "Insert RGB")
	      
	      ("m" (lambda (line) (message "%s" (cdr line)))))))

;; * ivy-top

(defcustom ivy-top-command
  "top -stats pid,command,user,cpu,mem,pstate,time -l 1"
  "Top command for `ivy-top'."
  :group 'scimax-ivy)

(defun ivy-top ()
  (interactive)
  (let* ((output (shell-command-to-string ivy-top-command))
	 (lines (progn
		  (string-match "TIME" output)
		  (split-string (substring output (+ 1 (match-end 0))) "\n")))
	 (candidates (mapcar (lambda (line)
			       (list line (split-string line " " t)))
			     lines)))
    (ivy-read "process: " candidates)))


;; * ivy-ps


;; a data structure for a process
(defstruct ivy-ps user pid)


(defun ivy-ps ()
  "WIP: ivy selector for ps.
TODO: sorting, actions."
  (interactive)
  (let* ((output (shell-command-to-string "ps aux | sort -k 3 -r"))
	 (lines (split-string output "\n"))
	 (candidates (mapcar
		      (lambda (line)
			(cons line
			      (let ((f (split-string line " " t)))
				(make-ivy-ps :user (elt f 0) :pid (elt f 1)))))
		      lines)))
    (ivy-read "process: " candidates
	      :action
	      '(1
		("k" (lambda (cand) (message "%s" (ivy-ps-pid cand))))))))

(provide 'scimax-ivy)

;;; scimax-ivy.el ends here
