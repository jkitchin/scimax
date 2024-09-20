;;; scimax-ldap.el --- An ivy interface to LDAP services


;;; Commentary:
;; An ivy interface to an LDAP server

;;; Code:

(defvar ldap-server "ldap.andrew.cmu.edu"
  "Hostname of your LDAP server.")

(defun ldap-query (query-string)
  "Send QUERY-STRING to our ldap server.
Parse results into a list of p-lists for each entry returned."
  (interactive "sLDAP query: ")
  (let ((output (butlast (split-string
			  (shell-command-to-string
			   (format (concat  "ldapsearch -x -LLL "
					    "-h %s "
					    "-b ou=Person,dc=cmu,dc=edu %s")
				   ldap-server
				   query-string))
			  "\n")))
	(lines '())
	(result '())
	(results '(())))
    ;; cleanup trailing lines and ignore initial lines
    (loop for line in output
	  do
	  (cond
	   ;; ignore starting lines
	   ((s-starts-with? "SASL" line)
	    nil)
	   ;; join lines that run over
	   ((s-starts-with? " " line)
	    (setf (car (last lines))
		  (concat (car (last lines)) line)))
	   ;; ignore this
	   ((string-match "Size limit exceeded" line)
	    nil)
	   (t
	    (add-to-list 'lines line t))))

    ;; now we need to parse the lines. A new entry starts with a dn: line.
    (dolist (line lines)
      (cond
       ((s-starts-with? "dn:" line)
	;; add new entry
	(add-to-list 'results `(:dn ,line)))
       ((string-match ":" line)
	(let* ((s (split-string line ":"))
	       (prop (intern (concat ":" (s-trim (car s)))))
	       (val (s-trim (cadr s))))
	  (setf (car results) (plist-put (car results) prop val))))))
    ;; last result seems to be nil so we drop it
    (-filter (lambda (x) (not (null x))) results)))


;; [2020-07-28 Tue] This seems to be broken now, commenting it out.
;; (defun cmu-directory (name)
;;   "Look up NAME in CMU directory.
;; NAME can be partial name or andrewid."
;;   (interactive "sNAme or AndrewID: ")
;;   (let* ((url-request-method "POST")
;; 	 (url-request-data
;; 	  (mapconcat
;; 	   'identity
;; 	   `(,(format  "search[generic_search_terms]=%s" name)
;; 	     "authenticity_token=foGA4weA2fcDn3HIQ4eSeLUxss/g9pujB/zIIcnvl5U="
;; 	     "commit=Search")
;; 	   "&"))
;; 	 (url "https://directory.andrew.cmu.edu/search/basic/results"))
;;     (with-current-buffer  (url-retrieve-synchronously url)
;;       (let ((content (buffer-substring
;; 		      url-http-end-of-headers (point-max)))
;; 	    (fname (make-temp-file "cmu" nil ".html")))
;; 	(setq content
;; 	      (replace-regexp-in-string
;; 	       "href=\"/search"
;; 	       "href=\"https://directory.andrew.cmu.edu/search"
;;                content))
;; 	(setq content
;; 	      (replace-regexp-in-string
;; 	       "action=\"/search"
;; 	       "action=\"https://directory.andrew.cmu.edu/search"
;;                content))
;; 	(with-temp-file fname
;; 	  (insert content))
;; 	(browse-url fname)))))


(defun ivy-ldap (query-string)
  (interactive "sLDAP query: ")
  (let ((candidates (mapcar
		     (lambda (x)
		       (cons
			(format
			 "%20s|%30s|%30s|%20s|%s"
			 (s-truncate
			  20
			  (or (plist-get x :title) " "))
			 (or (plist-get x :cn) "")
			 (or (plist-get x :mail) "")
			 (or (plist-get x :cmuDisplayAddress) "")
			 (or (plist-get x :telephoneNumber) " "))
			x))
		     (ldap-query
		      (if (string-match "=" query-string)
			  query-string
			(concat "cn=*" query-string "*"))))))

    (ivy-read "Select: " candidates
	      :action '(1
			("e" (lambda (x)
			       (compose-mail)
			       (message-goto-to)
			       (insert (or
					(plist-get x :cmuPreferredMail)
					(plist-get x :mail)
					(concat (plist-get x :cmuAndrewID)
						"@andrew.cmu.edu")))
			       (message-goto-subject))
			 "Email")
			("P" (lambda (x)
			       (org-entry-put (point) "EMAIL" (or
							       (plist-get (cdr x) :cmuPreferredMail)
							       (plist-get (cdr x) :mail)
							       (concat (plist-get (cdr x) :cmuAndrewID)
								       "@andrew.cmu.edu"))))
			 "Add EMAIL property")
			;; ("p" (lambda (x)
			;;        (cisco-call
			;; 	(plist-get (cdr x) :telephoneNumber)))
			;;  "Call")
			("c" (lambda (x)
			       (org-insert-heading)
			       (insert (plist-get (cdr x) :cn) "\n")
			       (cl-loop for key in (plist-get-keys (cdr x))
					do
					(insert (format "- %s :: %s\n" key (plist-get (cdr x) key))))
			       (org-entry-put (point) "EMAIL" (or
							       (plist-get (cdr x) :cmuPreferredMail)
							       (plist-get (cdr x) :mail)
							       (concat (plist-get (cdr x) :cmuAndrewID)
								       "@andrew.cmu.edu"))))
			 "Insert contact heading")

			("w" (lambda (x)
			       (kill-new
				(format
				 "%s <%s>"
				 (plist-get (cdr x) :cn)
				 (plist-get (cdr x) :mail))))
			 "Copy name and email")
			("n" (lambda (x)
			       (insert
				(format
				 "%s <%s>"
				 (plist-get (cdr x) :cn)
				 (or
				  (plist-get (cdr x) :cmuPreferredMail)
				  (plist-get (cdr x) :mail)
				  (concat (plist-get (cdr x) :cmuAndrewID)
					  "@andrew.cmu.edu")))))
			 "insert name and email")
			("f" (lambda (x)
			       (switch-to-buffer
				(get-buffer-create "*ivy ldap*"))
			       (erase-buffer)
			       (org-mode)
			       (dolist (key (cl-loop
					     for key in x by #'cddr
					     collect key))
				 (insert (format "|%s | %s|\n"
						 key (plist-get (cdr x) key))))
			       (previous-line)
			       (org-ctrl-c-ctrl-c)

			       (goto-char (point-max))

			       (insert "\n")

			       (when (plist-get x :telephoneNumber)
				 (insert
				  (format "[[elisp:(cisco-call \"%s\")][Call]]  "
					  (plist-get x :telephoneNumber))))

			       (when (or
				      (plist-get (cdr x) :cmuPreferredMail)
				      (plist-get (cdr x) :mail)
				      (concat (plist-get (cdr x) :cmuAndrewID)
					      "@andrew.cmu.edu"))
				 (insert
				  (format "[[elisp:(progn (compose-mail) (message-goto-to) (insert \"%s\")(message-goto-subject))][Send email]]"
					  (or
					   (plist-get (cdr x) :cmuPreferredMail)
					   (plist-get (cdr x) :mail)
					   (concat (plist-get (cdr x) :cmuAndrewID)
						   "@andrew.cmu.edu")))))

			       (insert "\n\npress q to quit.")
			       (setq buffer-read-only t)
			       (use-local-map (copy-keymap org-mode-map))
			       (local-set-key "q"
					      #'(lambda ()
						  (interactive)
						  (quit-window t))))
			 "Information")
			("r" (lambda (candidate)
			       (ivy-ldap (read-string "LDAP Query: ")))
			 "New search")))))

(provide 'scimax-ldap)

;;; scimax-ldap.el ends here
