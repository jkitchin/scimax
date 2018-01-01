;;; words.el --- Functions to operate on word at point or region  -*- lexical-binding: t -*-

;; Copyright(C) 2014,2015 John Kitchin

;; Author: John Kitchin  <jkitchin@andrew.cmu.edu>

;; Version: 0.1.0
;; Package-Requires: ((hydra "0"))

;;; Commentary:

;; These functions mostly provide easy access to web-based searches of the word
;; at point, or the selected words. The following functions are available.

;; - words-dictionary
;; - words-thesaurus
;; - words-atd :: a spell/grammar checker

;; - words-google
;; - words-twitter

;; - words-google-scholar
;; - words-scopus
;; - words-wos :: Search Web of Science
;; - words-crossref
;; - words-pubmed

;; - words-bibtex :: search default bibtex file

;; - words-mdfind :: search local computer with mdfind (Mac)
;; - words-finder :: search local computer with finder (Mac)

;; These functions just open websites for convenience.
;; - wos :: open Web of Science
;; - pubmed :: open pubmed
;; - scopus :: open Scopus

;; - words :: offers a menu of functions for word at point or region
;; - words/body will open a "hydra" menu.

;;; Code:
(require 'hydra)
(require 'url)
(require 'xml)

(setq hydra-is-helpful t)

;; * Dictionary/thesaurus/grammar
(defun words-dictionary ()
  "Look up word at point in an online dictionary."
  (interactive)
  (browse-url
   (format
    "http://dictionary.reference.com/browse/%s?s=t"
    (thing-at-point 'word))))


(defun words-thesaurus ()
  "Look up word at point in an online thesaurus."
  (interactive)
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (thing-at-point 'word))))


(defun words-atd ()
  "Send paragraph at point to After the deadline for spell and grammar checking."
  (interactive)

  (let* ((url-request-method "POST")
	 (url-request-data (format
			    "key=some-random-text-&data=%s"
			    (url-hexify-string
			     (thing-at-point 'paragraph))))
	 (xml  (with-current-buffer
		   (url-retrieve-synchronously
		    "http://service.afterthedeadline.com/checkDocument")
		 (xml-parse-region url-http-end-of-headers (point-max))))
	 (results (car xml))
	 (errors (xml-get-children results 'error)))

    (switch-to-buffer-other-frame "*ATD*")
    (erase-buffer)
    (dolist (err errors)
      (let* ((children (xml-node-children err))
	     ;; for some reason I could not get the string out, and had to do this.
	     (s (car (last (nth 1 children))))
	     ;; the last/car stuff doesn't seem right. there is probably
	     ;; a more idiomatic way to get this
	     (desc (last (car (xml-get-children children 'description))))
	     (type (last (car (xml-get-children children 'type))))
	     (suggestions (xml-get-children children 'suggestions))
	     (options (xml-get-children (xml-node-name suggestions) 'option))
	     (opt-string  (mapconcat
			   (lambda (el)
			     (when (listp el)
			       (car (last el))))
			   options
			   " ")))

	(insert (format "** %s ** %s
Description: %s
Suggestions: %s

" s type desc opt-string))))))


;; * Web functions
(defun words-google ()
  "Google the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://www.google.com/search?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))

(defun words-twitter ()
  "Search twitter for word at point or selection."
  (interactive)
  (browse-url
   (format
    "https://twitter.com/search?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))


;; * Scientific search functions
(defun words-google-scholar ()
  "Google scholar the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s"
    (if (region-active-p)
	(url-hexify-string (buffer-substring (region-beginning)
					     (region-end)))
      (thing-at-point 'word)))))


(defun words-wos ()
  "Open the word at point or selection in Web of Science."
  ;; the url was derived from this page: http://wokinfo.com/webtools/searchbox/
  (interactive)
  (browse-url
   (format "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
    (if (region-active-p)
	(mapconcat 'identity (split-string
			      (buffer-substring (region-beginning)
						(region-end))) "+")
      (thing-at-point 'word)))))


(defun words-scopus ()
  "Scopus the word at point or selection."
  (interactive)
  (browse-url
   (format
    "http://www.scopus.com//search/submit/basic.url?field1=TITLE-ABS-KEY&searchterm1=%s"
    (if (region-active-p)
	(mapconcat 'identity (split-string
			      (buffer-substring (region-beginning)
						(region-end))) "+")
      (thing-at-point 'word)))))


(defun words-crossref ()
  "Search region or word at point in CrossRef."
  (interactive)
  (browse-url
   (format
    "http://search.crossref.org/?q=%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (thing-at-point 'word)))))


(defun words-pubmed ()
  "Search region or word at point in pubmed."
  (interactive)
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (thing-at-point 'word)))))


(defun words-arxiv ()
  "Search region or word at point in arxiv.org."
  (interactive)
  (browse-url
   (format
    "http://arxiv.org/find/all/1/all:+AND+%s/0/1/0/all/0/1"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (thing-at-point 'word)))))


(defun words-semantic-scholar ()
  "Search region or word at point in www.semanticscholar.org."
  (interactive)
  (browse-url
   (format
    "https://www.semanticscholar.org/search?q=%s"
    (if (use-region-p)
	(url-hexify-string (buffer-substring
			    (region-beginning)
			    (region-end)))
      (thing-at-point 'word)))))


;; ** Convenience functions for scientific queries
;; These just open websites, with no search queries.

(defun wos ()
  "Open Web of Science search page in browser."
  (interactive)
  (browse-url "http://apps.webofknowledge.com"))


(defun pubmed ()
  "Open Pubmed in browser."
  (interactive)
  (browse-url "http://www.ncbi.nlm.nih.gov/pubmed"))


(defun scopus ()
  "Open Scopus in browser."
  (interactive)
  (browse-url "http://www.scopus.com"))


(defun crossref ()
  "Open Crossref in browser."
  (interactive)
  (browse-url "http://search.crossref.org"))


;; * Bibtex search

(defun words-bibtex ()
  "Find selected region or word at point in variable `reftex-default-bibliography'."
  (interactive)
  (multi-occur
   (mapcar (lambda (f) (find-file-noselect f))
	   reftex-default-bibliography)
   (if (use-region-p)
       (buffer-substring
	(region-beginning)
	(region-end))
     (thing-at-point 'word))))

;; * Search functions for Mac
(defvar words-voice "Samantha"
  "Mac voice to use for speaking.")

(defun words-speak (&optional text speed)
  "Speak word at point or region or TEXT.  Mac only."
  (interactive)
  (setq speed (number-to-string (or speed 180)))
  (unless text
    (setq text (if (use-region-p)
		   (buffer-substring
		    (region-beginning) (region-end))
		 (thing-at-point 'word))))
  ;; escape some special applescript chars
  (setq text (replace-regexp-in-string "\\\\" "\\\\\\\\" text))
  (setq text (replace-regexp-in-string "\"" "\\\\\"" text))
  ;; (do-applescript
  ;;  (format
  ;;   "say \"%s\" using \"%s\""
  ;;   text
  ;;   words-voice))
  ;; (start-process "my-thing" "foo" "say" "-v" words-voice text "-r" speed)
  ;; (call-process "say" nil nil nil (mapconcat 'identity (list "-v" words-voice text "-r" speed) " "))
  (shell-command (format "say -v %s \"%s\" -r %s" words-voice text speed)))

(defvar words-languages
  '()
  "List of cons cells (language . code).")

(defvar words-speakers
  '(("German" . "Anna")
    ("Chinese" . "Ting-Ting"))
  "Speakers for different languages.")

(setq words-languages  '(("German" . "de")
			 ("Italian" . "it")
			 ("Chinese" . "zh")
			 ("Spanish" . "es")))

(defun words-translate (to-language)
  "Translate word at point or selection TO-LANGUAGE.
http://mymemory.translated.net TO-LANGUAGE is the code, see
http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry.
Assumes selected code is in English."
  (interactive
   (list
    (ido-completing-read
     "Language: "
     (mapcar 'car words-languages) nil t)))
  (let* ((text (if (use-region-p)
		   (buffer-substring
		    (region-beginning)
		    (region-end))
		 (thing-at-point 'word)))
	 (url (format "http://mymemory.translated.net/api/get?q=%s!&langpair=en|%s"
		      text
		      (cdr (assoc to-language words-languages))))
	 (json (with-current-buffer
		   (url-retrieve-synchronously url)
		 (json-read-from-string
		  (buffer-substring url-http-end-of-headers (point-max))))))
    (let ((words-voice (or (cdr (assoc to-language words-speakers))
			   "Vicki")))
      (words-speak
       (cdr (assoc 'translatedText (cdr (assoc 'responseData json))))))

    (message "Translation: %s"
	     (cdr (assoc 'translatedText (cdr (assoc 'responseData json)))))))


(defun words-mdfind ()
  "Search for file names matching word or selection at point using mdfind.
Opens an org-buffer with links to results.  Mac only."
  (interactive)
  (let ((query (if (use-region-p)
		   (buffer-substring
		    (region-beginning)
		    (region-end))
		 (thing-at-point 'word))))
    (switch-to-buffer-other-window "*mdfind*")
    (erase-buffer)
    (insert
     (mapconcat
      (lambda (x)
	(format "[[%s]]" x))
      (split-string
       (shell-command-to-string
	(format "mdfind -name %s"
		(shell-quote-argument query)))
       "\n")
      "\n"))
    (org-mode)))

(defun words-swiper-all ()
  "Run swiper-all on the word at point or region."
  (interactive)
  (let ((query (if (use-region-p)
		   (buffer-substring
		    (region-beginning)
		    (region-end))
		 (thing-at-point 'word))))
    (let ((ivy-initial-inputs-alist `((swiper-multi . ,query))))
      (swiper-all))))



(defun words-finder ()
  "Open Mac Finder with search of word at point or selection."
  (interactive)
  (let* ((query (if (use-region-p)
		    (buffer-substring
		     (region-beginning)
		     (region-end))
		  (thing-at-point 'word)))
	 (applescript (concat
		       "tell application \"Finder\" to activate
tell application \"System Events\"
	tell process \"Finder\"
		click menu item \"Find\" of menu \"File\" of menu bar 1
		keystroke " (format "\"%s\"" query )
		"
	end tell
end tell")))
    (message "%s" applescript)

    ;; from org-mac-link
    (do-applescript applescript)))


;; * A hydra interface to words

;; hydra (http://oremacs.com/2015/01/20/introducing-hydra/) is a relatively new
;; menu type interface to select actions with single key strokes. It is a nicer
;; implementation than what I setup in the words function above.

;; https://github.com/abo-abo/hydra

(defhydra words-hydra (:color blue :hint nil)
  "
words
_d_: Dictionary  _g_: Google           _T_: Twitter _b_: Bibtex      _k_: Speak
_t_: Thesaurus   _G_: Google Scholar   ^ ^          _f_: Mac finder  _r_: Translate
_s_: Spell       _c_: Crossref         ^ ^          _w_: swiper-all
^ ^              _S_: Scopus           ^ ^          _M_: Mac mdfind
^ ^              _W_: Web Of Science
^ ^              _p_: Pubmed
^ ^              _a_: Arxiv
^ ^              _o_: Semantic Scholar
_q_: quit
"
  ("d" words-dictionary "dictionary")
  ("t" words-thesaurus "thesaurus")
  ("s" words-atd "spell/grammar")
  ("g" words-google "google")
  ("T" words-twitter "Twitter")
  ("W" words-wos "Web of Science")
  ("G" words-google-scholar "Google scholar")
  ("c" words-crossref "CrossRef")
  ("S" words-scopus "Scopus") 
  ("o" words-semantic-scholar "Semantic Scholar")
  ("p" words-pubmed "Pubmed")
  ("a" words-arxiv "Arxiv")
  ("b" words-bibtex "bibtex")
  ("f" words-finder "Mac Finder")
  ("w" words-swiper-all "swiper-all")
  ("M" words-mdfind "mdfind")
  ("k" words-speak "Speak")
  ("r" words-translate "Translate")
  ("q" nil "cancel"))


;;; End:
(provide 'words)
;;; words.el ends here
