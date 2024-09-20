;;; words.el --- Functions to operate on word at point or region  -*- lexical-binding: t -*-

;; Copyright(C) 2014-2021 John Kitchin

;; Author: John Kitchin  <jkitchin@andrew.cmu.edu>

;; Version: 0.2.0
;; Package-Requires: ((hydra "0"))

;;; Commentary:

;; These functions mostly provide easy access to web-based searches of the word
;; at point, or the selected words. The following functions are available.

;; - `words-dictionary'
;; - `words-thesaurus'
;; - `words-gramma' uses gramma to do a grammar check.

;; These functions search various online search spaces
;; - `words-google'
;; - `words-twitter'
;; - `words-google-scholar'
;; - `words-scopus'
;; - `words-wos' :: Search Web of Science
;; - `words-crossref'
;; - `words-pubmed'
;; - `words-arxiv'
;; - `words-semantic-scholar'

;; - `words-bibtex' :: search for words in your bibtex files

;; - `words-mdfind' :: search local computer with mdfind (Mac)
;; - `words-finder' :: search local computer with finder (Mac)
;; - `words-swiper-all' :: search all open buffers

;; These functions just open websites for convenience.
;; - `wos' :: open Web of Science
;; - `pubmed' :: open pubmed
;; - `scopus' :: open Scopus

;; Speaking and translating: - `words-speak' will use a speech program to read the
;; selection out loud. See `words-speech-program' and
;; `words-speech-program-options' for configuration.


;; - `words/body' will open a "hydra" menu.

;;; Code:
(require 'hydra)
(require 'url)
(require 'xml)

(setq hydra-is-helpful t)

;; to quiet byte-compile error
(defvar url-http-end-of-headers)


(defcustom words-speech-program "say"
  "Program to speak words out loud.
Mac options include \"say\", but you can use homebrew to install
espeak too."
  :group 'words)


(defcustom words-speech-program-options "-v Samantha -r 180"
  "Options specific to `words-speech-program'.
It is assumed the text to be spoken will be the last argument."
  :group 'words)


(defcustom words-translate-shell-program "trans"
  "Name of the translate-shell executable."
  :group 'words)


(defcustom words-translate-shell-native-language "en"
  "Two letter code for the default native language to translate
from. Defaults to English (en).
See http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry."
  :group 'words)


(defcustom words-translate-shell-options "-p"
  "Options for the translate shell executable.
-p makes it speak the translation out loud.
Do not include the translate language here, it is added in
`words-translate-shell'."
  :group 'words)


(defcustom words-translate-preferred-language nil
  "A two letter language code for the preferred language to translate to.
See http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry."
  :group 'words)


(defcustom words-translate-speak nil
  "If non-nil use `words-speak' to read the translation out loud in `words-translate-shell'."
  :group 'words)


(defun words-at-point ()
  "The active region, or word at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word)))


;; * Dictionary/thesaurus/grammar

(defun words-dictionary (text)
  "Look up TEXT in an online dictionary."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://dictionary.reference.com/browse/%s?s=t"
    (url-hexify-string text))))


(defun words-thesaurus (text)
  "Look up TEXT in an online thesaurus."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://www.thesaurus.com/browse/%s"
    (url-hexify-string text))))


(defun words-gramma (text)
  "Check TEXT grammar with gramma.
This pops to *gramma* with the output."
  (interactive (list (words-at-point)))

  (unless (executable-find "gramma")
    (error "gramma was not found. Go to https://caderek.github.io/gramma/"))

  (pop-to-buffer "*gramma*")
  (erase-buffer)
  (insert (shell-command-to-string (format "gramma listen -p %S" text))))


;; * Web functions

(defun words-google (text)
  "Google TEXT."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://www.google.com/search?q=%s"
    (url-hexify-string text))))


(defun words-twitter (text)
  "Search twitter for TEXT."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "https://twitter.com/search?q=%s"
    (url-hexify-string text))))


;; * Scientific search functions

(defun words-google-scholar (text)
  "Search TEXT in Google scholar."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://scholar.google.com/scholar?q=%s"
    (url-hexify-string text))))


(defun words-wos (text)
  "Search TEXT in Web of Science."
  ;; the url was derived from this page: http://wokinfo.com/webtools/searchbox/
  (interactive (list (words-at-point)))
  (browse-url
   (format "http://gateway.webofknowledge.com/gateway/Gateway.cgi?topic=%s&GWVersion=2&SrcApp=WEB&SrcAuth=HSB&DestApp=UA&DestLinkType=GeneralSearchSummary"
	   (mapconcat 'identity (split-string text) "+"))))


(defun words-scopus (text)
  "Search TEXT in Scopus."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://www.scopus.com//search/submit/basic.url?field1=TITLE-ABS-KEY&searchterm1=%s"
    (mapconcat 'identity (split-string text) "+"))))


(defun words-crossref (text)
  "Search TEXT in CrossRef."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://search.crossref.org/?q=%s&from_ui=yes"
    (replace-regexp-in-string "%20" "+"
			      (url-hexify-string text)))))


(defun words-pubmed (text)
  "Search TEXT in pubmed."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "http://www.ncbi.nlm.nih.gov/pubmed/?term=%s"
    (url-hexify-string text))))


(defun words-arxiv (text )
  "Search TEXT in arxiv.org."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "https://arxiv.org/search/?query=%s&searchtype=all&abstracts=show&order=-announced_date_first&size=50"
    (url-hexify-string text))))


(defun words-semantic-scholar (text)
  "Search TEXT in www.semanticscholar.org."
  (interactive (list (words-at-point)))
  (browse-url
   (format
    "https://www.semanticscholar.org/search?q=%s"
    (url-hexify-string text))))


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

(defun words-bibtex (text)
  "Find TEXT in files listed in `bibtex-completion-bibliography'."
  (interactive (list (words-at-point)))
  (multi-occur
   (mapcar (lambda (f)
	     (find-file-noselect f))
	   (if (listp bibtex-completion-bibliography)
	       bibtex-completion-bibliography
	     (list bibtex-completion-bibliography)))
   text))

;; * Speech

(defun words-speak (&optional text)
  "Speak TEXT.
Uses `words-speech-program' and `words-speech-program-options'."
  (interactive (list (words-at-point)))
  (shell-command
   (concat words-speech-program " "
	   words-speech-program-options " "
	   (format "\"%s\"" text))))


;; * Translation functions

(defun words-get-language-data ()
  "Retrieve a list of languages.
This is a list of lists ((tag value)...) for each language."
  (with-current-buffer (url-retrieve-synchronously
			"http://www.iana.org/assignments/language-subtag-registry/language-subtag-registry")
    (goto-char (point-min))
    (search-forward "%%")
    (forward-line)
    (let ((languages '())
	  (language))
      (while (not (eobp))
	(catch 'block
	  (when (looking-at "%%")
	    (push language languages)
	    (setq language '())
	    (forward-line)
	    (throw 'block t))
	  (let ((fields (split-string (buffer-substring
				       (line-beginning-position)
				       (line-end-position))
				      ":")))
	    (when (= 2 (length fields))
	      (push (cons (cl-first fields)
			  (string-trim (cl-second fields)))
		    language)))
	  (forward-line)))
      languages)))


(defun words-language-candidates ()
  "Returns an a-list of (language . two-letter code) for languages.
Some languages have multiple mappings of language name to code.
This captures them all."
  (let ((candidates '()))
    (cl-loop for language in (words-get-language-data)
	     do
	     ;; collect all the descriptions
	     (cl-loop for (tag . value) in language
		      if (string= tag "Description")
		      do
		      (push (cons value (cdr (assoc "Subtag" language))) candidates)))
    candidates))


(defun words-choose-translate-language ()
  "Use completion to choose a translation language."
  (let ((candidates (words-language-candidates)))
    (cdr (assoc (completing-read "Translate to: " candidates) candidates))))


(defun words-translate (&optional text arg)
  "Translate TEXT to `words-translate-preferred-language'.

If words-translate-preferred-language is nil or there is a prefix
ARG, use completion to choose the language.

Translation is done with http://mymemory.translated.net.
Not every language is supported.

The translation will appear as a message.

If `words-translate-speak' is non-nil, or a double prefix arg is
used the translation will be read aloud by `words-speak'.

Note: there are usage limits of 1000
words/day (https://mymemory.translated.net/doc/usagelimits.php)."
  (interactive (list (words-at-point) current-prefix-arg))
  (let* ((to-language (if (or arg (null words-translate-preferred-language))
			  (words-choose-translate-language)
			words-translate-preferred-language))
	 (url (format "http://mymemory.translated.net/api/get?q=%s!&langpair=en|%s"
		      text
		      to-language))
	 (json (with-current-buffer
		   (url-retrieve-synchronously url)
		 (json-read-from-string
		  (buffer-substring url-http-end-of-headers (point-max))))))

    (when (or words-translate-speak
	      (equal '(16) arg))
      (words-speak
       (cdr (assoc 'translatedText (cdr (assoc 'responseData json))))))

    (pop-to-buffer "*words-translate*")
    (erase-buffer)
    (insert (cdr (assoc 'translatedText (cdr (assoc 'responseData json)))))))


;; ** using translate-shell

(defun words-translate-shell (&optional text arg)
  "Use translate-shell to translate the TEXT.
TEXT is translated to `words-translate-preferred-language',
unless you use a prefix ARG, then choose the language instead.

Assumes the TEXT is in `words-translate-shell-native-language'.
Use a double prefix arg to select the TEXT language.


https://github.com/soimort/translate-shell
On Mac: brew reinstall translate-shell"
  (interactive (list
		(words-at-point)
		current-prefix-arg))


  (let (translate-from translate-to candidates)
    (cond

     ;; Here you choose the to language if you didn't set one or use a single prefix.
     ((or (null words-translate-preferred-language) (equal '(4) arg))
      (setq translate-from words-translate-shell-native-language
	    translate-to (words-choose-translate-language)))

     ;; Here you choose the from and to target
     ((equal '(16) arg)
      (setq candidates (words-language-candidates)
	    translate-from (cdr (assoc (completing-read "Translate from: " candidates) candidates))
	    translate-to (cdr (assoc (completing-read "Translate to: " candidates) candidates))))
     (t
      (setq translate-from words-translate-shell-native-language
	    translate-to words-translate-preferred-language)))

    (shell-command (string-join
		    (list
		     words-translate-shell-program
		     words-translate-shell-options
		     (format "%s:%s" translate-from translate-to)
		     (shell-quote-argument text))
		    " "))))


;; * Search functions for Mac

(defun words-swiper-all (text)
  "Run `swiper-all' on TEXT."
  (interactive (list (words-at-point)))
  (let ((ivy-initial-inputs-alist `((swiper-all . ,text))))
    (swiper-all)))


;; (defun words-mdfind (query)
;;   "Search mdfind with QUERY.
;; Opens an org-buffer with links to results.  Mac only."
;;   (interactive (list (words-at-point)))
;;   (switch-to-buffer-other-window "*mdfind*")
;;   (erase-buffer)
;;   (insert
;;    (mapconcat
;;     (lambda (x)
;;       (format "[[%s]]" x))
;;     (split-string
;;      (shell-command-to-string
;;       (format "mdfind -name %s"
;; 	      (shell-quote-argument query)))
;;      "\n")
;;     "\n"))
;;   (org-mode))

(defun words-mdfind-function (str)
  (or
   (ivy-more-chars)
   (progn
     (counsel--async-command
      (format "mdfind -name \"%s\"" str))
     '("" "working..."))))


(defun words-mdfind (query)
  "Search mdfind with QUERY.
Opens an org-buffer with links to results.  Mac only."
  (interactive (list (words-at-point)))
  (ivy-read "mdfind: " #'words-mdfind-function
	    :initial-input query
	    :dynamic-collection t
	    :action #'find-file))


(defun words-finder (query)
  "Open Mac Finder with QUERY."
  (interactive (list (words-at-point)))
  ;; from org-mac-link
  (do-applescript (concat
		   "tell application \"Finder\" to activate
tell application \"System Events\"
	tell process \"Finder\"
		click menu item \"Find\" of menu \"File\" of menu bar 1
		keystroke " (format "\"%s\"" query)
		   "
	end tell
end tell")))


;; * A hydra interface to words


(defhydra words-hydra (:color blue :hint nil)
  "
words"

  ("d" words-dictionary "dictionary" :column "Lookup")
  ("t" words-thesaurus "thesaurus" :column "Lookup")
  ("g" words-gramma "grammar" :column "Lookup")

  ("wg" words-google "google" :column "WWW")
  ("wt" words-twitter "Twitter" :column "WWW")
  ("ww" words-wos "Web of Science" :column "WWW")
  ("ws" words-google-scholar "Google scholar" :column "WWW")
  ("wc" words-crossref "CrossRef" :column "WWW")
  ("wu" words-scopus "Scopus" :column "WWW")
  ("wh" words-semantic-scholar "Semantic Scholar" :column "WWW")
  ("wp" words-pubmed "Pubmed" :column "WWW")
  ("wa" words-arxiv "Arxiv" :column "WWW")

  ("sb" words-bibtex "bibtex" :column "search")
  ("sf" words-finder "Mac Finder" :column "search")
  ("sa" words-swiper-all "swiper-all" :column "search")
  ("sm" words-mdfind "mdfind" :column "search")

  ("k" words-speak "Speak" :column "audio")

  ("l" words-translate-shell "Translate-shell" :column "translate")
  ("L" words-translate "Translate" :column "translate")

  ("q" nil "quit"))


;;; End:
(provide 'words)
;;; words.el ends here
