;;; scimax-autoformat-abbrev.el --- Autoformatting and abbreviations in scimax  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2021  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; These variables control the autoformatting capability in org-mode. They
;; default to t. To turn on autoformatting you have to turn on
;; `scimax-autoformat-mode'.

;; - `scimax-autoformat-superscripts'
;; - `scimax-autoformat-transposed-caps'
;; - `scimax-autoformat-ordinals'
;; - `scimax-autoformat-fractions'
;; - `scimax-autoformat-sentence-capitalization'

;; The variable `scimax-org-autoformat-functions' has a list of functions that
;; are called in the `scimax-org-autoformat'.

;; This module also defines a set of abbreviations that may be helpful. The definitions are defined in these variables which you can customize:
;; - `scimax-month-abbreviations'
;; - `scimax-weekday-abbreviations'
;; - `scimax-contraction-abbreviations'
;; - `scimax-transpositions-abbreviations'
;; - `scimax-chemical-formula-abbreviations'
;; - `scimax-misc-abbreviations'

;; They are disabled by default. To enable them, use commands like:
;; (scimax-toggle-abbrevs 'scimax-month-abbreviations 1) These abbreviations do
;; not expand in src-blocks or name/tblname/latex_headers. This is controlled by
;; the variable `scimax-abbrev-function' which is a function that takes no args
;; and returns non-nil if an abbreviation should be expanded at the current
;; point.

;; This module also sets up flyspell to save abbrevs so they autocorrect in the
;; future. The variable `scimax-save-spellcheck-abbrevs' defaults to t, and
;; saves any word you correct with flyspell as an abbrev. It also binds C-x C-i
;; to a function that calls ispell on a word, and then creates an abbrev for the
;; selection you make.
;;
;; Finally, this library advises `undo-tree-undo' so that you can easily undo an
;; abbrev expansion.

;;; Code:

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;;* Customization variables

;; ** Autoformatting

(defcustom scimax-autoformat-superscripts t
  "Determines if words ending in a number should be superscripted."
  :group 'scimax-autoformat
  :type 'boolean)

(defcustom scimax-autoformat-transposed-caps t
  "Determines if scimax autoformats transposed caps, .e.g. tHe to The."
  :group 'scimax-autoformat
  :type 'boolean)

(defcustom scimax-autoformat-ordinals t
  "Determines if scimax autoformats ordinal numbers."
  :group 'scimax-autoformat
  :type 'boolean)

(defcustom scimax-autoformat-fractions t
  "Determines if scimax autoformats fractions."
  :group 'scimax-autoformat
  :type 'boolean)

(defcustom scimax-autoformat-sentence-capitalization t
  "Determines if first word should be capitalized in a sentence."
  :group 'scimax-autoformat
  :type 'boolean)

(defcustom scimax-org-autoformat-functions
  '(scimax-org-autoformat-ordinals
    scimax-org-autoformat-fractions
    scimax-org-autoformat-transposed-caps
    scimax-org-autoformat-superscripts
    scimax-org-autoformat-sentence-capitalization
    scimax-org-autoformat-am-pm)
  "List of functions to call for autoformatting."
  :group 'scimax-autoformat
  :type '(repeat function))

;; ** Abbreviations

(defcustom scimax-abbrev-function
  (lambda ()
    (and
     (not (org-in-src-block-p))
     (save-excursion
       (beginning-of-line)
       (not (looking-at "#\\+\\(name\\|tblname\\|latex_header\\)")))))
  "Function that determines if scimax abbrevs are expanded.
The function should take no args and return non-nil if an
abbreviation should be expanded at the current point."
  :group 'scimax
  :type 'function)

;;*** Months
;; Note: I disabled the May abbrev. I found I use the word may a lot, and it was too annoying to undo.
(defcustom scimax-month-abbreviations
  '(("january" "January")
    ("february" "February")
    ("march" "March")
    ("april" "April")
    ;; ( "may" "May")
    ("june" "June")
    ("july" "July")
    ("august" "August")
    ("september" "September")
    ("october" "October")
    ("november" "November")
    ("december" "December")
    ;; abbreviations
    ("jan" "Jan.")
    ("feb" "Feb.")
    ("mar" "Mar.")
    ("apr" "Apr.")
    ("jun" "Jun.")
    ("jul" "Jul.")
    ("aug" "Aug.")
    ("sept" "Sept.")
    ("oct" "Oct.")
    ("nov" "Nov.")
    ("dec" "Dec."))
  "Months should be expanded as abbrevs."
  :group 'scimax-autoformat
  :type '(repeat (list string string)))

;;*** Weekdays

(defcustom scimax-weekday-abbreviations
  '(("monday" "Monday")
    ("tuesday" "Tuesday")
    ("wednesday" "Wednesday")
    ("thursday" "Thursday")
    ("friday" "Friday")
    ("saturday" "Saturday")
    ("sunday" "Sunday")
    ;; abbreviations
    ("mon" "Mon.")
    ("tue" "Tue.")
    ("wed" "Wed.")
    ("thur" "Thur.")
    ("fri" "Fri.")
    ("sat" "Sat.")
    ("sun" "Sun."))
  "Weekdays that should be expanded."
  :group 'scimax-autoformat
  :type '(repeat (list string string)))


;;*** Contractions

(defcustom scimax-contraction-abbreviations
  '(("arent" "are not")
    ("cant" "can not")
    ("couldnt" "could not")
    ("didnt" "did not")
    ("doesnt" "does not")
    ("dont" "do not")
    ("hadnt" "had not")
    ("hasnt" "has not")
    ("isnt" "is not")
    ("shouldnt" "should not")
    ("thats" "that is")
    ("wasnt" "was not")
    ("whos" "who is")
    ("wont" "will not")
    ("wouldve" "would have")
    ("wouldnt" "would not"))
  "List of (name expansion) to replace in abbrev-mode.
We do not use contractions in technical writing so this replaces
them with the full version."
  :group 'scimax-autoformat
  :type '(repeat (list string string)))


;;*** transposed letter words

(defcustom scimax-transposition-abbreviations
  '(("adn" "and")
    ("ahve" "have")
    ("fi" "if")
    ("fo" "of")
    ("nto" "not")
    ("teh" "the")
    ("hte" "the")
    ("htat" "that")
    ("htem" "them")
    ("iwth" "with")
    ("hwat" "what")
    ("waht" "what")
    ("wehn" "when"))
  "Common transpositions that should be fixed."
  :group 'scimax-autoformat
  :type '(repeat (list string string)))


;;*** Common Chemical Formulas

(defcustom scimax-chemical-formula-abbreviations
  '(("co2" "CO_{2}")
    ("n2" "N_{2}")
    ("h2" "H_{2}")
    ("h2o" "H_{2}O")
    ("ch4" "CH_{4}")
    ("c2h2" "C_{2}H_{2}")
    ("c2h4" "C_{2}H_{4}")
    ("c2h6" "C_{2}H_{6}"))
  "List of (abbrev expansion) for defining abbreviations."
  :group 'scimax-autoformat
  :type '(repeat (list string string)))


;;*** Misc. abbreviations

(defcustom scimax-misc-abbreviations
  '(("degC" "°C")
    ("degF" "°F")
    ("ang" "Å")
    ("tm" "™")
    ;; Some common names with umlauts/accents/slashed letters
    ("norskov" "Nørskov")
    ("schrodinger" "Schrödinger"))
  "Miscellaneous abbreviations"
  :group 'scimax-autoformat
  :type '(repeat (list string string)))


;; *** abbreviation list

(defcustom scimax-abbreviations
  '(scimax-month-abbreviations
    scimax-weekday-abbreviations
    scimax-contraction-abbreviations
    scimax-transposition-abbreviations
    scimax-chemical-formula-abbreviations
    scimax-misc-abbreviations)
  "List of abbreviation symbols."
  :group 'scimax-autoformat)


;;* Autoformat mode in org-mode

(defun scimax-org-autoformat-superscripts ()
  "Expand things like m2 to m^{2}."
  (interactive)
  (when (and scimax-autoformat-superscripts
	     (eq major-mode 'org-mode)
	     (not (org-in-src-block-p))
	     (let ((case-fold-search nil))
	       (looking-back "\\(?1:\\<[a-zA-Z]+\\)\\(?2:[0-9]+\\)\\([[:space:]]\\|[[:punct:]]\\)"
			     (line-beginning-position))))
    (undo-boundary)
    (save-excursion
      (replace-match "\\1^{\\2}"))))


(defun scimax-org-autoformat-transposed-caps ()
  "If you write hTe, fixes it to The."
  (interactive)
  (when (and scimax-autoformat-transposed-caps
	     (eq major-mode 'org-mode)
	     (not (org-in-src-block-p))
	     (let ((case-fold-search nil))
	       (looking-back "\\<\\(?1:[a-z]\\)\\(?2:[A-Z]\\)[a-z]+"
			     (line-beginning-position))))
    (undo-boundary)
    (save-excursion
      (replace-match (upcase (match-string 1)) nil nil nil 1)
      (replace-match (downcase (match-string 2)) nil nil nil 2))))


(defun scimax-org-autoformat-ordinals ()
  "Expand ordinal words to superscripted versions in org-mode.
1st to 1^{st}.
2nd to 2^{nd}
3rd to 3^{rd}
4th to 4^{th}"
  (interactive)
  (when (and scimax-autoformat-ordinals
	     (eq major-mode 'org-mode)
	     (not (org-in-src-block-p))
	     (looking-back "\\(?3:\\<\\(?1:[0-9]+\\)\\(?2:st\\|nd\\|rd\\|th\\)\\>\\)\\(?:[[:punct:]]\\|[[:space:]]\\)"
			   (line-beginning-position)))
    (undo-boundary)
    (save-excursion
      (replace-match "\\1^{\\2}" nil nil nil 3))))


(defun scimax-org-autoformat-fractions ()
  "Expand fractions to take up space."
  (interactive)
  (when (and scimax-autoformat-fractions
	     (eq major-mode 'org-mode)
	     (not (org-in-src-block-p))
	     (looking-back "\\(?3:\\<\\(1/4\\|1/2\\|3/4\\)\\>\\)\\(?:[[:punct:]]\\|[[:space:]]\\)"
			   (line-beginning-position)))
    (undo-boundary)
    (save-excursion
      (replace-match (cdr (assoc (match-string 3) '(("1/4" . "¼")
						    ("1/2" . "½")
						    ("3/4" . "¾"))))
		     nil nil nil 3))))


(defun scimax-org-autoformat-sentence-capitalization ()
  "Auto-capitalize first words of a sentence.
Either at the beginning of a line, or after a sentence end."
  (interactive)
  (when (and scimax-autoformat-sentence-capitalization
	     (eq major-mode 'org-mode)
	     (not (org-in-src-block-p))
	     (or (save-excursion (backward-char) (bolp))
		 (looking-back (concat (sentence-end) "[a-z]"))))
    (undo-boundary)
    (capitalize-word -1)))


(defun scimax-org-autoformat-am-pm ()
  "Replace am/pm with a.m. and p.m. and 12am/pm with midnight/noon."
  (interactive)
  (when (looking-back "[0-9]\\(am\\)" 3)
    (replace-match " a.m."
		   nil nil nil 1))
  
  (when (looking-back "[0-9]\\(pm\\)" 3)
    (replace-match " p.m."
		   nil nil nil 1))

  (when (looking-back "12pm" 4)
    (replace-match "noon"
		   nil nil nil 1))
  
  (when (looking-back "12am" 4)
    (replace-match "midnight"
		   nil nil nil 1)))


(defun scimax-org-autoformat ()
  "Autoformat functions.
This is run as a post-self-insert-hook in `scimax-autoformat-mode'."
  (mapc 'funcall scimax-org-autoformat-functions))


(define-minor-mode scimax-autoformat-mode
  "Toggle `scimax-autoformat-mode'.  Converts 1st to 1^{st} as you type."
  :init-value nil
  :lighter (" om")
  (if scimax-autoformat-mode
      (progn
	(add-hook 'post-self-insert-hook #'scimax-org-autoformat nil 'local)
	(setq-local org-pretty-entities nil)
	(org-toggle-pretty-entities))
    (setq-local org-pretty-entities t)
    (org-toggle-pretty-entities)
    (remove-hook 'post-self-insert-hook #'scimax-org-autoformat 'local)))


;;* Abbreviations

(defun scimax-toggle-abbrevs (sym &optional state)
  "Toggle the abbrevs in SYM.
If STATE is nil then toggle the state.
If STATE is 1 or a single prefix arg, turn on.
If STATE is -1 or a double prefix arg, turn off.
This saves the toggle state as a property on the variables that
define the definitions."
  (interactive
   (list
    (intern (ivy-read
	     "Collection: "
	     ))
    current-prefix-arg))

  (let ((currently-enabled (get sym 'enabled)))
    (cl-loop for (abbrev expansion) in (symbol-value sym)
	     do
	     (define-abbrev org-mode-abbrev-table
	       abbrev
	       (cond
		((member state '(1 (4)))
		 expansion)
		((member state '(-1 (16)))
		 nil)
		(t
		 (if currently-enabled
		     nil
		   expansion)))
	       nil
	       :enable-function scimax-abbrev-function))
    (put sym 'enabled (cond
		       ((member state '(1 (4)))
			t)
		       ((member state '(-1 (16)))
			nil)
		       (t (not currently-enabled))))
    (message "%s: %s"
	     sym
	     (cdr (assoc (get sym 'enabled) '((t . "enabled")
					      (nil . "disabled")))))))


(define-minor-mode scimax-abbrev-mode
  "Toggle scimax abbrev-mode on."
  :init-value nil
  (if scimax-abbrev-mode
      ;; turn on
      (dolist (sym scimax-abbreviations)
	(scimax-toggle-abbrevs sym 1))
    ;; turn off
    (dolist (sym scimax-abbreviations)
      (scimax-toggle-abbrevs sym -1))))


;;* Abbrev/spell-check

(define-key ctl-x-map "\C-i"
  #'endless/ispell-word-then-abbrev)

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil		; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))


;;* Advice on undo for abbrevs

(defadvice undo-tree-undo (around undo-abbrev-expansion nil activate)
  "Make undo unexpand an abbrev if it was the second to last thing that was done"
  (if (and last-abbrev-text
	   (= (point) (+ last-abbrev-location
			 (length (symbol-value last-abbrev))
			 1)))
      (let ((buffer-undo-list '()))
	(unexpand-abbrev))
    ad-do-it))


;;* The end
(provide 'scimax-autoformat-abbrev)
;;; scimax-autoformat-abbrev.el ends here
