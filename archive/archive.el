;;; archive.el --- Archived code  -*- lexical-binding: t; -*-

;;; Commentary:
;;

;; * Navigation

(defvar navy-l 'forward-char
  "The next item in a forward sense.")

(defvar navy-j 'backward-char
  "The previous item in a backward sense.")

(defvar navy-i 'previous-line
  "The previous item in an up sense.")

(defvar navy-k 'next-line
  "The next item in a down sense.")

(defvar navy-semicolon 'avy-goto-char
  "Command bound to ;.")

(defvar navy-quote 'avy-goto-line
  "Command bound to '.")

(defvar navy-comma 'avy-goto-char-2
  "Command bound to ,")

(defvar navy-period 'avy-goto-word-0
  "Command bound to .")

(defvar navy-slash 'end-of-visual-line
  "The end of an item.")

(defvar navy-h 'beginning-of-visual-line
  "Command bound to h, usually a beginning of command.")

(defvar navy-mode "char"
  "The active mode.")


(defhydra navy (:color red :hint nil)
  "
%s(format \"%s-mode\" navy-mode)
%s(make-string (length (symbol-name navy-j)) ? )     _i_: %`navy-i
%`navy-j :_j_     _l_: %`navy-l     _;_: %`navy-semicolon  _'_: %`navy-quote
%s(make-string (length (symbol-name navy-j)) ? )     _k_: %`navy-k
  _,_: %`navy-comma _._: %`navy-period _/_: %`navy-slash
  point-min: _<_    _>_: point-max

"
  ("j" (funcall navy-j))
  ("l" (funcall navy-l))
  ("i" (funcall navy-i))
  ("k" (funcall navy-k))

  ("q" nil "quit" :color blue)

  ("h" (call-interactively navy-h))

  (";" (call-interactively navy-semicolon))
  ("'" (call-interactively navy-quote))

  ("," (call-interactively navy-comma))
  ("." (call-interactively navy-period))
  ("/" (call-interactively navy-slash))

  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ;; these are different modes
  ;; char

  ("c" (lambda ()
	 (interactive)
	 (setq navy-mode "char"
	       navy-j 'backward-char
	       navy-i 'previous-line
	       navy-l 'forward-char
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-char-in-line
	       navy-period 'avy-goto-word-1))
   "char mode")

  ("w" (lambda ()
	 (interactive)
	 (setq navy-mode "word"
	       navy-j 'backward-word
	       navy-i 'previous-line
	       navy-l 'forward-word
	       navy-k 'next-
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "word mode")

  ("s" (lambda ()
	 (interactive)
	 (setq navy-mode "sentence"
	       navy-j 'backward-sentence
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-l 'forward-sentence
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "sentence mode")

  ("p" (lambda ()
	 (interactive)
	 (setq navy-mode "paragraph"
	       navy-j 'backward-paragraph
	       navy-l 'forward-paragraph
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "paragraph mode")

  ("g" (lambda ()
	 (interactive)
	 (setq navy-mode "page"
	       navy-j 'backward-page
	       navy-l 'forward-page
	       navy-i 'backward-page
	       navy-k 'forward-page
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "page mode")

  ("n" (lambda ()
	 (interactive)
	 (setq navy-mode "line"
	       navy-i 'avy-goto-line-above
	       navy-k 'avy-goto-line-below
	       navy-l 'next-line
	       navy-j 'previous-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'avy-goto-word-1
	       navy-period 'avy-goto-word-or-subword-1))
   "line mode")

  ("x" (lambda ()
	 (interactive)
	 (setq navy-mode "sexp"
	       navy-j 'backward-sexp
	       navy-l 'forward-sexp
	       navy-i 'previous-line
	       navy-k 'next-line
	       navy-semicolon 'avy-goto-char-2
	       navy-quote 'avy-goto-line
	       navy-comma 'lispy-ace-symbol
	       navy-period 'lispy-ace-paren))
   "sexp mode")

  ("a" swiper-all "swiper-all")
  ("r" counsel-git-grep "git grep")
  ("t" avy-goto-char-timer "char timer"))


(defun navy ()
  "Run the `navy/body' hydra."
  (interactive)
  (setq navy-mode "char"
	navy-j 'backward-char
	navy-i 'previous-line
	navy-l 'forward-char
	navy-k 'next-line
	navy-quote 'avy-goto-line
	navy-comma 'avy-goto-char-2
	navy-period 'avy-goto-char-in-line
	navy-h 'beginning-of-visual-line
	navy-semicolon 'avy-goto-char)
  (navy/body))


(provide 'archive)

;;; archive.el ends here
