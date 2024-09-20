;;; scimax-hydra.el --- Hydras for scimax -*- lexical-binding: t; -*-

;;; Commentary:
;; The goal of this library is to emulate Spacemacs with hydras. You can access
;; a lot of the commands we use a lot with just 2-3 keystrokes. The hydras are
;; saved in a stack as you visit them so you can go to the previous one with ,
;; (comma). You can get to M-x by pressing x in any of these hydras, and / to
;; undo. Not every command will be shorter, e.g. C-a is shorter than f12 n a,
;; but this shows you tips of what you can do, and doesn't require any chording.
;;
;; This should not get in the way of any regular keybindings as you access all
;; of them through a single dispatch key (I use f12, which is remapped onto the
;; capslock key).
;;
;; At the moment this probably requires scimax, and has a number of Mac specific
;; things in it.

;; https://ericjmritz.wordpress.com/2015/10/14/some-personal-hydras-for-gnu-emacs/

(require 'cl)

(defgroup scimax-hydra nil
  "Customization for `scimax-hydra'."
  :tag "scimax-hydra")

(defcustom scimax-hydra-key "<f12>"
  "Key to bind `scimax/body' to."
  :type 'string
  :group 'scimax-hydra)

(defcustom scimax-hydra-mode-key "<M-f12>"
  "Key to bind `scimax-dispatch-mode-hydra/body' to."
  :type 'string
  :group 'scimax-hydra)

(global-set-key (kbd scimax-hydra-key) 'scimax/body)
(global-set-key (kbd scimax-hydra-mode-key) 'scimax-dispatch-mode-hydra)

;;* scimax-hydra utilities

;; Lexical closure to encapsulate the stack variable.
(lexical-let ((scimax-hydra-stack '()))
  (defun scimax-hydra-push (expr)
    "Push an EXPR onto the stack."
    (push expr scimax-hydra-stack))

  (defun scimax-hydra-pop ()
    "Pop an expression off the stack and call it."
    (interactive)
    (let ((x (pop scimax-hydra-stack)))
      (when x
	(call-interactively x))))

  (defun scimax-hydra ()
    "Show the current stack."
    (interactive)
    (with-help-window (help-buffer)
      (princ "Scimax-hydra-stack\n")
      (pp scimax-hydra-stack)))

  (defun scimax-hydra-reset ()
    "Reset the stack to empty."
    (interactive)
    (setq scimax-hydra-stack '())))

(defmacro scimax-open-hydra (hydra)
  "Push current HYDRA to a stack.
This is a macro so I don't have to quote the hydra name."
  `(progn
     (scimax-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))

(defun scimax-hydra-help ()
  "Show help buffer for current hydra."
  (interactive)
  (with-help-window (help-buffer)
    (with-current-buffer (help-buffer)
      (unless (featurep 'emacs-keybinding-command-tooltip-mode)
	(require 'emacs-keybinding-command-tooltip-mode))
      (emacs-keybinding-command-tooltip-mode +1))
    (let ((s (format "Help for %s\n" hydra-curr-body-fn)))
      (princ s)
      (princ (make-string (length s) ?-))
      (princ "\n"))

    (princ (mapconcat
	    (lambda (head)
	      (format "%s%s"
		      ;;  key
		      (s-pad-right 10 " " (car head))
		      ;; command
		      (let* ((hint (if (stringp (nth 2 head))
				       (concat " " (nth 2 head))
				     ""))
			     (cmd (cond
				   ;; quit
				   ((null (nth 1 head))
				    "")
				   ;; a symbol
				   ((symbolp (nth 1 head))
				    (format "`%s'" (nth 1 head)))
				   ((and (listp (nth 1 head))
					 (eq 'scimax-open-hydra (car (nth 1 head))))
				    (format "`%s'" (nth 1 (nth 1 head))))
				   ((listp (nth 1 head))
				    (with-temp-buffer
				      (pp (nth 1 head) (current-buffer))
				      (let ((fill-prefix (make-string 10 ? )))
					(indent-code-rigidly
					 (save-excursion
					   (goto-char (point-min))
					   (forward-line)
					   (point))
					 (point-max) 10))
				      (buffer-string)))
				   (t
				    (format "%s" (nth 1 head)))))
			     (l1 (format "%s%s" (s-pad-right 50 " " (car (split-string cmd "\n"))) hint))
			     (s (s-join "\n" (append (list l1) (cdr (split-string cmd "\n"))))))
			(s-pad-right 50 " " s))))
	    (symbol-value
	     (intern
	      (replace-regexp-in-string
	       "/body$" "/heads"
	       (symbol-name  hydra-curr-body-fn))))
	    "\n"))))

(defhydra scimax-base (:color blue)
  "base"
  ("," scimax-hydra-pop "back" :color blue)
  ("x" counsel-M-x "M-x")
  ("C-s" save-buffer "Save")
  ("/" undo-tree-undo "undo" :color red)
  ("\\" undo-tree-redo "redo" :color red)
  ("8" (switch-to-buffer "*scratch*") "*scratch*")
  ("?" scimax-hydra-help "Menu help")
  ("." scimax-dispatch-mode-hydra "Major mode hydras")
  ("u" (hydra--universal-argument current-prefix-arg) "C-u" :color red)
  ("q" nil "quit"))

;;* scimax hydra

(defhydra scimax (:color blue :inherit (scimax-base/heads)
                         :columns 4 :body-pre (scimax-hydra-reset)
                         :idle 0.5)
  "scimax"
  ("a" (scimax-open-hydra scimax-applications/body) "Applications")
  ("b" (scimax-open-hydra scimax-buffers/body) "Buffers")
  ;; c for user? compile?
  ;; d ?
  ("e" (scimax-open-hydra scimax-errors/body) "Edit/Errors")
  ("f" (scimax-open-hydra scimax-files/body) "Files")
  ("g" (scimax-open-hydra scimax-google/body) "Google")
  ("h" (scimax-open-hydra scimax-help/body) "Help")
  ("i" (scimax-open-hydra scimax-insert/body) "Insert")
  ("j" (scimax-open-hydra scimax-jump/body) "Jump")
  ("k" (scimax-open-hydra scimax-bookmarks/body) "Bookmarks")
  ("l" (scimax-open-hydra scimax-lisp/body) "Lisp")
  ("m" (scimax-open-hydra scimax-minor-modes/body) "Minor modes/mark")
  ("M" (scimax-open-hydra scimax-smerge/body) "smerge")
  ("s-m" scimax-dispatch-mode-hydra "Major mode hydras")
  ("n" (scimax-open-hydra scimax-navigation/body) "Navigation")
  ("o" (scimax-open-hydra scimax-org/body) "org")
  ("p" (scimax-open-hydra hydra-projectile/body) "Project")
  ;; q is for quit, don't reassign
  ("r" (scimax-open-hydra scimax-registers/body) "Registers/resume")
  ("s" (scimax-open-hydra scimax-search/body) "Search")
  ("t" (scimax-open-hydra scimax-text/body) "Text")
  ;; u is a prefix arg, do not reassign
  ("v" (scimax-open-hydra scimax-version-control/body) "Version control")
  ("w" (scimax-open-hydra scimax-windows/body) "Windows")
  ;; x is for M-x, don't reassign
  ("z" (scimax-open-hydra scimax-customize/body) "Customize"))


;;** applications

(defun scimax-app-hints ()
  "Calculate some variables for the applications hydra."
  (setq elfeed-count
	(s-pad-right 12 " "
		     (if (get-buffer "*elfeed-search*")
			 (format "RSS(%s)"
				 (car (s-split "/" (with-current-buffer "*elfeed-search*"
						     (elfeed-search--count-unread)))))
		       "RSS(?)"))))


(defhydra scimax-applications (:hint nil
				     :pre (scimax-app-hints)
				     :color blue
				     :inherit (scimax-base/heads))
  "applications"

  ("a" (org-db-agenda "+2d") "agenda" :column "Emacs")
  ("d" dired "dired" :column  "Emacs")
  ("j" scimax-journal/body "journal" :column "Emacs")
  ("n" nb-hydra/body "notebook" :column "Emacs")
  ("r" elfeed "elfeed" :column "Emacs")
  ("F" scimax-org-feed "scimax-org-feed" :column "Emacs")
  
  ("b" bash "bash" :column "OS")
  ("f" finder "Finder" :column "OS")
  ("e" eshell "eshell" :column "OS")

  ("c" google-calendar "Calendar" :column "Web")
  ("g" google "Google" :column "Web")
  ("o" (scimax-open-hydra scimax-office/body) "MS Office" :column "Web")
  ("G" (scimax-open-hydra scimax-gsuite/body) "GSuite" :column "Web")
  ("s" slack/body "Slack" :column "Web")
  
  ("k" package-list-packages "List packages" :column "commands")
  ("m" compose-mail "Compose mail" :column "commands"))


(defhydra scimax-office (:color blue)
  "Office"
  ("e" excel"Excel")
  ("p" powerpoint "Powerpoint")
  ("w" word "Word"))

(defhydra scimax-gsuite (:color blue)
  "GSuite"
  ("v" (browse-url "https://drive.google.com/drive/u/0/my-drive") "GDrive")
  ("d" (browse-url "https://docs.google.com/document/u/0/" "GDoc"))
  ("h" (browse-url "https://docs.google.com/spreadsheets/u/0/" "GSheet"))
  ("s" (browse-url "https://docs.google.com/presentation/u/0/" "GSlides"))
  ("j" (browse-url "https://jamboard.google.com/" "Jamboard")))

;;** buffers

(defhydra scimax-buffers (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "
buffer
Switch                  ^Kill                Split        Misc
------------------------------------------------------------------
 _a_: ace-window        _k_: kill           _2_: below   _l_: list (%(length (buffer-list)))
 _b_: switch buffer     _K_: kill others    _3_: right   _r_: rename
 _o_: other-window      _A_: kill all
 _O_: switch other win  _m_: kill matching
 _n_: next buffer       _0_: delete win
 _p_: prev buffer       _1_: delete other
 _s_: scratch           _4_: kill buf/win
 _f_: other frame       _6_: kill some
 _F_: buf in frame      _y_: bury
------------------------------------------------------------------
"
  ("0" delete-window)
  ("1" delete-other-windows)
  ("2" split-window-below)
  ("3" split-window-right)
  ("5" make-frame-command)
  ("4" kill-buffer-and-window)
  ("6" kill-some-buffers)
  ("a" ace-window :color red)
  ("b" switch-to-buffer)
  ("A" kill-all-buffers)
  ("f" other-frame :color red)
  ("F" switch-to-buffer-other-frame)
  ("k" kill-this-buffer :color red)
  ("K" kill-other-buffers)
  ("l" ibuffer)
  ("m" kill-matching-buffers :color red)
  ("n" next-buffer :color red)
  ("o" other-window :color red)
  ("O" switch-to-buffer-other-window :color red)
  ("p" previous-buffer :color red)
  ("s" (switch-to-buffer "*scratch*"))
  ("r" rename-buffer)
  ("y" bury-buffer))

;;** drag

(defhydra scimax-drag (:color red :inherit (scimax-base/heads)  :hint nil)
  ("<left>" drag-stuff-left :color red)
  ("<right>" drag-stuff-right :color red)
  ("<up>" drag-stuff-up :color red)
  ("<down>" drag-stuff-down :color red))


;;** edit/errors

(defhydra scimax-errors (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "
edit/errors
Edit                Errors
------------------------------------------------------------------
_a_: edit abbrevs   _n_: next error
_c_: copy (dwim)    _p_: prev error
_k_: kill (dwim)
_v_: paste
_V_: paste ring
------------------------------------------------------------------
"
  ("a" edit-abbrevs)
  ("c" scimax-copy-dwim)
  ("v" yank)
  ("V" counsel-yank-pop)
  ("k" scimax-kill-dwim)
  ("n" next-error :color red)
  ("p" previous-error :color red))


;;** files

(defhydra scimax-files (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "
files
------------------------------------------------------------------
_f_: find file     _R_: rename  _r_: recentf
_4_: other window  _k_: close   _l_: locate
_5_: other frame   _d_: dired
_p_: ffap
------------------------------------------------------------------"
  ("4" find-file-other-window)
  ("5" find-file-other-frame)
  ("b" describe-file)
  ("d" (dired default-directory))
  ("f" find-file)
  ("k" kill-this-buffer)
  ("l" counsel-locate)
  ("p" ffap)
  ("r" counsel-recentf)
  ("R" write-file))


;;** google
(defhydra scimax-google (:color blue :inherit (scimax-base/heads) :columns 3)
  "google"
  ("e" google-this-error "Error")
  ("f" google-this-forecast "Forecast")
  ("g" google-this-region "Region")
  ("k" google-this-lucky-search "Lucky")
  ("l" google-this-line "Line")
  ("m" google-maps "Maps")
  ("r" google-this-ray "Ray")
  ("s" google-this-search "Search")
  ("t" google-this "This")
  ("w" google-this-word "Word")
  ("y" google-this-symbol "Symbol"))

;;** help

(defhydra scimax-help (:color blue :inherit (scimax-base/heads) :columns 3)
  "help"
  ("a" apropos "Apropos" :column "Code")
  ("c" describe-command "Command" :column "Code")
  ("f" describe-function "Function" :column "Code")
  ("v" describe-variable "Variable" :column "Code")
  ("g" view-echo-area-messages "Messages" :column "Code")

  ("o" ore "Org explorer" :column "Point")
  ("t" describe-text-properties "Text properties" :column "Point")
  ("s" describe-syntax "Syntax" :column "Point")

  ("h" describe-theme "Theme" :column "Thing")
  ("k" describe-key "Key" :column "Thing")
  ("K" describe-keymap "Keymap" :column "Thing")
  ("m" describe-mode "Mode" :column "Thing")
  ("p" describe-package "Package" :column "Thing")

  ("S" scimax-help "Scimax help" :column "Documentation")
  ("e" info-emacs-manual "Emacs manual" :column "Documentation")
  ("T" help-with-tutorial "Emacs tutorial" :column "Documentation")
  ("i" info "Info" :column "Documentation")
  ("w" woman "Unix manual pages" :column "Documentation"))


;;** insert

(defhydra scimax-insert (:color blue :inherit (scimax-base/heads) :columns 3)
  "insert stuff"
  ("b" insert-buffer "Buffer")
  ("c" org-db-contacts "Contact")
  ("e" ivy-insert-org-entity "Org-entity")
  ("f" insert-file "File contents")
  ("k" org-inlinetask-insert-task "org task")
  ;; ("l" ) a link function...
  ("L" lorem-ipsum-insert-paragraphs "Lorem ipsum" :color red)
  ("u" insert-char "Unicode char")
  ("p" insert-parentheses "Parentheses")
  ("r" insert-register "Register")
  ("ss" screenshot "screenshot")
  ("sp" pngpaste  "Paste image")
  ("st" tesseract "screenshot with OCR")
  ("t" org-time-stamp-inactive "Inactive [timestamp]")
  ("T" org-time-stamp "Active <timestamp>")
  ("y" ivy-yasnippet "yasnippet"))

;;** jump

(defhydra scimax-jump (:color blue :inherit (scimax-base/heads) :columns 3)
  "jump"
  ("<" beginning-of-buffer "Beginning of buffer" :column "line")
  (">" end-of-buffer "End of buffer" :column "line")

  ("a" beginning-of-line "Beginning of line" :column "line")
  ("e" end-of-line "End of line" :column "line")
  ("g" goto-line "Goto line" :column "line")
  ("l" (scimax-open-hydra scimax-jump-line/body) "Line" :column "line")

  ("c" (scimax-open-hydra scimax-jump-char/body) "Char" :column "text")
  ("w" (scimax-open-hydra scimax-jump-word/body) "Word" :column "text")
  ("s" avy-jump-to-sentence "Sentence" :column "text")
  ("r" avy-jump-to-paragraph "Paragraph" :column "text")
  ("y" (scimax-open-hydra scimax-jump-symbol/body) "Symbol" :column "text")

  
  ("h" org-db-headings "org-db-heading" :column "org")
  ("d" org-db/body "org-db hydra" :column "org")
  ("k" ace-link "Link"  :column "org")
  ("o" (scimax-open-hydra scimax-jump-org/body) "Org"  :column "org")
  ("3" scimax-ob-jump-to-src-block "src block" :column "org")

  ("pp" counsel-projectile-switch-project  "project" :column "Project")
  ("pb" counsel-projectile-switch-to-buffer  "buffer" :column "Project")
  ("pf" projectile-find-file "file" :column "Project")
  ("ph" ivy-org-jump-to-project-headline "headline" :column "Project")
  
  
  ("b" counsel-ibuffer "Buffer" :column "misc")
  ("n" ace-window "Ace window" :column "misc")
  ("f" counsel-recentf "Recent file" :column "misc")
  ("j" avy-goto-char-timer "avy timer" :column "misc")
  )


(defhydra scimax-jump-char (:color blue :inherit (scimax-base/heads) :columns 3)
  "char"
  ("c" avy-goto-char "Char")
  ("l" avy-goto-char-in-line "In line")
  ("t" avy-goto-char-timer "Timer")
  ("2" avy-goto-char-2 "Char2")
  ("a" avy-goto-char-2-above "Above")
  ("b" avy-goto-char-2-below "Below"))


(defhydra scimax-jump-line (:color blue :inherit (scimax-base/heads) :columns 3)
  "line"
  ("l" avy-goto-line "Line")
  ("a" avy-goto-line-above "Above")
  ("b" avy-goto-line-below "Below"))


(defhydra scimax-jump-org (:color blue :inherit (scimax-base/heads) :columns 3)
  "org"
  ("v" ivy-org-jump-to-visible-headline "Visible heading" :column "heading")
  ("h" ivy-org-jump-to-heading "Heading" :column "heading")
  ("o" ivy-org-jump-to-open-headline "Open heading" :column "heading")
  ("d" ivy-org-jump-to-heading-in-directory "Directory heading" :column "heading")
  ("p" ivy-org-jump-to-project-headline "Project heading" :column "heading")
  ("a" ivy-org-jump-to-agenda-heading "Agenda heading" :column "heading")
  
  ("bb" scimax-ob-jump-to-src-block "Jump to src-block" :column "block")
  ("bv" scimax-ob-jump-to-visible-src-block "Jump to visible src-block" :column "block")
  ("bi" scimax-ob-jump-to-inline-src "Jump to inline src" :column "block")

  ("k" ace-link "link" :column "misc"))


(defhydra scimax-jump-word (:color blue :inherit (scimax-base/heads) :columns 3)
  "word"
  ("w" avy-goto-word-1 "word1")
  ("l" avy-jump-to-word-in-line "in line")
  ("0" avy-goto-word-0 "word0")
  ("a" avy-goto-word-0-above "above-0")
  ("A" avy-goto-word-1-above "above-1")
  ("b" avy-goto-word-0-below "below0")
  ("B" avy-goto-word-1-below "below1")
  ("o" avy-goto-word-or-subword-1 "word or subword")
  ("s" avy-goto-subword-0 "subword-0")
  ("S" avy-goto-subword-1 "subword-1"))


(defhydra scimax-jump-symbol (:color blue :inherit (scimax-base/heads) :columns 3)
  "symbol"
  ("y" avy-goto-symbol-1 "symbol")
  ("a" avy-goto-symbol-1-above "Above")
  ("b" avy-goto-symbol-1-below "below"))

;;** bookmarks

(when (eq 'darwin system-type)
  ;; this is in org-contrib, but I actually keep a copy of my own. It is not
  ;; part of scimax though.
  (require 'org-mac-link))

(defun scimax-bookmark-chrome (nickname)
  "Save the url currently open as a bookmark."
  (interactive (list (bmkp-completing-read-lax "Nickname: ")))
  (bmkp-url-target-set (car (split-string (org-as-mac-chrome-get-frontmost-url) "::"))
		       nil nickname))

(defhydra scimax-bookmarks (:color blue :inherit (scimax-base/heads) :columns 3)
  "bookmarks"
  ("k" bookmark-jump "jump")
  ("l" bookmark-bmenu-list "list")
  ("sc" scimax-bookmark-chrome "save chrome url")
  ("su" bmkp-url-target-set "save url")
  ("n" bookmark-set "new"))

;;** lisp

(defhydra scimax-lisp (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "lisp"
  ("a" eval-buffer "eval buffer")
  ("c" byte-recompile-file "byte-compile file")
  ("d" (eval-defun t) "debug defun")
  ("z" (eval-defun nil) "stop edebug")
  ("e" eval-defun "eval defun")
  ("v" eval-last-sexp "eval last sexp")
  ("g" (eval-region (point-min) (point)) "eval region")
  ("h" (describe-function 'lispy-mode) "lispy help")
  ("i" ielm "ielm")
  ("l" load-file "load file")
  ("L" counsel-load-library "load library")
  ("f" counsel-find-library "find library")
  ("r" eval-region "region")
  ("t" toggle-debug-on-error "toggle debug")
  ("y" edebug-on-entry "debug on entry"))

;;** mark/minor modes

(defhydra scimax-minor-modes (:color blue :inherit (scimax-base/heads) :columns 3 :hint nil)
  "
minor modes and marks
Marks                     minor-modes
------------------------------------------------------------------
_w_: mark word            _i_: aggressive indent
_n_: mark sentence        _b_: org-bullets
_p_: mark paragraph       _k_: emacs-keybindings
_g_: mark page            _l_: nlinum
_s_: mark sexp            _r_: rainbow
_d_: mark defun           _on_: org-numbered-headings
_a_: mark buffer
_e_: mark org-element
_m_: set mark
_j_: jump to mark
------------------------------------------------------------------
"
  ("i" aggressive-indent-mode)
  ("b" org-bullets-mode)
  ("k" emacs-keybinding-command-tooltip-mode)
  ("l" nlinum-mode)
  ("r" rainbow-mode)

  ("a" mark-whole-buffer)
  ("d" mark-defun)
  ("e" org-mark-element)
  ("g" mark-page)
  ("j" pop-to-mark-command)
  ("m" set-mark-command)
  ("n" mark-end-of-sentence)
  ("on" scimax-numbered-org-mode)
  ("p" mark-paragraph)
  ("s" mark-sexp)
  ("w" mark-word))


;;** navigation

(defvar scimax-hydra-modes (make-ring 4)
  "Holds list of navigation modes.")

(ring-insert scimax-hydra-modes 'scimax-nav-paragraph/body)
(ring-insert scimax-hydra-modes 'scimax-nav-sentence/body)
(ring-insert scimax-hydra-modes 'scimax-nav-word/body)
(ring-insert scimax-hydra-modes 'scimax-navigation/body)


(defvar scimax-hydra-mode-counter 0
  "Integer counter for current mode.")


(defun scimax-hydra-cycle-navigation-mode (&optional arg)
  (interactive "P")
  (if arg
      (cl-decf scimax-hydra-mode-counter)
    (cl-incf scimax-hydra-mode-counter))
  (eval `(scimax-open-hydra ,(ring-ref scimax-hydra-modes scimax-hydra-mode-counter))))


(defhydra scimax-navigation (:color red :inherit (scimax-base/heads)
				    :columns 4 :hint nil
				    :pre (setq scimax-hydra-mode-counter 0))
  "
navigation
-----------------------------------------------------------------------------------
_j_: ← _k_: ↑ _l_: ↓ _;_: →  _i_: imenu
_a_: beginning of line _e_: end of line _<_: beginning of buffer _>_: end of buffer

_H-w_: beginning of word _H-s_: beginning of sentence _H-p_: beginning of paragraph
_s-w_: end of word _s-s_: end of sentence _s-p_: end of paragraph
_z_: jump

_f_: delete forward _d_: delete backward
_t_: transpose chars
_<tab>_: %(ring-ref scimax-hydra-modes (+ 1 scimax-hydra-mode-counter)) _S-<tab>_: %(ring-ref scimax-hydra-modes (- scimax-hydra-mode-counter 1))
-----------------------------------------------------------------------------------
"
  ("j" backward-char)
  (";" forward-char)
  ("k" previous-line)
  ("l" next-line)
  ("i" counsel-imenu)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("f" delete-char :color red)
  ("d" backward-delete-char :color red)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("t" transpose-chars)
  ("z" (scimax-open-hydra scimax-jump/body) :color blue)
  ("H-w" backward-word)
  ("H-s" backward-sentence)
  ("H-p" backaward-paragraph)
  ("s-p" forward-paragraph)
  ("s-w" forward-word)
  ("s-s" forward-sentence)
  ("<tab>" scimax-hydra-cycle-navigation-mode :color blue)
  ("S-<tab>" (scimax-hydra-cycle-navigation-mode t) :color blue))



(defhydra scimax-nav-word (:color red :inherit (scimax-base/heads)
				  :columns 4 :hint nil
				  :pre (setq scimax-hydra-mode-counter 1))
  "
word navigation
----------------------------
_j_: ← _k_: ↑ _l_: ↓ _;_: →
_a_: beginning of line _e_: end of line _i_: imenu
_f_: kill forward _d_: kill backward _m_: Mark word
_t_: transpose words
_z_: jump

_<tab>_: %(ring-ref scimax-hydra-modes (+ 1 scimax-hydra-mode-counter)) _S-<tab>_: %(ring-ref scimax-hydra-modes (- scimax-hydra-mode-counter 1))
------------------------------------------------------------------"
  ("j" backward-word)
  (";" forward-word)
  ("k" previous-line)
  ("l" next-line)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("i" counsel-imenu)
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("f" (kill-word 1))
  ("d" backward-kill-word)
  ("t" transpose-words)
  ("z" (scimax-open-hydra scimax-jump/body) :color blue)

  ("m" mark-word)
  ("<tab>" (scimax-hydra-cycle-navigation-mode) :color blue)
  ("S-<tab>" (scimax-hydra-cycle-navigation-mode t) :color blue))


(defhydra scimax-nav-sentence (:color red :inherit (scimax-base/heads) :columns 4 :hint nil
				      :pre (setq scimax-hydra-mode-counter 2))
  "
sentence
_j_: ← _k_: ↑ _l_: ↓ _;_: →
_f_: kill forward _d_: kill backward
_t_: transpose sentences
_z_: jump

_<tab>_: %(ring-ref scimax-hydra-modes (+ 1 scimax-hydra-mode-counter)) _S-<tab>_: %(ring-ref scimax-hydra-modes (- scimax-hydra-mode-counter 1))
------------------------------------------------------------------"
  ("j" backward-sentence)
  (";" forward-sentence)
  ("k" previous-line)
  ("l" next-line)
  ("d" (kill-sentence -1))
  ("f" kill-sentence)
  ("t" transpose-sentences)
  ("m" (unless (sentence-beginning-p)
	 (backward-sentence)
	 (set-mark (point))
	 (forward-sentence)))
  ("z" (scimax-open-hydra scimax-jump/body) :color blue)
  ("<tab>" scimax-hydra-cycle-navigation-mode :color blue)
  ("S-<tab>" (scimax-hydra-cycle-navigation-mode t) :color blue))


(defhydra scimax-nav-paragraph (:color red :inherit (scimax-base/heads) :columns 4 :hint nil
				       :pre (setq scimax-hydra-mode-counter 3))
  "
paragraph
_j_: ← _k_: ↑ _l_: ↓ _;_: →
_f_: kill forward _d_: kill backward
_t_: transpose paragraphs  _m_: mark paragraph
_z_: jump

_<tab>_: %(ring-ref scimax-hydra-modes (+ 1 scimax-hydra-mode-counter)) _S-<tab>_: %(ring-ref scimax-hydra-modes (- scimax-hydra-mode-counter 1))
------------------------------------------------------------------"
  ("j" backward-paragraph)
  (";" forward-paragraph)
  ("k" previous-line)
  ("l" next-line)
  ("d" (kill-paragraph -1))
  ("f" (kill-paragraph nil))
  ("t" transpose-paragraphs)
  ("m" mark-paragraph)
  ("z" (scimax-open-hydra scimax-jump/body) :color blue)
  ("<tab>" scimax-hydra-cycle-navigation-mode :color blue)
  ("S-<tab>" (scimax-hydra-cycle-navigation-mode t) :color blue))

;;** org

(defhydra scimax-org (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-mode"
  ("'" org-edit-special "edit")
  ("a" org-agenda "agenda")
  ("b" (scimax-open-hydra scimax-org-block/body) "block hydra")
  ("cc" org-ctrl-c-ctrl-c "C-c C-c")
  ("sl" scimax-store-link "store link")
  ("il" org-insert-link "insert link")
  ("d" (scimax-open-hydra scimax-org-db/body) "org-db hydra")
  ("e" org-export-dispatch "Export")
  ("E" (scimax-open-hydra hydra-ox/body) "export hydra")
  ("g" org-babel-tangle "tangle")
  ("h" ivy-org-jump-to-heading "jump to heading")
  ("I" org-clock-in "clock in")
  ("O" org-clock-out "clock out")
  ("n" outline-next-heading "next heading" :color red)
  ("p" outline-previous-heading "previous heading" :color red)
  ("<tab>" org-cycle "cycle" :color red)
  ("r" (scimax-open-hydra scimax-org-ref/body) "org-ref")
  ("t" (scimax-open-hydra scimax-org-toggle/body) "toggle"))


(defhydra scimax-org-block (:color red :inherit (scimax-base/heads) :columns 3)
  "org blocks"
  ("r" org-babel-remove-result "clear result")
  ("<return>" org-babel-execute-src-block "execute")
  ("S-<return>" scimax-ob-execute-and-next-block "execute and next")
  ("M-<return>" (scimax-ob-execute-and-next-block t) "execute and new")
  ("n" org-next-block "next block")
  ("p" org-previous-block "previous block")
  ("-" scimax-ob-split-src-block "split block")
  ("k" scimax-ob-kill-block-and-results "kill")
  ("w" scimax-ob-copy-block-and-results "copy")
  ("y" yank "paste")
  ("c" scimax-ob-clone-block "clone")
  ("h" scimax-ob-edit-header "edit header")
  ("v" scimax-jump-to-visible-block "jump to visible")
  ("j" scimax-jump-to-block "jump to block")
  ("S-<up>" scimax-ob-move-src-block-up "move up")
  ("S-<down>" scimax-ob-move-src-block-down "move down"))


(defun scimax-installed-latex-packages ()
  (if (get 'scimax-installed-latex-packages 'packages)
      (get 'scimax-installed-latex-packages 'packages)
    (when
	(executable-find "tlmgr")
      (require 'async)
      (async-start
       `(lambda ()
	  (require 'cl-lib)
	  (mapcar
	   (lambda (s)
	     (second (split-string (first (split-string s ":")) " ")))
	   (cl-loop for line in (process-lines ,(executable-find "tlmgr")
					       "info" "--only-installed")
		    if (and (stringp line) (string= "i" (substring line 0 1)))
		    collect line)))

       (lambda (result)
	 (put 'scimax-installed-latex-packages 'packages result))))
    '("Getting packages. Try later")))


(defun scimax-insert-latex-package ()
  (interactive)
  (insert (format "#+latex_header: \\usepackage{%s}"
		  (completing-read "Package: " (scimax-installed-latex-packages)))))


(defhydra scimax-org-ref (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-ref"
  ("b" org-ref-insert-bibliography-link "bibliography")
  ("s" org-ref-insert-bibliographystyle-link "bibliographystyle")
  ("p" scimax-insert-latex-package "package")
  ("c" org-ref-insert-link "cite")
  ("l" (org-ref-insert-link '(16)) "label")
  ("r" (org-ref-insert-link '(4)) "ref")
  ("o" org-ref "org-ref"))


(defhydra scimax-org-db (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-db"
  ("c" org-db-contacts "contact")
  ("h" org-db-headings "heading")
  ("f" org-db-files "file")
  ("l" org-db-locations "location")
  ("k" org-db-links "link")
  ("r" org-db-recent-files "recent file")
  ("t" org-db-hashtags "hashtag")
  ("2" org-db-@ "@-link"))


(defhydra scimax-org-toggle (:color blue :inherit (scimax-base/heads) :columns 3)
  "toggle"
  ("e" org-toggle-pretty-entities "pretty entities")
  ("i" org-toggle-inline-images "images")
  ("l" org-latex-preview "latex"))

;; *** export

;; adapted from https://github.com/abo-abo/hydra/blob/master/hydra-ox.el

(defhydradio hydra-ox ()
  (body-only "Export only the body.")
  (export-scope "Export scope." [buffer subtree])
  (async-export "When non-nil, export async.")
  (visible-only "When non-nil, export visible only")
  (force-publishing "Toggle force publishing"))


(defhydra hydra-ox-html (:color blue)
  "ox-html"
  ("H" (org-html-export-as-html
        hydra-ox/async-export
        (eq hydra-ox/export-scope 'subtree)
        hydra-ox/visible-only
        hydra-ox/body-only)
   "As HTML buffer")
  ("h" (org-html-export-to-html
        hydra-ox/async-export
        (eq hydra-ox/export-scope 'subtree)
        hydra-ox/visible-only
        hydra-ox/body-only) "As HTML file")
  ("o" (org-open-file
        (org-html-export-to-html
         hydra-ox/async-export
         (eq hydra-ox/export-scope 'subtree)
         hydra-ox/visible-only
         hydra-ox/body-only)) "As HTML file and open")
  ("q" nil "quit"))


(defhydra hydra-ox-latex (:color blue)
  "ox-latex"
  ("L" org-latex-export-as-latex "As LaTeX buffer")
  ("l" org-latex-export-to-latex "As LaTeX file")
  ("p" org-latex-export-to-pdf "As PDF file")
  ("o" (org-open-file (org-latex-export-to-pdf)) "As PDF file and open")
  ("b" hydra-ox/body "back")
  ("q" nil "quit"))


(defhydra hydra-ox ()
  "
_C-b_ Body only:    % -15`hydra-ox/body-only^^^ _C-v_ Visible only:     %`hydra-ox/visible-only
_C-s_ Export scope: % -15`hydra-ox/export-scope _C-f_ Force publishing: %`hydra-ox/force-publishing
_C-a_ Async export: %`hydra-ox/async-export
"
  ("C-b" (hydra-ox/body-only) nil)
  ("C-v" (hydra-ox/visible-only) nil)
  ("C-s" (hydra-ox/export-scope) nil)
  ("C-f" (hydra-ox/force-publishing) nil)
  ("C-a" (hydra-ox/async-export) nil)
  ("h" hydra-ox-html/body "Export to HTML" :exit t)
  ("l" hydra-ox-latex/body "Export to LaTeX" :exit t)
  ("n" ox-ipynb-export-to-ipynb-file-and-open "Jupyter" :exit t)
  ("q" nil "quit"))


;;** project
;; See also https://github.com/abo-abo/hydra/wiki/Projectile
(defhydra hydra-projectile (:color teal :hint nil)
  "
     PROJECTILE: %(projectile-project-root)
"

  ("A" projectile-add-known-project "Add project" :column "projects")
  ("C" projectile-configure-project "Configure" :column "build")
  ("D" projectile-dired "Dired" :column "Files")
  ("E" projectile-edit-dir-locals "dir-locals.el" :column "projects")
  ("F" projectile-find-file-in-known-projects "Find in projects" :column "Files")
  ("I" projectile-ibuffer "ibuffer" :column "buffers")
  ("P" projectile-test-project "Run tests" :column "build")
  ("R" projectile-regenerate-tags "Rebuild tags" :column "search")
  ("S" projectile-save-project-buffers "Save buffers" :column "buffers")
  ("T" projectile-find-test-file)
  ("V" projectile-browse-dirty-projects "Dirty projects" :column "projects")
  ("X" projectile-remove-known-project "Remove project" :column "projects")

  ("a" projectile-find-other-file "Find other file" :column "Files")
  ("b" projectile-switch-to-buffer "Switch buffer" :column "buffers")
  ("c" projectile-compile-project "Compile" :column "build")
  ("d" projectile-find-dir "Find dir" :column "Files")
  ("e" projectile-recentf "Recentf" :column "Files")
  ("f" projectile-find-file "Find file" :column "Files")
  ("g" projectile-find-file-dwim "Find dwim" :column "Files")
  ("i" projectile-invalidate-cache "invalidate cache" :column "projects")
  ("j" projectile-find-tag "find tag" :column "search")
  ("k" projectile-kill-buffers "Kill buffers" :column "buffers")
  ("l" projectile-find-file-in-directory "Find in dir" :column "Files")
  ("m" projectile-multi-occur "moccur" :column "search")


  ("p" projectile-switch-project "Switch" :column "projects")
  ;; ("q" projectile-switch-open-project "Switch to open" :column "projects")
  ("q" nil "quit")
  ("r" projectile-replace "Replace" :column "search")

  ("t" projectile-toggle-between-implementation-and-test)
  ("u" projectile-run-project "Run" :column "build")
  ("v" projectile-vc "vc" :column "build")

  ("z" projectile-cache-current-file "Cache file" :column "projects")

  ("<return>" (cl-loop for readme in '("readme.org"
				       "README.org")
		       when (file-exists-p readme)
		       do
		       (find-file readme)
		       (cl-return)) "Open readme?")

  ("x1" projectile-run-shell-command-in-root "Run cmd" :column "cmd")
  ("x7" projectile-run-async-shell-command-in-root "Run async cmd" :column "cmd")
  ("xe" projectile-run-eshell "eshell" :column "cmd")
  ("xi" projectile-run-ielm "ielm" :column "cmd")
  ("xs" projectile-run-shell "shell" :column "cmd")
  ("xt" projectile-run-term "term" :column "cmd")

  ("sg" projectile-grep "Grep" :column "search")
  ("sr" projectile-ripgrep "Ripgrep" :column "search")
  ("ss" projectile-ag "ag" :column "search")
  ("so" ivy-org-jump-to-project-headline "org heading" :column "search")
  ("M-." xref-find-definitions "xref find" :column "search")
  ("M-," xref-pop-marker-stack "xref pop" :column "search")
  ("M-/" xref-find-apropos "xref apropos" :column "search"))


;;** registers/resume/replace

(defhydra scimax-registers (:color blue :inherit (scimax-base/heads) :columns 3)
  "
register/resume/replace
Register                     Resume             Replace
------------------------------------------------------------------
_j_: jump to register        _v_: ivy resume    _q_: query replace
_i_: insert register         ^ ^                _x_: regexp replace
_c_: copy to register
_a_: append to register
_n_: number to register
_p_: point to register
_w_: window conf to register
_f_: frameset to register
_l_: list registers
------------------------------------------------------------------"
  ("a" append-to-register)
  ("c" copy-to-register)
  ("f" frameset-to-register)
  ("i" insert-register)
  ("j" jump-to-register)
  ("l" list-registers)
  ("n" number-to-register)
  ("p" point-to-register)
  ("q" query-replace)
  ("v" ivy-resume)
  ("w" window-configuration-to-register)
  ("x" query-replace-regexp))


;;** search

(defhydra scimax-search (:color blue :inherit (scimax-base/heads) :columns 3)
  "search"
  ("a" swiper-all "swiper-all")
  ("cg" counsel-ag "counsel ag")
  ("g" counsel-git-grep "grep")
  ("m" multi-moccur "moccur")
  ("o" occur "occur")
  ("pa" projectile-ag "project ag")
  ("pg" projectile-grep "project grep")
  ("pr" projectile-ripgrep "project-ripgrep")
  ("po" projectile-multi-occur "project multi-occur")
  ("r" isearch-backward "search back")
  ("s" counsel-grep-or-swiper "grep-or-swiper")
  ("t" counsel-pt "pt"))


;;** text

(defhydra scimax-text (:color blue :inherit (scimax-base/heads) :columns 3)
  "text"
  ("A" (mark-whole-buffer) "Select all")
  ("c" kill-ring-save "Copy")
  ("C" capitalize-dwim "Capitalize" :color red)
  ("d" downcase-dwim "Downcase" :color red)
  ("k" kill-region "Cut")
  ("l" kill-whole-line "Kill line" :color red)
  ("m" set-mark-command "Set mark" :color red)
  ("n" (scimax-open-hydra scimax-narrow/body) "narrow")
  ("s" (scimax-open-hydra scimax-spellcheck/body) "spell-check")
  ("S" sentence-case-region "Sentence case" :color red)
  ("t" (scimax-open-hydra scimax-transpose/body) "transpose")
  ("u" upcase-dwim "Upcase" :color red)
  ("v" yank "paste")
  ("w" count-words "count words")
  ("y" counsel-yank-pop "yank ring")
  (";" comment-dwim "comment")
  (":" uncomment-region "uncomment")
  ("b" comment-box "comment-box"))


(defhydra scimax-kill (:color blue :inherit (scimax-base/heads) :columns 3)
  "kill"
  ("c" kill-comment "comment")
  ("d" scimax-kill-dwim "kill dwim")
  ("l" kill-whole-line "line")
  ("p" kill-paragraph "paragraph")
  ("r" kill-region "region")
  ("s" kill-sentence "sentence")
  ("v" kill-visual-line "visual line")
  ("w" kill-word "word")
  ("x" kill-sexp "sexp"))


(defhydra scimax-narrow (:color blue :inherit (scimax-base/heads) :columns 3)
  "narrow"
  ("b" org-narrow-to-block "org-block")
  ("d" narrow-to-defun "defun")
  ("e" org-narrow-to-element "org element")
  ("p" narrow-to-page)
  ("r" narrow-to-region "region")
  ("s" org-narrow-to-subtree "org subtree")
  ("w" widen "widen"))


(defhydra scimax-spellcheck (:color red :inherit (scimax-base/heads) :columns 3)
  "spell"
  ("b" flyspell-buffer "buffer")
  ("p" flyspell-correct-previous "correct previous")
  ("n" flyspell-correct-next "correct next")
  ("[" scimax-flyspell-goto-prev-error  "prev typo")
  ("]" flyspell-goto-next-error "next typo")
  ("w" flyspell-correct-wrapper "correct word")
  ("a" scimax-ivy-jump-to-typo "Jump to visible typo"))


(defhydra scimax-transpose (:color red :inherit (scimax-base/heads) :columns 3)
  "transpose"
  ("c" transpose-chars "chars")
  ("l" transpose-lines "lines")
  ("p" transpose-paragraphs "paragraphs")
  ("s" transpose-sentences "sentences")
  ("x" transpose-sexps "sexps")
  ("w" transpose-words "words"))


;;** version control

(defhydra scimax-version-control (:color blue :inherit (scimax-base/heads) :columns 4)
  "vc"
  ("b" magit-branch-popup "Branch")
  ("c" magit-commit-popup "Commit")
  ("d" magit-diff-popup "Diff")
  ("f" magit-fetch-popup "Fetch")
  ("k" magit-checkout "Checkout")
  ("l" magit-log-all "Log")
  ("n" vc-next-action "Next action")
  ("p" magit-push-popup "Push")
  ("r" magit-revert-popup "Revert")
  ("s" magit-stage "Stage")
  ("u" magit-pull-popup "Pull")
  ("v" magit-status "Magit"))


;;** windows

(defhydra scimax-windows (:color blue :inherit (scimax-base/heads) :columns 4 :hint nil)
  "
Windows:
Switch              Delete                Split                 Size
----------------------------------------------------------------------
_a_: ace-window     _do_: delete window   _sb_: split below    _[_: enlarge horizontally
_ow_: other window  _do_: delete others   _sr_: split right    _]_: shrink horizontally
_of_: other frame   _y_: bury buffer      ^ ^                  _=_: enlarge
_b_: buffers        _df_: delete frame    ^ ^                  _-_: shrink
------------------------------------------------------------------
"
  ("a" ace-window)
  ("dw" delete-window)
  ("b" (scimax-open-hydra scimax-buffers/body))
  ("do" delete-other-windows)
  ("sb" split-window-below)
  ("sr" split-window-right)
  ("ow" other-window)
  ("of" other-frame)
  ("df" delete-frame)
  ("y" bury-buffer)
  ("[" (shrink-window-horizontally 1) :color red)
  ("]" (enlarge-window-horizontally 1) :color red)
  ("=" (enlarge-window 1) :color red)
  ("-" (enlarge-window -1) :color red))


;;** Customize

(defhydra scimax-customize (:color blue :inherit (scimax-base/heads) :hint nil :columns 3)
  "
Customize               Font
--------------------------------------------------------------
_t_: Theme              _+_: increase size
_c_: Customize Emacs    _-_: decrease size
_u_: Scimax user.el     _0_: reset size
_z_: Customize scimax   _f_: change font
---------------------------------------------------------------
"
  ("+" text-scale-increase :color red)
  ("-" text-scale-decrease :color red)
  ("0" (text-scale-adjust 0))
  ("c" customize)
  ("f" (ivy-read "Font: " (x-list-fonts "*")
		 :action
		 (lambda (font)
		   (set-frame-font font 'keep-size t))))
  ("t" load-theme)
  ("u" scimax-customize-user)
  ("z" (customize-apropos "scimax")))

;;** smerge
;; https://emacs.stackexchange.com/questions/16469/how-to-merge-git-conflicts-in-emacs

(defhydra scimax-smerge (:color red :inherit (scimax-base/heads) :hint nil)
  "
Navigate       Keep               other
----------------------------------------
_p_: previous  _c_: current       _e_: ediff
_n_: next      _m_: mine  <<      _u_: undo
_j_: up        _o_: other >>      _r_: refine
_k_: down      _a_: combine
               _b_: base
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("c" smerge-keep-current)
  ("m" smerge-keep-upper)
  ("o" smerge-keep-other)
  ("b" smerge-keep-base)
  ("a" smerge-keep-all)
  ("e" smerge-ediff)
  ("j" previous-line)
  ("k" forward-line)
  ("r" smerge-refine)
  ("u" undo))

(defun my-enable-smerge-maybe ()
  (when (and buffer-file-name (vc-backend buffer-file-name))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "^<<<<<<< " nil t)
        (smerge-mode +1)
	(scimax-smerge/body)))))

(add-hook 'find-file-hook #'my-enable-smerge-maybe)
;; (add-hook 'buffer-list-update-hook #'my-enable-smerge-maybe)



;;* Mode specific hydras
(defun sentence-beginning-p ()
  "Determine if point is at the beginning of a sentence.
The idea is to move forward a sentence, then back.  If the point
doesn't move, it means you were at the beginning of a sentence."
  (let ((cp (point)))
    (save-excursion
      (forward-sentence)
      (backward-sentence)
      (= cp (point)))))


(defun paragraph-beginning-p ()
  "Determine if point is at the beginning of a paragraph.
The idea is to move forward a paragraph, then back.  If the point
doesn't move, it means you were at the beginning of a paragraph."
  (let ((cp (point)))
    (save-excursion
      (forward-paragraph)
      (backward-paragraph)
      (= cp (point)))))


(defun scimax-dispatch-mode-hydra ()
  "Context sensitive dispatcher."
  (interactive)
  (pcase major-mode
    ('emacs-lisp-mode (scimax-open-hydra scimax-lisp/body))
    ('elfeed-search-mode (scimax-open-hydra scimax-elfeed/body))
    ('mu4e-headers-mode (scimax-open-hydra scimax-mu4e/body))
    ('bibtex-mode (scimax-open-hydra org-ref-bibtex-hydra/body))
    ('dired-mode (scimax-open-hydra scimax-dired/body))
    ('python-mode (scimax-open-hydra scimax-python-mode/body))
    ('org-mode (let ((el (org-element-context)))
		 (cond
		  ((eql (car el) 'src-block)
		   (cond
		    ((string= "ipython" (car (org-babel-get-src-block-info)))
		     (scimax-obi/body))
		    (t
		     (scimax-open-hydra scimax-src-block-hydra/body))))

		  ((memq (car el) '(table table-row table-cell))
		   (scimax-open-hydra scimax-org-table/body))

		  ((eql (car el) 'headline)
		   (scimax-open-hydra scimax-org-headline/body))

		  ((memq (car el) '(latex-fragment latex-environment))
		   (org-latex-preview))

		  ;; match most specific first then more general
		  ((and (eql (car el) 'link)
			(string= "cite" (or (org-element-property :type el) "")))
		   (scimax-open-hydra scimax-org-ref-cite-hydra/body))

		  ((eql (car el) 'link)
		   (scimax-open-hydra scimax-link-hydra/body))

		  ((org-in-item-p)
		   (scimax-open-hydra scimax-item-hydra/body))

		  ((and (eql (car el) 'paragraph)
			(paragraph-beginning-p))
		   (scimax-open-hydra scimax-nav-paragraph/body))

		  ((and (eql (car el) 'paragraph)
			(sentence-beginning-p))
		   (scimax-open-hydra scimax-nav-sentence/body))

		  ((and (eql (car el) 'paragraph)
			(thing-at-point 'word))
		   (scimax-open-hydra scimax-words/body))

		  (t
		   (scimax-open-hydra scimax-org/body)))))
    (_ (message "no hydra found for this context"))))


;; ** major mode hydras
(defhydra scimax-words (:color blue :hint nil :inherit (scimax-base/heads))
  "
word helper
_b_old         _c_apitalize  _n_: scimax-nav-word
_i_talic       _u_pcase
_v_erbatim     _d_owncase
___: underline
_+_: strike
"
  ("c" capitalize-word)
  ("u" upcase-word)
  ("d" downcase-word)
  ("t" transpose-words)
  ("b" org-bold-region-or-point)
  ("i" org-italics-region-or-point)
  ("v" org-verbatim-region-or-point)
  ("+" org-strikethrough-region-or-point)
  ("_" org-underline-region-or-point)
  ("n" scimax-nav-word/body))

(defhydra scimax-dired (:color blue :hint nil :inherit (scimax-base/heads))
  "
Mark              Operate         Misc              Navigate
----              -------         ----              --------
_fd_: flag del    _C_: copy       _+_: mkdir        _<up>_: up directory
_f#_: autosave    _R_: rename     _o_: open other
_f~_: backups     _D_: delete
_f&_: garbage     _F_: open marks
_fe_: extension
----
_m_: mark         _T_: touch
_/_: directories  _M_: chmod
_@_: symlinks     _G_: chgrp
_O_: omitted      _O_: chown
----
_U_: unmark all   _A_: find regex
_t_: toggle marks _Q_: find/rep
"
  ;; marking
  ("t" dired-toggle-marks "Toggle marks")
  ("m" dired-mark "mark")
  ("u" dired-unmark "unmark")
  ("fd" dired-flag-file-deletion "Flag for deletion")
  ("f#" dired-flag-auto-save-files "Flag autosave")
  ("f~" dired-flag-backup-files "Flag backup files")
  ("f&" dired-flag-garbage-files "Flag garbage files")
  ("fe" dired-flag-extension "Flag extension")
  ("/" dired-mark-directories "Mark directories")
  ("@" dired-mark-symlinks "Mark symlinks")
  ("." dired-mark-extension  "Mark extension")
  ("O" dired-mark-omitted "Mark omitted")
  ("U" dired-unmark-all-marks "Unmark all marks")

  ("C" dired-do-copy)
  ("R" dired-do-rename)
  ("D" dired-do-delete)
  ("F" dired-do-find-marked-files)
  ("!" dired-do-shell-command)
  ("&" dired-do-async-shell-command)

  ("T" dired-do-touch)
  ("M" dired-do-chmod)
  ("G" dired-do-chgrp)
  ("O" dired-do-chown)

  ("A" dired-do-find-regexp)
  ("Q" dired-do-find-regexp-and-replace)

  ("+" dired-create-directory)
  ("o" dired-find-file-other-window)

  ("<up>" dired-up-directory))


(defhydra scimax-item-hydra (:color red :inherit (scimax-base/heads))
  "
org item helper
"
  ("f" org-indent-item "indent item")
  ("d" org-outdent-item "outdent item")
  ("b" org-cycle-list-bullet "cycle bullets")
  ("B" (org-cycle-list-bullet "previous") "cycle bullet backwards")
  ("w" org-move-item-up "move up")
  ("s" org-move-item-down "move down")
  ("n" org-next-item "next")
  ("p" org-previous-item "previous")
  ("i" org-insert-item "insert item")
  ("t" org-toggle-item "toggle"))


(defhydra scimax-src-block-hydra (:color pink :hint nil :inherit (scimax-base/heads))
  "
org babel src block helper functions
_n_ next       _i_ info           _I_ insert header _t_angle
_p_ prev       _c_ check
_h_ goto head  _E_ expand
^ ^            _s_ split
_q_ quit       _r_ remove result  _e_ examplify region
"
  ("i" org-babel-view-src-block-info)
  ("I" org-babel-insert-header-arg)
  ("c" org-babel-check-src-block :color blue)
  ("s" org-babel-demarcate-block :color blue)
  ("n" org-babel-next-src-block)
  ("p" org-babel-previous-src-block)
  ("E" org-babel-expand-src-block :color blue)
  ("e" org-babel-examplify-region :color blue)
  ("r" org-babel-remove-result :color blue)
  ("h" org-babel-goto-src-block-head)
  ("t" org-babel-tangle)
  ("q" nil :color blue))


;; adapted from https://gist.github.com/dfeich/1df4e174d45f05fb5798ca514d28c68a
(defhydra scimax-link-hydra (:color red :inherit (scimax-base/heads))
  "
org link helper
_i_ backward slurp <     _o_ forward slurp >   _n_ next link
_j_ backward barf  <     _k_ forward barf  >   _p_ previous link
_t_: toggle link display
"
  ("i" org-link-edit-backward-slurp)
  ("o" org-link-edit-forward-slurp)
  ("j" org-link-edit-backward-barf)
  ("k" org-link-edit-forward-barf)
  ("t" org-toggle-link-display)
  ("n" org-next-link)
  ("p" org-previous-link))


(defhydra scimax-org-ref-cite-hydra (:color red :hint nil :inherit (scimax-base/heads))
  "
org-ref
_n_ext key _<left>_ swap left
_p_revious key _<right>_ swap right
_s_ort keys
"
  ("s" org-ref-sort-citation-link)
  ("n" org-ref-next-key)
  ("p" org-ref-previous-key)
  ("<right>" (org-ref-swap-citation-link 1))
  ("<left>" (org-ref-swap-citation-link -1)))


(defhydra scimax-org-table (:color red :hint nil :inherit (scimax-base/heads))
  "
org table
_ic_: insert column    _M-<left>_: move col left    _d_: edit field
_dc_: delete column    _M-<right>_: move col right  _e_: eval formula
_ir_: insert row       _M-<up>_: move row up        _E_: export table
_ic_: delete row       _M-<down>_: move row down    _r_: recalculate
_i-_: insert line      _w_: wrap region             _I_: org-table-iterate
_-_: insert line/move  ^ ^                          _D_: formula debugger
_s_ort  _t_ranspose _m_ark
_<_: beginning of table _>_: end of table
"
  ("ic" org-table-insert-column)
  ("ir" org-table-insert-row)
  ("dc" org-table-delete-column)
  ("dr" org-table-kill-row)
  ("i-" org-table-insert-hline)
  ("-" org-table-hline-and-move)

  ("d" org-table-edit-field)
  ("e" org-table-eval-formula)
  ("E" org-table-export :color blue)
  ("r" org-table-recalculate)
  ("I" org-table-iterate)
  ("B" org-table-iterate-buffer-tables)
  ("w" org-table-wrap-region)
  ("D" org-table-toggle-formula-debugger)

  ("M-<up>" org-table-move-row-up)
  ("M-<down>" org-table-move-row-down)
  ("M-<left>" org-table-move-column-left)
  ("M-<right>" org-table-move-column-right)
  ("t" org-table-transpose-table-at-point)

  ("m" (progn (goto-char (org-table-begin))
	      (org-mark-element)))
  ("s" org-table-sort-lines)
  ("<" (goto-char (org-table-begin)))
  (">" (progn (goto-char (org-table-begin))
	      (goto-char (org-element-property :end (org-element-context))))))


(defhydra scimax-org-headline (:color red :hint nil :inherit (scimax-base/heads))
  "
org headline
Navigation               Organize         insert
--------------------------------------------------------------------------------------------------------------------
_n_ext heading           _mu_: move up    _ip_: set property    _s_: narrow subtree _I_: clock in   _,_: priority
_p_revious heading       _md_: move down  _dp_: delete property _w_: widen          _O_: clock out  _0_: rm priority
_f_: forward same level  _mr_: demote     _it_: tag             _r_: refile         _e_: set effort _1_: A
_b_: back same level     _ml_: promote    _t_: todo             _mm_: mark           _E_: inc effort _2_: B
_j_ump to heading        _ih_: insert hl  _id_: deadline        _=_: columns        ^ ^             _3_: C
_F_: next block          _a_: archive     _is_: schedule
_B_: previous block      _S_: sort        _v_: agenda           _/_: sparse tree
"

  ;; Navigation
  ("n" org-next-visible-heading)
  ("p" org-previous-visible-heading)
  ("f" org-forward-heading-same-level)
  ("b" org-backward-heading-same-level)
  ("j" org-goto)
  ("F" org-next-block)
  ("B" org-previous-block)
  ("a" org-archive-subtree-default-with-confirmation)
  ("ih" org-insert-heading)
  ("S" org-sort)
  ("mm" org-mark-subtree)

  ;; organization
  ("mu" org-move-subtree-up)
  ("md" org-move-subtree-down)
  ("mr" org-demote-subtree)
  ("ml" org-promote-subtree)

  ("ip" org-set-property)
  ("dp" org-delete-property)
  ("id" org-deadline)
  ("is" org-schedule)
  ("t" org-todo)
  ("it" org-set-tags)
  ("<tab>" org-cycle)

  ("r" org-refile)
  ("#" org-toggle-comment)
  ("s" org-narrow-to-subtree)
  ("w" widen)
  ("=" org-columns)

  ("I" org-clock-in)
  ("O" org-clock-out)
  ("e" org-set-effort)
  ("E" org-inc-effort)
  ("," org-priority)
  ("0" (org-priority 32))
  ("1" (org-priority 65))
  ("2" (org-priority 66))
  ("3" (org-priority 67))

  ;; misc
  ("v" org-agenda)
  ("/" org-sparse-tree))


(defhydra scimax-mu4e (:color red :hint nil :inherit (scimax-base/heads))
  "
mu4e
_u_: Update"
  ("u" mu4e-update-mail-and-index))

(defhydra scimax-elfeed (:color red :hint nil :inherit (scimax-base/heads))
  "
elfeed
_u_: Update"
  ("u" elfeed-update))



(defun scimax-dwim-send ()
  "Send something to Python in a dwim sense.
With a prefix arg, send the buffer.
If a region is active, send that.
Otherwise, send the current statement
"
  (interactive "P")
  (cond
   (current-prefix-arg
    (python-shell-send-buffer))
   ((region-active-p)
    (python-shell-send-region (region-beginning) (region-end)))
   (t
    (python-shell-send-statement))))


(defhydra scimax-python-mode (:color red :hint nil :inherit (scimax-base/heads))
  "
Python helper
_a_: begin def/class  _w_: move up          _x_: syntax    _Sb_: send buffer
_e_: end def/class    _s_: move down        _n_: next err  _Sr_: send region
_<_: dedent line      _m_: mark class/def   _p_: prev err  _Ss_: send statement
_>_: indent line      ^ ^                   ^ ^            _Sh: switch shell 
_j_: jump to
_._: goto definition

_t_: run tests  _8_: autopep8
"
  ("a" beginning-of-defun)
  ("e" end-of-defun)
  ("<" python-indent-shift-left)
  (">" python-indent-shift-right)
  ("j" counsel-imenu)

  ("t" projectile-test-project)
  ("." xref-find-definitions)
  ("x" python-check)
  ("n" flymake-goto-next-error)
  ("p" flymake-goto-previous-error)

  ("m" python-mark-defun)

  ("w" move-text-up)
  ("s" move-text-down)

  ("Sb" python-shell-send-buffer)
  ("Sr" python-shell-send-region)
  ("Ss" python-shell-send-statement)
  ("Sh" python-shell-switch-to-shell)

  ("8" autopep8))





;;* the end

(provide 'scimax-hydra)

;;; scimax-hydra.el ends here
