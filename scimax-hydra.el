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
  "Push current HYDRA to a stack."
  `(progn
     (scimax-hydra-push hydra-curr-body-fn)
     (call-interactively ',hydra)))

(defhydra scimax-base (:color red)
  "base"
  ("," scimax-hydra-pop "back")
  ("`" scimax-hydra "show stack")
  ("x" counsel-M-x "M-x")
  ("/" undo-tree-undo "undo" :color red)
  ("?" undo-tree-redo "redo" :color red)
  ("q" nil "quit"))

;; * scimax hydra

(defhydra scimax (:color blue :inherit (scimax-base/heads)
			 :columns 4 :body-pre (scimax-hydra-reset))
  "scimax"
  ("a" (scimax-open-hydra scimax-applications/body) "Applications")
  ("b" (scimax-open-hydra scimax-buffers/body) "Buffers")
  ("e" (scimax-open-hydra scimax-errors/body) "Edit/Errors")
  ("f" (scimax-open-hydra scimax-files/body) "Files")
  ("g" (scimax-open-hydra scimax-google/body) "Google")
  ("h" (scimax-open-hydra scimax-help/body) "Help")
  ("i" (scimax-open-hydra scimax-insert/body) "Insert")
  ("j" (scimax-open-hydra scimax-jump/body) "Jump")
  ("k" (scimax-open-hydra scimax-bookmarks/body) "Bookmarks")
  ("l" (scimax-open-hydra scimax-lisp/body) "Lisp")
  ("m" (scimax-open-hydra scimax-minor-modes/body) "Minor modes")
  ("n" (scimax-open-hydra scimax-navigation/body) "Navigation")
  ("o" (scimax-open-hydra scimax-org/body) "org")
  ("p" (scimax-open-hydra hydra-projectile/body) "Project")
  ("r" (scimax-open-hydra scimax-registers/body) "Registers/resume")
  ("s" (scimax-open-hydra scimax-search/body) "Search")
  ("t" (scimax-open-hydra scimax-text/body) "Text")
  ("v" (scimax-open-hydra scimax-version-control/body) "Version control")
  ("w" (scimax-open-hydra scimax-windows/body) "Windows")
  ("z" (scimax-open-hydra scimax-settings/body) "Settings"))

(global-set-key (kbd "<f12>") 'scimax/body)

;; ** applications

(defhydra scimax-applications (:color blue :inherit (scimax-base/heads) :columns 3)
  "applications"
  ("a" app "app")
  ("c" ivy-contacts "Contacts")
  ("C" google-calendar "Calendar")
  ("d" dired-x "dired")
  ("e" mu4e "email")
  ("f" finder "Finder")
  ("i" messages "iChat")
  ("k" package-list-packages "Packages")
  ("m" compose-mail "compose email")
  ("r" elfeed "RSS")
  ("s" safari "Safari")
  ("t" terminal "Terminal")
  ("w" twit "Twitter")
  ("W" tweetdeck "Tweetdeck")
  ("o" (scimax-open-hydra scimax-office/body) "Office"))


(defhydra scimax-office (:color blue)
  "Office"
  ("e" excel"Excel")
  ("p" powerpoint "Powerpoint")
  ("w" word "Word"))

;; ** buffers
(defhydra scimax-buffers (:color blue :inherit (scimax-base/heads) :columns 3)
  "buffer"
  ("1" delete-other-windows "delete other windows")
  ("2" split-window-below "split below")
  ("3" split-window-right "split right")
  ("5" make-frame-command "new frame")
  ("4" kill-buffer-and-window "kill buffer and window")
  ("6" kill-some-buffers "kill some buffers")
  ("a" ace-window "ace-window")
  ("b" switch-to-buffer "switch")
  ("A" kill-all-buffers "kill all buffers")
  ("f" other-frame "other frame" :color red)
  ("F" switch-to-buffer-other-frame "in other frame")
  ("k" kill-this-buffer "kill" :color red)
  ("K" kill-other-buffers "kill others")
  ("l" ibuffer "List")
  ("M" kill-matching-buffers "kill matching")
  ("n" next-buffer "next buffer" :color red)
  ("o" other-window "other window" :color red)
  ("O" switch-to-buffer-other-window "in other window" :color red)
  ("p" previous-buffer "previous buffer" :color red)
  ("s" save-buffer "save")
  ("S" (switch-to-buffer "*scratch*") "scratch")
  ("w" write-file "Rename")
  ("y" bury-buffer "Bury"))

;; ** errors

(defhydra scimax-errors (:color blue :inherit (scimax-base/heads) :columns 3)
  "edit/errors"
  ("a" edit-abbrevs "Edit abbrevs")
  ("n" next-error "Next error")
  ("p" previous-error "Previous error"))

;; ** files
(defhydra scimax-files (:color blue :inherit (scimax-base/heads) :columns 3)
  "files"
  ("4" window-file-other-window "Find other find")
  ("5" find-file-other-frame "Find other frame")
  ("b" describe-file "Describe file")
  ("d" (dired default-directory) "Dired")
  ("f" find-file "Find-file")
  ("l" counsel-locate "Locate")
  ("p" ffap "File at point")
  ("r" counsel-recentf "Recent")
  ("s" save-buffer "Save")
  ("w" write-file "Rename"))

;; ** google
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

;; ** help

(defhydra scimax-help (:color blue :inherit (scimax-base/heads) :columns 3)
  "help"
  ("a" apropos "Apropos")
  ("c" describe-command "Command")
  ("e" info-emacs-manual "Emacs manual")
  ("f" describe-function "Function")
  ("h" describe-theme "Theme")
  ("i" info "Info")
  ("k" describe-key "Key")
  ("K" describe-keymap "Keymap")
  ("m" describe-mode "Mode")
  ("M" describe-man "Man")
  ("o" ore "Org explorer")
  ("p" describe-package "Package")
  ("s" describe-syntax "Syntax")
  ("t" describe-text-properties "Text properties")
  ("T" help-with-tutorial "Emacs tutorial")
  ("v" describe-variable "Variable")
  ("x" scimax-help "Scimax help")
  ("w" woman "Woman"))

;; ** insert

(defhydra scimax-insert (:color blue :inherit (scimax-base/heads) :columns 3)
  "help"
  ("b" insert-buffer "Buffer")
  ("c" insert-char "Char")
  ("e" ivy-insert-org-entity "Org-entity")
  ("f" insert-file "File")
  ("l" lorem-ipsum-insert-paragraphs "Lorem ipsum" :color red)
  ("p" insert-parentheses "Parentheses")
  ("r" insert-register "Register")
  ("t" org-time-stamp "Timestamp"))

;; ** jump

(defhydra scimax-jump (:color blue :inherit (scimax-base/heads) :columns 3)
  "jump"
  ("<" beginning-of-buffer "Beginning of buffer")
  (">" end-of-buffer "End of buffer")
  ("a" beginning-of-line "Beginning of line")
  ("d" ace-window "Ace window")
  ("e" end-of-line "End of line")
  ("c" (scimax-open-hydra scimax-jump-char/body) "Char")
  ("g" goto-line "Goto line")
  ("l" (scimax-open-hydra scimax-jump-line/body) "Line")
  ("k" ace-link "Link")
  ("o" (scimax-open-hydra scimax-jump-org/body) "Org")
  ("s" (scimax-open-hydra scimax-jump-symbol/body) "Symbol" )
  ("w" (scimax-open-hydra scimax-jump-word/body) "Word"))


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
  ("a" avy-goto-line-above "Above")
  ("b" avy-goto-line-below "Below")
  ("l" avy-goto-line "Line"))

(defhydra scimax-jump-org (:color blue :inherit (scimax-base/heads) :columns 3)
  "org"
  ("a" ivy-org-jump-to-agenda-heading "Agenda heading")
  ("d" ivy-org-jump-to-heading-in-directory "Directory heading")
  ("h" ivy-org-jump-to-heading "Heading")
  ("o" ivy-org-jump-to-open-headline "Open heading")
  ("p" ivy-org-jump-to-project-headline "Project heading")
  ("v" ivy-org-jump-to-visible-headline "Visible heading"))

(defhydra scimax-jump-word (:color blue :inherit (scimax-base/heads) :columns 3)
  "word"
  ("0" avy-goto-word-0 "word0")
  ("1" avy-goto-word-1 "word1")
  ("a" avy-goto-word-0-above "above-0")
  ("A" avy-goto-word-1-above "above-1")
  ("b" avy-goto-word-0-below "below0")
  ("B" avy-goto-word-1-below "below1")
  ("o" avy-goto-word-or-subword-1 "word or subword")
  ("s" avy-subword-0 "subword-0")
  ("S" avy-subword-1 "subword-1"))

(defhydra scimax-jump-symbol (:color blue :inherit (scimax-base/heads) :columns 3)
  "symbol"
  ("a" avy-goto-symbol-1-above "Above")
  ("b" avy-goto-symbol-1-below "below")
  ("s" avy-goto-symbol-1 "symbol"))

;; ** bookmarks

(defhydra scimax-bookmarks (:color blue :inherit (scimax-base/heads) :columns 3)
  "bookmarks"
  ("j" bookmark-jump "jump")
  ("l" bookmark-bmenu-list "list")
  ("n" bookmark-set "new"))

;; ** lisp

(defhydra scimax-lisp (:color red :inherit (scimax-base/heads) :columns 3)
  "lisp"
  ("c" byte-recompile-file "byte-compile file")
  ("d" byte-recompile-directory "byte-compile dir")
  ("e" eval-buffer "buffer")
  ("l" load-file "load file")
  ("r" eval-region "region"))

;; ** minor modes

(defhydra scimax-minor-modes (:color blue :inherit (scimax-base/heads) :columns 3)
  "minor modes"
  ("a" aggressive-indent-mode "aggressive indent")
  ("b" org-bullets-mode "org-bullets")
  ("e" emacs-keybinding-command-tooltip-mode "keybinding")
  ("n" nlinum-mode "nlinum")
  ("r" rainbow-mode "rainbow"))

;; ** navigation

(defhydra scimax-navigation (:color red :inherit (scimax-base/heads) :columns 3)
  "navigation"
  ("j" backward-char "←")
  (";" forward-char "→")
  ("k" previous-line "↑")
  ("l" next-line "↓")
  ("a" beginning-of-line "line-beginning")
  ("e" end-of-line "line-ending")
  ("d" delete-char "delete" :color red)
  ("b" backward-delete-char "backspace" :color red)
  ("<" beginning-of-buffer "beginning of buffer")
  (">" end-of-buffer "end of buffer")
  ("p" (scimax-open-hydra scimax-nav-paragraph/body) "paragraph" :color blue)
  ("w" (scimax-open-hydra scimax-nav-word/body) "word" :color blue)
  ("s" (scimax-open-hydra scimax-nav-sentence/body) "sentence" :color blue))


(defhydra scimax-nav-word (:color red :inherit (scimax-base/heads) :columns 3)
  "word"
  ("j" backward-word "previous word")
  (";" forward-word "next word")
  ("k" previous-line "previous line")
  ("l" next-line "next line")
  ("b" backward-kill-word "backward delete")
  ("d" kill-word "backward delete")
  ("t" transpose-words "transpose"))


(defhydra scimax-nav-sentence (:color red :inherit (scimax-base/heads) :columns 3)
  "sentence"
  ("j" backward-sentence "previous sentence")
  (";" forward-sentence "next sentence")
  ("k" previous-line "previous line")
  ("l" next-line "next line")
  ("b" (kill-sentence -1) "backspace")
  ("d" kill-sentence "delete")
  ("t" transpose-sentences "transpose"))


(defhydra scimax-nav-paragraph (:color red :inherit (scimax-base/heads) :columns 3)
  "paragraph"
  ("j" backward-paragraph "previous paragraph")
  (";" forward-paragraph "next paragraph")
  ("k" previous-line "previous line")
  ("l" next-line "next line")
  ("b" (kill-paragraph -1) "backspace")
  ("d" (kill-paragraph nil) "delete")
  ("t" transpose-paragraphs "transpose"))

;; ** org

(defhydra scimax-org (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-mode"
  ("'" org-edit-special "edit")
  ("a" org-agenda "agenda")
  ("b" (scimax-open-hydra scimax-org-block/body) "block")
  ("c" ivy-contacts "contacts")
  ("d" (scimax-open-hydra scimax-org-db/body) "database")
  ("e" (scimax-open-hydra hydra-ox/body) "export")
  ("g" org-babel-tangle "tangle")
  ("n" outline-next-heading "next heading" :color red)
  ("p" outline-previous-heading "previous heading" :color red)
  ("r" (scimax-open-hydra scimax-org-ref/body) "org-ref")
  ("t" (scimax-open-hydra scimax-org-toggle/body) "toggle"))


(defhydra scimax-org-block (:color blue :inherit (scimax-base/heads) :columns 3)
  "org blocks"
  ("c" org-babel-remove-result "clear result")
  ("e" org-babel-execute-src-block "execute")
  ("n" org-next-block "next block")
  ("p" org-previous-block "previous block"))


(defhydra scimax-org-ref (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-ref"
  ("b" org-ref-insert-bibliography-link "bibliography")
  ("c" org-ref-insert-link "cite")
  ("l" (org-ref-insert-link '(16)) "label")
  ("r" (org-ref-insert-link '(4)) "ref")
  ("o" org-ref "org-ref"))


(defhydra scimax-org-db (:color blue :inherit (scimax-base/heads) :columns 3)
  "org-db"
  ("h" org-db-open-heading "heading")
  ("f" org-db-open-file "file")
  ("l" org-db-open-link-in-file "link")
  ("r" org-db-open-recent-file "recent file"))


(defhydra scimax-org-toggle (:color blue :inherit (scimax-base/heads) :columns 3)
  "toggle"
  ("e" org-toggle-pretty-entities "pretty entities")
  ("i" org-toggle-inline-images "images")
  ("l" org-toggle-latex-fragment "latex"))

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
  ("q" nil "quit"))


;; ** project

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))


(defhydra hydra-projectile (:color teal
				   :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))


;; ** registers/resume/replace

(defhydra scimax-registers (:color blue :inherit (scimax-base/heads) :columns 3)
  "register/resume/replace"
  ("a" append-to-register "append to register")
  ("c" copy-to-register "copy to register")
  ("f" frameset-to-register "frames to register")
  ("h" helm-resume "helm resume")
  ("i" insert-register "insert")
  ("j" jump-to-register "jump to")
  ("l" list-registers "list")
  ("n" number-to-register "number to register")
  ("p" point-to-register "store point")
  ("q" query-replace "replace")
  ("v" ivy-resume "ivy resume")
  ("w" window-configuration-to-register "window to register")
  ("x" query-replace-regexp "replace regexp")
  ("y" counsel-yank-pop "yank ring"))


;; ** search

(defhydra scimax-search (:color blue :inherit (scimax-base/heads) :columns 3)
  "search"
  ("m" multioccur "moccur")
  ("o" occur "occur")
  ("r" isearch-backward "search back")
  ("s" counsel-grep-or-swiper "search"))


;; ** text

(defhydra scimax-text (:color blue :inherit (scimax-base/heads) :columns 3)
  "text"
  ("A" (mark-whole-buffer) "Select all")
  ("c" kill-ring-save "Copy")
  ("C" capitalize-dwim "Capitalize")
  ("d" downcase-dwim "Downcase")
  ("k" kill-region "Cut")
  ("l" kill-whole-line "Kill line" :color red)
  ("m" set-mark-command "Set mark" :color red)
  ("n" (scimax-open-hydra scimax-narrow/body) "narrow")
  ("s" (scimax-open-hydra scimax-spellcheck/body) "spell-check")
  ("S" sentence-case-region "Sentence case")
  ("t" (scimax-open-hydra scimax-transpose/body) "transpose")
  ("u" upcase-dwim "Upcase")
  ("v" yank "paste")
  ("w" count-words "count words"))


(defhydra scimax-kill (:color blue :inherit (scimax-base/heads) :columns 3)
  "kill"
  ("c" kill-comment "comment")
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


(defhydra scimax-spellcheck (:color blue :inherit (scimax-base/heads) :columns 3)
  "spell"
  ("b" ispell-buffer "buffer")
  ("c" flyspell-correct-previous-word-generic "correct")
  ("w" ispell-word "word"))


(defhydra scimax-transpose (:color blue :inherit (scimax-base/heads) :columns 3)
  "transpose"
  ("c" transpose-chars "chars")
  ("l" transpose-lines "lines")
  ("p" transpose-paragraphs "paragraphs")
  ("s" transpose-sentences "sentences")
  ("x" transpose-sexps "sexps")
  ("w" transpose-words "words"))


;; ** version control

(defhydra scimax-version-control (:color blue :inherit (scimax-base/heads) :columns 3)
  "vc"
  ("b" magit-branch-popup "branch")
  ("c" magit-commit-popup "commit")
  ("d" magit-diff-popup "diff")
  ("f" magit-fetch-popup "fetch")
  ("k" magit-checkout "checkout")
  ("l" magit-log-all "log")
  ("n" vc-next-action "next action")
  ("p" magit-push-popup "push")
  ("P" magit-pull-popup "pull")
  ("r" magit-revert-popup "revert")
  ("s" magit-stage "Stage" :color red)
  ("v" magit-status "magit"))


;; ** windows

(defhydra scimax-windows (:color blue :inherit (scimax-base/heads) :columns 3)
  "windows"
  ("a" ace-window "ace window")
  ("0" delete-window "delete window")
  ("b" bury-buffer "bury")
  ("1" delete-other-windows "delete other")
  ("2" split-window-below "split below")
  ("3" split-window-right "split right")
  ("o" other-window "other window")
  ("5" other-frame "other frame")
  ("%" delete-frame "delete frame"))


;; ** Customize

(defhydra scimax-settings (:color blue :inherit (scimax-base/heads) :columns 3)
  "Settings"
  ("+" text-scale-increase "increase font size" :color red)
  ("-" text-scale-decrease "decrease font size" :color red)
  ("0" (text-scale-adjust 0) "reset font size")
  ("c" customize "Customize")
  ("f" (ivy-read "Font: " helm-xfonts-cache
		 :action
		 (lambda (font)
		   (message "chose %s" font)
		   (set-frame-font font 'keep-size)))
   "Font")
  ("t" helm-themes "Theme")
  ("u" scimax-customize-user "Customize scimax user"))

(provide 'scimax-hydra)

;;; scimax-hydra.el ends here
