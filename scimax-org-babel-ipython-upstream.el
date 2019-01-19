;;; scimax-org-babel-ipython-upstream.el --- Modifications to the upstream ob-ipython module -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains monkey patches and enhancements to the upstream ob-ipython
;; module. Several new customizations are now possible.
;;
;; Some new header arguments:
;;
;; :display can be used to specify which mime-types are displayed. The default is all of them.
;; :restart can be used to restart the kernel before executing the cell
;; :async is not new, but it works by itself now, and causes an asynchronous evaluation of the cell

(require 'scimax-ob)

;; * Customizations

;;; Code:

(defcustom ob-ipython-eldoc-integration nil
  "If non-nil use eldoc to show signatures."
  :group 'ob-ipython)

(defcustom ob-ipython-buffer-unique-kernel t
  "If non-nil use a unique kernel for each buffer."
  :group 'ob-ipython)

(defcustom ob-ipython-show-mime-types t
  "If non-nil show mime-types in output."
  :group 'ob-ipython)

(defcustom ob-ipython-exception-results t
  "If non-nil put the contents of the traceback buffer as results."
  :group 'ob-ipython)

(defcustom ob-ipython-suppress-execution-count nil
  "If non-nil do not show the execution count in output."
  :group 'ob-ipython)

(defcustom ob-ipython-kill-kernel-on-exit t
  "If non-nil, prompt user to kill kernel when killing a buffer."
  :group 'ob-ipython)

(defcustom ob-ipython-delete-stale-images t
  "If non-nil remove images that will be replaced."
  :group 'ob-ipython)

(defcustom ob-ipython-mime-formatters
  '((text/plain . ob-ipython-format-text/plain)
    (text/html . ob-ipython-format-text/html)
    (text/latex . ob-ipython-format-text/latex)
    (text/org . ob-ipython-format-text/org)
    (image/png . ob-ipython-format-image/png)
    (image/svg+xml . ob-ipython-format-image/svg+xml)
    (application/javascript . ob-ipython-format-application/javascript)
    (default . ob-ipython-format-default)
    (output . ob-ipython-format-output))
  "An alist of (mime-type . format-func) for mime-types.
Each function takes two arguments, which is file-or-nil and a
string to be formatted."
  :group 'ob-ipython)

(defcustom ob-ipython-plain-text-filter-regexps
  '(
					;this is what boring python objects look like. I never need to see these, so
					;we strip them out. That might be a strong opinion though, and might
					;surprise people who like to or are used to seeing them.
    "^<.*at 0x.*>"
    )
  "A list of regular expressions to filter out of text/plain results."
  :group 'ob-ipython)

(defcustom ob-ipython-key-bindings
  '(("<return>" . #'newline-and-indent)
    ("C-<return>" . #'org-ctrl-c-ctrl-c)
    ("M-<return>" . (lambda () (interactive) (scimax-execute-and-next-block t)))
    ("S-<return>" . #'scimax-execute-and-next-block)
    ("M-S-<return>" . #'scimax-execute-to-point)
    ("s-<return>" . #'scimax-ob-ipython-restart-kernel-execute-block)
    ("M-s-<return>" . #'scimax-restart-ipython-and-execute-to-point)
    ("H-<return>" . #'scimax-ob-ipython-restart-kernel-execute-buffer)
    ("H-k" . #'scimax-ob-ipython-kill-kernel)
    ("H-r" . #'org-babel-switch-to-session)

    ;; navigation commands
    ("s-i" . #'org-babel-previous-src-block)
    ("s-k" . #'org-babel-next-src-block)
    ("H-q" . #'scimax-jump-to-visible-block)
    ("H-s-q" . #'scimax-jump-to-block)

    ;; editing commands
    ("H-=" . #'scimax-insert-src-block)
    ("H--" . #'scimax-split-src-block)
    ("H-n" . #'scimax-ob-copy-block-and-results)
    ("H-w" . #'scimax-ob-kill-block-and-results)
    ("H-o" . #'scimax-ob-clone-block)
    ("s-w" . #'scimax-ob-move-src-block-up)
    ("s-s" . #'scimax-ob-move-src-block-down)
    ("H-l" . #'org-babel-remove-result)
    ("H-s-l" . #'scimax-ob-clear-all-results)
    ("H-m" . #'scimax-merge-ipython-blocks)
    ("H-h" . #'scimax-ob-edit-header)
    ("H-M-l" . #'scimax-ob-toggle-line-numbers)
    ("s-." . #'scimax-ob-ipython-complete-ivy)
    ("s-/" . #'ob-ipython-inspect)

    ;; the jupyter hydras
    ("H-e" . #'scimax-jupyter-edit-mode/body)
    ("H-c" . #'scimax-jupyter-command-mode/body)

    ;; The hydra/popup menu
    ("H-s" . #'scimax-obi/body)
    ("<mouse-3>" . #'scimax-ob-ipython-popup-command))
  "An alist of key bindings and commands.
These are activated in function `ob-ipython-key-bindings'."
  :group 'ob-ipython)

(defcustom ob-ipython-menu-items
  '(("Execute"
     ["Current block" org-ctrl-c-ctrl-c t]
     ["Current and next" scimax-execute-and-next-block t]
     ["To point" scimax-execute-to-point t]
     ["Restart/block" scimax-ob-ipython-restart-kernel-execute-block t]
     ["Restart/to point" scimax-restart-ipython-and-execute-to-point t]
     ["Restart/buffer" scimax-ob-ipython-restart-kernel-execute-buffer t])
    ("Edit"
     ["Move block up" scimax-ob-move-src-block-up t]
     ["Move block down" scimax-ob-move-src-block-down t]
     ["Kill block" scimax-ob-kill-block-and-results t]
     ["Copy block" scimax-ob-copy-block-and-results t]
     ["Clone block" scimax-ob-clone-block t]
     ["Split block" scimax-split-src-block t]
     ["Clear result" org-babel-remove-result t]
     ["Edit header" scimax-ob-edit-header t]
     ["Toggle line numbers" scimax-ob-toggle-line-numbers t])
    ("Navigate"
     ["Previous block" org-babel-previous-src-block t]
     ["Next block" (lambda ()
		     (interactive)
		     (org-babel-next-src-block))
      t]
     ["Jump to visible block" scimax-jump-to-visible-block t]
     ["Jump to block" scimax-jump-to-block t])
    ["Inspect" ob-ipython-inspect t]
    ["Show source" (lambda ()
		     (interactive)
		     (ob-ipython-inspect))
     t]
    ["Kill kernel" scimax-ob-ipython-kill-kernel t]
    ["Switch to repl" org-babel-switch-to-session t])
  "Items for the menu bar and popup menu."
  :group 'ob-ipython)

(defcustom ob-ipython-buttons
  '(("<run>"  org-ctrl-c-ctrl-c "Click to run")
    ("<restart and run>"  scimax-ob-ipython-restart-kernel-execute-block "click to restart and run")
    ("<repl>"  org-babel-switch-to-session "Click to open repl")
    ("<interrupt>"  ob-ipython-interrupt-kernel "Click to interrupt")
    ("<delete block>"  scimax-ob-kill-block-and-results "kill block")
    ("<menu>" scimax-ob-ipython-popup-command "Popup menu")
    ("<output>" (lambda () (interactive)
		  (pop-to-buffer "*ob-ipython-out*")) "open output buffer")
    ("<debug>" (lambda () (interactive)
		 (pop-to-buffer "*ob-ipython-debug*")) "open debug buffer")
    ("<execute>" (lambda () (interactive)
		   (pop-to-buffer "*ob-ipython-execute*")) "open execute buffer"))
  "A list of (text cmd help) to make clickable buttons.
text is regexp/string that will become a button.
cmd is run when you click on the button.
help is a string for a tooltip."
  :group 'ob-ipython)

(defun ob-ipython-key-bindings ()
  "Function to define key-bindings.
Usually called in a hook function."
  (cl-loop for cell in ob-ipython-key-bindings
	   do
	   (eval `(scimax-define-src-key ipython ,(car cell) ,(cdr cell)))))

(add-hook 'org-mode-hook 'ob-ipython-key-bindings t)


;; * org templates and default header args

;; org 9.2 changed this variable in a backwards incompatible way. I think I do
;; all of these through yasnippet now, so I am going to just comment these out
;; for now, in case I want to add them to a snippet later.

;; (add-to-list 'org-structure-template-alist
;; 	     '("ip" "#+BEGIN_SRC ipython\n?\n#+END_SRC"
;; 	       "<src lang=\"python\">\n?\n</src>"))

;; (add-to-list 'org-structure-template-alist
;; 	     '("ipv" "#+BEGIN_SRC ipython :results value\n?\n#+END_SRC"
;; 	       "<src lang=\"python\">\n?\n</src>"))

;; (add-to-list 'org-structure-template-alist
;; 	     '("plt" "%matplotlib inline\nimport matplotlib.pyplot as plt\n?"
;; 	       ""))

;; (add-to-list 'org-structure-template-alist
;; 	     '("np" "import numpy as np\n?"
;; 	       ""))

;; (add-to-list 'org-structure-template-alist
;; 	     '("anp" "import autograd.numpy as np\n?"
;; 	       ""))



(setq org-babel-default-header-args:ipython
      '((:results . "output replace drawer")
	(:session . "ipython")
	(:exports . "both")
	(:cache .   "no")
	(:noweb . "no")
	(:hlines . "no")
	(:tangle . "no")
	(:eval . "never-export")))


(defun scimax-install-ipython-lexer ()
  "Install the IPython lexer for Pygments.
You need this to get syntax highlighting."
  (interactive)
  (unless (= 0
	     (shell-command
	      "python -c \"import pygments.lexers; pygments.lexers.get_lexer_by_name('ipython')\""))
    (shell-command "pip install git+git://github.com/sanguineturtle/pygments-ipython-console")))


;; * A hydra for ob-ipython blocks

(defhydra scimax-obi (:color blue :hint nil)
  "
        Execute                   Navigate                 Edit             Misc
-----------------------------------------------------------------------------------------------------------------------------
    _<return>_: current           _i_: previous            _w_: move up     _._: inspect                 _<up>_:
  _S-<return>_: current to next   _k_: next                _s_: move down   _l_: clear result  _<left>_:           _<right>_:
_S-M-<return>_: to point          _q_: visible             _x_: kill        _L_: clear all              _<down>_:
  _s-<return>_: Restart/block     _Q_: any                 _n_: copy        _,_: complete
_M-s-<return>_: Restart/to point  _C-<up>_: goto start     _c_: clone       _o_: toggle result folding
  _H-<return>_: Restart/buffer    _C-<down>_: goto end     _m_: merge
           _K_: kill kernel       _C-<left>_: word left    _-_: split
           _r_: Goto repl         _C-<right>_: word right  _+_: insert above
           ^ ^                    ^ ^                      _=_: insert below
           ^ ^                    ^ ^                      _h_: header
_[_: dedent _]_: indent  _3_: toggle comment  _z_: undo    _y_: redo
Convert
------------------------------------------------------------------
_y_: to code
_M_: to markdown
_O_: to org
markdown headings _1_: _2_: _3_: _4_: _5_: _6_:
"
  ("o" ob-ipython-toggle-output :color red)
  ("<up>" ob-ipython-edit-up :color red)
  ("<down>" ob-ipython-edit-down :color red)
  ("<left>" left-char :color red)
  ("<right>" right-char :color red)
  ("C-<up>" ob-ipython-jump-to-first-line)
  ("C-<down>" ob-ipython-jump-to-end-line)
  ("C-<left>" left-word :color red)
  ("C-<right>" right-word :color red)

  ;; These don't really have great analogs in org-mode, but maybe it makes sense
  ;; to be able to do this.
  ("y" (ob-ipython-convert-block-to "ipython"))
  ("M" (ob-ipython-convert-block-to "markdown"))
  ("O" (ob-ipython-convert-block-to "org"))

  ;; These change to markdown block and trim blank lines off the top and add #
  ;; to beginning
  ("1" (ob-ipython-markdown-headings 1))
  ("2" (ob-ipython-markdown-headings 2))
  ("3" (ob-ipython-markdown-headings 3))
  ("4" (ob-ipython-markdown-headings 4))
  ("5" (ob-ipython-markdown-headings 5))
  ("6" (ob-ipython-markdown-headings 6))

  ("z" undo-tree-undo :color red)
  ("y" undo-tree-redo :color red)
  ("[" (progn
	 (org-edit-special)
	 (python-indent-line t)
	 (org-edit-src-exit))  :color red)
  ("]" (progn
	 (org-edit-special)
	 (python-indent-line)
	 (org-edit-src-exit))  :color red)
  ("<return>" org-ctrl-c-ctrl-c :color red)
  ("S-<return>" scimax-execute-and-next-block :color red)
  ("S-M-<return>" scimax-execute-to-point)
  ("s-<return>" scimax-ob-ipython-restart-kernel-execute-block)
  ("M-s-<return>" scimax-restart-ipython-and-execute-to-point)
  ("H-<return>" scimax-ob-ipython-restart-kernel-execute-buffer)
  ("K" scimax-ob-ipython-kill-kernel)
  ("r" org-babel-switch-to-session)

  ("i" org-babel-previous-src-block :color red)
  ("k" org-babel-next-src-block :color red)
  ("q" scimax-jump-to-visible-block)
  ("Q" scimax-jump-to-block)

  ("w" scimax-ob-move-src-block-up :color red)
  ("s" scimax-ob-move-src-block-down :color red)
  ("x" scimax-ob-kill-block-and-results)
  ("n" scimax-ob-copy-block-and-results)
  ("c" scimax-ob-clone-block)
  ("m" scimax-merge-ipython-blocks)
  ("-" scimax-split-src-block)
  ("+" scimax-insert-src-block)
  ("=" (scimax-insert-src-block t))
  ("l" org-babel-remove-result)
  ("L" scimax-ob-clear-all-results)
  ("h" scimax-ob-edit-header)
  ("3" org-comment-dwim :color red)
  ("." ob-ipython-inspect)
  ("," scimax-ob-ipython-complete-ivy))

;; * command/edit-mode hydra

;;These hydras are to mimic as closely as possible the Jupyter keybindings.

(defun ob-ipython-convert-block-to (type)
  "Convert current block to TYPE.
TYPE is usually one of ipython, markdown, org
Note: you will lose header arguments from this."
  (interactive (list (completing-read "Type: " '(ipython markdown org))))
  (let* ((src-info (org-babel-get-src-block-info 'light))
	 (header-start (sixth src-info))
	 (header-end (save-excursion (goto-char header-start)
				     (line-end-position))))
    (setf (buffer-substring header-start header-end)
	  (format "#+BEGIN_SRC %s" type))))


(defun ob-ipython-markdown-headings (N)
  "Convert block to markdown and set first line to heading level N."
  (interactive "nLevel: ")
  (ob-ipython-convert-block-to "markdown")
  (save-restriction
    (org-narrow-to-block)
    (goto-char (point-min))
    (forward-line)
    (delete-trailing-whitespace)
    (while (looking-at "^$") (delete-char 1))
    (insert (make-string N ?#))
    (insert " ")))


(defun ob-ipython-toggle-output ()
  "Toggle folded state of results if there are some."
  (interactive)
  (let ((loc (org-babel-where-is-src-block-result)))
    (when loc
      (save-excursion
	(goto-char loc)
	(org-cycle)))))


(defun ob-ipython-jump-to-first-line ()
  "Move point to start of first line in the src block."
  (interactive)
  (org-edit-special)
  (goto-char (point-min))
  (org-edit-src-exit))


(defun ob-ipython-jump-to-end-line ()
  "Move point to end of last line in the src block."
  (org-edit-special)
  (goto-char (point-max))
  (org-edit-src-exit))


(defun ob-ipython-mark-code ()
  "Mark the code in the block."
  (interactive)
  (org-edit-special)
  (let ((p0 (point-min))
	(p1 (point-max)))
    (goto-char p0)
    (org-edit-src-exit)
    (set-mark (point))
    (goto-char (+ (point) (- p1 2)))))


(defun ob-ipython-merge-next-cell ()
  "Merge current cell with next one."
  (interactive)
  (let ((r1 (point))
	(r2 (save-excursion (org-babel-next-src-block) (point))))
    (scimax-merge-ipython-blocks r1 r2)))


;; https://www.cheatography.com/weidadeyue/cheat-sheets/jupyter-notebook/
(defhydra scimax-jupyter-edit-mode (:color blue :hint nil)
  "
 Execute                            Edit           Navigate
-----------------------------------------------------------------------------------------
_C-<return>_: run cell           _[_: dedent            _C-<up>_: goto start           _<up>_:
_S-<return>_: run cell and next  _]_: indent          _C-<down>_: goto end    _<left>_:        _<right>_:
_M-<return>_: run cell and new   _-_: split cell      _C-<left>_: word left          _<down>_:
^ ^                              _/_: toggle comment _C-<right>_: word right
^ ^                              _a_: select cell

_c_: command mode   _z_: undo   _y_: redo
"
  ("[" (python-indent-line t) "dedent" :color red)
  ("]" (python-indent-line) "indent" :color red)
  ("a" ob-ipython-mark-code "select all")
  ("z" undo-tree-undo "undo")
  ("y" undo-tree-redo "redo")

  ;; I don't have a <home> or <end> on my mac keyboard...
  ;; ("<home>" ob-ipython-jump-to-first-line "goto cell start")
  ;; ("C-<end>" ob-ipython-jump-to-end-line "goto cell end")

  ("<up>" ob-ipython-edit-up "move cursor up or previous cell" :color red)
  ("<down>" ob-ipython-edit-down "move cursor down or next cell" :color red)
  ("<left>" left-char "move cursor left" :color red)
  ("<right>" right-char "move cursor right" :color red)
  ("C-<up>" ob-ipython-jump-to-first-line "goto cell start")
  ("C-<down>" ob-ipython-jump-to-end-line  "goto cell end")
  ("C-<left>" left-word "one word left" :color red)
  ("C-<right>" right-word "one word right" :color red)

  ;; We can't use esc for command mode
  ("c" scimax-jupyter-command-mode/body "command mode")

  ("C-<return>" org-ctrl-c-ctrl-c "run cell" :color red)
  ("S-<return>" scimax-execute-and-next-block "run cell, select below" :color red)
  ("M-<return>" (scimax-execute-and-next-block t) "run cell, insert new" :color red)

  ("-" scimax-split-src-block"split cell")

  ("/" org-comment-dwim "toggle comment on current or selected lines" :color red))


(defun ob-ipython-edit-up ()
  "Move to previous line, unless at the top.
In that case first move to beginning of line, and then move to
previous cell."
  (interactive)
  (let ((first-code-line-p (save-excursion
			     (forward-line -1)
			     (beginning-of-line)
			     (looking-at "#\\+BEGIN_SRC"))))
    (cond
     ((and (bolp) first-code-line-p)
      (ignore-errors
	(catch 'block
	  (while (org-babel-previous-src-block)
	    (when (string= "ipython"
			   (org-element-property :language (org-element-context)))
	      (throw 'block t))))))
     (first-code-line-p
      (beginning-of-line))
     (t
      (previous-line)))))


(defun ob-ipython-edit-down ()
  "Move to next line, unless at the bottom.
In that case first move to beginning of line, and then move to
previous cell."
  (interactive)
  (let ((last-code-line-p (save-excursion
			    (forward-line 1)
			    (beginning-of-line)
			    (looking-at "#\\+END_SRC"))))
    (cond
     ((and (eolp) last-code-line-p)
      (ignore-errors
	(catch 'block
	  (while (org-babel-next-src-block)
	    (when (string= "ipython"
			   (org-element-property :language (org-element-context)))
	      (throw 'block t))))))
     (last-code-line-p
      (end-of-line))
     (t
      (next-line)))))


;; https://www.cheatography.com/weidadeyue/cheat-sheets/jupyter-notebook/
(defhydra scimax-jupyter-command-mode (:color blue :hint nil)
  "
         Navigate                 Execute                  Edit                    Misc
-----------------------------------------------------------------------------------------------------------
  _<up>_: previous cell   _C-<return>_: run cell        _a_: insert cell above    _l_: toggle line numbers
     _k_: previous cell   _S-<return>_: run cell/next   _b_: insert below         _o_: toggle result folding
_<down>_: next cell       _M-<return>_: run cell/next   _x_: cut cell            _ii_: interrupt kernel
     _j_: next cell       ^ ^                           _V_: copy above           _0_: restart kernel
     ^ ^                  ^ ^                           _v_: copy below     _<SPC>_: scroll down
     ^ ^                  ^ ^                          _dd_: delete cell  _S-<SPC>_: scroll up
     ^ ^                  ^ ^                           _M_: merge next
Convert
------------------------------------------------------------------
_y_: to code
_m_: to markdown
_r_: to org
markdown headings _1_: _2_: _3_: _4_: _5_: _6_:

_s_: save buffer  _z_: undo _<return>_: edit mode
"
  ("<return>" scimax-jupyter-edit-mode/body "Enter edit mode")
  ("C-<return>" org-ctrl-c-ctrl-c "run cell" :color red)
  ("S-<return>" scimax-execute-and-next-block "run cell, select next" :color red)
  ("M-<return>" (scimax-execute-and-next-block t) "run cell, insert new" :color red)

  ;; These don't really have great analogs in org-mode, but maybe it makes sense
  ;; to be able to do this.
  ("y" (ob-ipython-convert-block-to "ipython") "to code")
  ("m" (ob-ipython-convert-block-to "markdown") "to markdown")
  ("r" (ob-ipython-convert-block-to "org") "to raw")

  ;; These change to markdown block and trim blank lines off the top and add #
  ;; to beginning
  ("1" (ob-ipython-markdown-headings 1) "to heading 1")
  ("2" (ob-ipython-markdown-headings 2) "to heading 2")
  ("3" (ob-ipython-markdown-headings 3) "to heading 3")
  ("4" (ob-ipython-markdown-headings 4) "to heading 4")
  ("5" (ob-ipython-markdown-headings 5) "to heading 5")
  ("6" (ob-ipython-markdown-headings 6) "to heading 6")

  ;; navigation
  ("<up>" org-babel-previous-src-block "select cell above" :color red)
  ("k" org-babel-previous-src-block "select cell above" :color red)
  ("<down>" org-babel-next-src-block "select cell below" :colr red)
  ("j" org-babel-next-src-block "select cell below" :colr red)

  ("a" scimax-insert-src-block "insert cell above")
  ("b" (scimax-insert-src-block t) "insert cell below")

  ("x" scimax-ob-kill-block-and-results "cut cell")
  ("V" (scimax-ob-clone-block t) "paste cell above")
  ("v" scimax-ob-clone-block "paste cell below")
  ("z" undo "undo last cell deletion" :color red)

  ("dd" scimax-ob-kill-block-and-results "delete cell")

  ;; need a new function to select region from point to next one.
  ("M" ob-ipython-merge-next-cell "merge cell below")

  ;; I am not sure we can do this with a kernel
  ;; ("C-s" "save and checkpoint")
  ("s" save-buffer "Save buffer")

  ("l" scimax-ob-toggle-line-numbers "toggle line numbers" :color red)
  ;; this folds output
  ("o" ob-ipython-toggle-output "toggle output" :color red)

  ;; for large ouputs, puts results in a window you can scroll in. Not sure if
  ;; that is possible in emacs. May be no analog.
  ;; ( ;; "S-o" "toggle output scrolling"
  ;;  )
  ;; Maybe no analog?
  ;; ("esc" "close pager")

  ;; ("h" "Show keyboard help")
  ("ii" ob-ipython-interrupt-kernel "Interrupt kernel")
  ;; 00 is not a good hydra command
  ("0" (when (y-or-n-p "Restart kernel? ")
	 (call-interactively 'ob-ipython-kill-kernel)) "restart kernel")
  ;; Emacs has the opposite scroll convention as a browser
  ("<SPC>" scroll-up-command "scroll down" :color red)
  ("S-<SPC>" scroll-down-command "scroll up" :color red))


;; * A context menu

(define-prefix-command 'scimax-ob-ipython-mode-map)


(easy-menu-define ob-ipython-menu scimax-ob-ipython-mode-map "ob-ipython menu"
  ob-ipython-menu-items)


(defun ob-ipython-org-menu ()
  "Add the ob-ipython menu to the Org menu."
  (easy-menu-change '("Org") "ob-ipython" ob-ipython-menu-items "Show/Hide")
  (easy-menu-change '("Org") "--" nil "Show/Hide"))


(add-hook 'org-mode-hook 'ob-ipython-org-menu)


(defun scimax-ob-ipython-popup-command (event)
  "Popup a menu of actions for src blocks."
  (interactive "e")
  (popup-menu (append '("ob-ipython") ob-ipython-menu-items)))

;; * Execution functions

(defun scimax-ob-ipython-default-session ()
  "Return the default name of the session for a src block."
  (concat
   ;; this is the block language
   (car (org-babel-get-src-block-info t))
   "-"
   (if-let (bf (buffer-file-name))
       (md5 (expand-file-name bf))
     "scratch")))


(defun scimax-ob-ipython-restart-kernel-execute-block ()
  "Restart kernel and execute block."
  (interactive)
  (ob-ipython-kill-kernel
   (cdr (assoc (scimax-ob-ipython-default-session )
	       (ob-ipython--get-kernel-processes))))
  (org-babel-execute-src-block-maybe))


(defun scimax-ob-ipython-restart-kernel-execute-buffer ()
  "Restart kernel and execute buffer."
  (interactive)
  (ob-ipython-kill-kernel
   (cdr (assoc (scimax-ob-ipython-default-session)
	       (ob-ipython--get-kernel-processes))))
  (org-babel-execute-buffer))


(defun scimax-restart-ipython-and-execute-to-point ()
  "Kill the kernel and run src-blocks to point."
  (interactive)
  (call-interactively 'ob-ipython-kill-kernel)
  (scimax-execute-to-point))


(defun scimax-ob-ipython-kill-kernel ()
  "Kill the active kernel."
  (interactive)
  (when (and (not (s-contains? "*temp*" (buffer-name))) (y-or-n-p "Kill kernel? "))
    (ob-ipython-kill-kernel
     (cdr (assoc (scimax-ob-ipython-default-session)
		 (ob-ipython--get-kernel-processes))))
    (setq header-line-format nil)
    (redisplay)
    ;; clean up some buffers
    (loop for buf in (list
		      "ob-ipython-out*" "*ob-ipython-debug*"
		      "*ob-ipython-kernel-ipython*"
		      (format "*ob-ipython-kernel-%s*" (scimax-ob-ipython-default-session))
		      (format "*Python:%s" (scimax-ob-ipython-default-session)))
	  do
	  (when (get-buffer buf)
	    (kill-buffer buf)))))



;; * block editing functions
(defun scimax-merge-ipython-blocks (r1 r2)
  "Merge blocks in the current region (R1 R2).
This deletes the results from each block, and concatenates the
code into a single block in the position of the first block.
Currently no switches/parameters are preserved. It isn't clear
what the right thing to do for those is, e.g. dealing with
variables, etc."
  (interactive "r")
  ;; Expand the region to encompass the src blocks that the points might be in.
  (let* ((R1 (save-excursion
	       (goto-char r1)
	       (if (org-in-src-block-p)
		   (org-element-property :begin (org-element-context))
		 r1)))
	 (R2 (save-excursion
	       (goto-char r2)
	       (if (org-in-src-block-p)
		   (org-element-property :end (org-element-context))
		 r2))))
    (save-restriction
      (narrow-to-region R1 R2)
      (let* ((blocks (org-element-map (org-element-parse-buffer) 'src-block
		       (lambda (src)
			 (when (string= "ipython" (org-element-property :language src))
			   src))))
	     (first-start (org-element-property :begin (car blocks)))
	     (merged-code (s-join "\n" (loop for src in blocks
					     collect
					     (org-element-property :value src)))))
	;; Remove blocks
	(loop for src in (reverse blocks)
	      do
	      (goto-char (org-element-property :begin src))
	      (org-babel-remove-result)
	      (setf (buffer-substring (org-element-property :begin src)
				      (org-element-property :end src))
		    ""))
	;; Now create the new big block.
	(goto-char first-start)
	(insert (format "#+BEGIN_SRC ipython
%s
#+END_SRC\n\n" (s-trim merged-code)))))))



;; * Modifications of ob-ipython

;;  I frequently get an error on startup that seems to be related to how long
;;  jupyter takes to start up. Usually, I just run the cell again and it works.
;;  This modification is designed to wait just long enough for the json file to
;;  get created. This seems to fix the issue. It used to wait just 1 second, but
;;  sometimes it takes up to two seconds to create this file (it is used in
;;  driver.py I think).
(defcustom scimax-create-kernel-max-wait 5
  "Maximum seconds to wait before kernel program starts."
  :group 'ob-ipython)

(defun ob-ipython--create-kernel (name &optional kernel)
  (when (and (not (ignore-errors (process-live-p (get-process (format "kernel-%s" name)))))
             (not (s-ends-with-p ".json" name)))
    (ob-ipython--create-process
     (format "kernel-%s" name)
     (append
      (list ob-ipython-command "console" "--simple-prompt")
      (list "-f" (ob-ipython--kernel-file name))
      (if kernel (list "--kernel" kernel) '())
      ;;should be last in the list of args
      ob-ipython-kernel-extra-args))
    (let ((i 0)
	  (t0 (float-time))
    	  (tincrement 0.1)
    	  (cfile (expand-file-name
    		  (ob-ipython--kernel-file name)
    		  (s-trim (shell-command-to-string "jupyter --runtime-dir")))))
      (while (and (not (file-exists-p cfile))
		  (< (- (float-time) t0) scimax-create-kernel-max-wait))
	(sleep-for tincrement))
      (message "Kernel started in %1.2f seconds" (- (float-time) t0))
      (setq header-line-format name))))


(defun ob-ipython-kill-kernel (proc)
  "Kill a kernel process.
If you then re-evaluate a source block a new kernel will be started."
  (interactive (ob-ipython--choose-kernel))
  (when proc
    (let* ((proc-name (process-name proc))
	   (proc-buffer (format "*ob-ipython-%s*" proc-name))
	   (cfile (expand-file-name
		   (format "%s.json" (s-replace "kernel-" "emacs-" proc-name))
		   (s-trim (shell-command-to-string "jupyter --runtime-dir")))))
      (when (f-exists? cfile)
	(f-delete cfile))
      (delete-process proc)
      (kill-buffer (process-buffer proc))
      (setq header-line-format nil)
      (message (format "Killed %s and deleted %s" proc-name cfile)))))


;; Modified to make buffer unique kernels automatically
(defun org-babel-execute:ipython (body params)
  "Execute a block of IPython code with Babel.
This function is called by `org-babel-execute-src-block'."

  ;; make sure we get prompted to kill the kernel when exiting.
  (when ob-ipython-kill-kernel-on-exit
    (add-hook 'kill-buffer-hook 'scimax-ob-ipython-kill-kernel nil t))

  (when (and
	 ;; if these are equal, we use default, if not user defined session
	 ;; unless they just used :session
	 (not (null (cdr (assoc :session
				(third (org-babel-get-src-block-info t))))))
	 (eq (assoc :session org-babel-default-header-args:ipython)
	     (assoc :session (third (org-babel-get-src-block-info t))))
	 ;; we want unique kernels
	 ob-ipython-buffer-unique-kernel)
    (make-local-variable 'org-babel-default-header-args:ipython)

    ;; remove the old session info
    (setq org-babel-default-header-args:ipython
	  (remove (assoc :session org-babel-default-header-args:ipython)
		  org-babel-default-header-args:ipython))

    ;; add the new session info
    (let ((session-name (scimax-ob-ipython-default-session)))
      (add-to-list 'org-babel-default-header-args:ipython
		   (cons :session session-name))
      (setf (cdr (assoc :session params)) session-name)))

  (ob-ipython--clear-output-buffer)

  ;; delete any figures that will be replaced and clear results here.
  (when ob-ipython-delete-stale-images
    (let ((result-string (let ((location (org-babel-where-is-src-block-result)))
			   (when location
			     (save-excursion
			       (goto-char location)
			       (when (looking-at (concat org-babel-result-regexp ".*$"))
				 (buffer-substring-no-properties
				  (save-excursion
				    (skip-chars-backward " \r\t\n")
				    (line-beginning-position 2))
				  (progn (forward-line) (org-babel-result-end))))))))
	  (files '())
	  ;; This matches automatic file generation
	  (fregex "\\[\\[file:\\(obipy-resources/.*\\)\\]\\]"))
      (when result-string
	(with-temp-buffer
	  (insert result-string)
	  (goto-char (point-min))
	  (while (re-search-forward fregex nil t)
	    (push (match-string 1) files)))
	(mapc (lambda (f)
		(when (f-exists? f)
		  (f-delete f)))
	      files))))

  (org-babel-remove-result nil t)

  ;; scimax feature to restart
  (when (assoc :restart params)
    (let ((session (cdr (assoc :session (third (org-babel-get-src-block-info))))))
      (ob-ipython-kill-kernel
       (cdr (assoc session
		   (ob-ipython--get-kernel-processes))))
      (cl-loop for buf in (list (format "*Python:%s*" session)
				(format "*ob-ipython-kernel-%s*" session))
	       do
	       (when (get-buffer buf)
		 (kill-buffer buf)))))
  ;; I think this returns the results that get inserted by
  ;; `org-babel-execute-src-block'. If there is an exec-dir, we wrap this block
  ;; to temporarily change to that directory.
  (let* ((exec-dir (cdr (assoc :dir params)))
         (exec-body (concat
                     (when exec-dir
                       (concat "from os import chdir as __ob_ipy_chdir; "
			       "from os import getcwd as __ob_ipy_getcwd; "
			       "__ob_ipy_cwd = __ob_ipy_getcwd(); "
			       " __ob_ipy_chdir(\""
			       exec-dir
			       "\")\n"))
                     body
		     (when exec-dir
		       "\n__ob_ipy_chdir(__ob_ipy_cwd)"))))
    ;; Check if we are debugging
    (if (string-match "^%pdb" exec-body)
	(progn
	  (pop-to-buffer (org-babel-initiate-session))
	  (comint-send-string (get-buffer-process (current-buffer)) body)
	  (comint-send-input))
      ;; not debugging
      (if (assoc :async params)
	  (ob-ipython--execute-async exec-body params)
	(ob-ipython--execute-sync exec-body params)))))


;; ** Fine tune the output of blocks
;; It was necessary to redefine these to get selective outputs via :display

(defun ob-ipython--execute-async (body params)
  "Execute asynchronously."
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
         (result-type (cdr (assoc :result-type params)))
         (sentinel (ipython--async-gen-sentinel))
	 ;; I added this. It is like the command in jupyter, but unfortunately
	 ;; similar to :display in the results from jupyter. This is to specify
	 ;; what you want to see.
	 (display-params (cdr (assoc :display params)))
	 (display (when display-params (mapcar 'intern-soft
					       (s-split " " display-params t)))))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (ob-ipython--execute-request-async
     (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                    params (org-babel-variable-assignments:python params))
     (ob-ipython--normalize-session session)

     `(lambda (ret sentinel buffer file result-type)
	(when ,display-params
	  (setf (cdr (assoc :display (assoc :result ret)))
		(-filter (lambda (el) (memq (car el) ',display))
			 (cdr (assoc :display (assoc :result ret)))))
	  (setf (cdr (assoc :value (assoc :result ret)))
		(-filter (lambda (el) (memq (car el) ',display))
			 (cdr (assoc :value (assoc :result ret))))))
	(save-window-excursion
	  (save-excursion
	    (save-restriction
	      (with-current-buffer buffer
		(goto-char (point-min))
		(re-search-forward sentinel)
		(re-search-backward "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")
		(org-babel-remove-result)
		(org-babel-insert-result
		 (ob-ipython--process-response ret file result-type)
		 (cdr (assoc :result-params (nth 2 (org-babel-get-src-block-info)))))
		(org-redisplay-inline-images))))))

     (list sentinel (current-buffer) file result-type))
    (format "%s - %s <output> <interrupt>" (length ob-ipython--async-queue) sentinel)))


(defun ob-ipython--execute-sync (body params)
  "Execute BODY with PARAMS synchronously."
  (let* ((file (cdr (assoc :ipyfile params)))
         (session (cdr (assoc :session params)))
	 (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
	 ;; I added this. It is like the command in jupyter, but unfortunately
	 ;; similar to :display in the results from jupyter. This is to specify
	 ;; what you want to see.
	 (display-params (cdr (assoc :display params)))
	 (display (when display-params (mapcar 'intern-soft
					       (s-split " " display-params t)))))
    (ob-ipython--create-kernel (ob-ipython--normalize-session session)
                               (cdr (assoc :kernel params)))
    (-when-let (ret (ob-ipython--eval
                     (ob-ipython--execute-request
                      (org-babel-expand-body:generic
		       (encode-coding-string body 'utf-8)
		       params (org-babel-variable-assignments:python params))
                      (ob-ipython--normalize-session session))))
      ;; Now I want to filter out things not in the display we want. Default is everything.
      (when display-params
	(setf (cdr (assoc :display (assoc :result ret)))
	      (-filter (lambda (el) (memq (car el) display))
		       (cdr (assoc :display (assoc :result ret)))))
	(setf (cdr (assoc :value (assoc :result ret)))
	      (-filter (lambda (el) (memq (car el) display))
		       (cdr (assoc :value (assoc :result ret))))))
      (let ((*ob-ipython-output-results-prefix* (if (-contains? result-params "raw") "" ": ")))
	(ob-ipython--process-response ret file result-type)))))


;; This gives me the output I want. Note I changed this to process one result at
;; a time instead of passing all the results to `ob-ipython--render.
(defun ob-ipython--process-response (ret file result-type)
  (let* ((result (cdr (assoc :result ret)))
	 (output (cdr (assoc :output ret)))
	 (value (cdr (assoc :value result)))
	 (display (cdr (assoc :display result))))

    ;; check for data to show.
    (save-excursion
      (when (cdr (assoc :data ret))
	(pop-to-buffer "*ob-ipython-data*")
	(read-only-mode -1)
	(erase-buffer)
	(insert (cdr (assoc :data ret)))
	(goto-char (point-min))
	(ansi-color-apply-on-region (point-min) (point-max))
	(special-mode)))

    (if (eq 'inline-src-block (car (org-element-context)))
	(cdr (assoc 'text/plain value))
      (s-concat
       (if ob-ipython-suppress-execution-count
	   ""
	 (format "# Out[%d]:\n" (cdr (assoc :exec-count ret))))
       (when (and (not (string= "" output)) ob-ipython-show-mime-types) "# output\n")
       (funcall (cdr (assoc 'output ob-ipython-mime-formatters)) nil output)
       ;; I process the outputs one at a time here.
       (s-join "\n\n" (loop for (type . value) in (append value display)
			    collect
			    (ob-ipython--render file (list (cons type value)))))))))


;; ** Formatters for output
(defvar *ob-ipython-output-results-prefix* ": "
  "Prefix string for output.
The default is to put a colon in front, making the results verbatim.")


(defun ob-ipython-format-output (file-or-nil output)
  "Format OUTPUT as a result.
This adds : to the beginning so the output will export as
verbatim text. FILE-OR-NIL is not used, and is here for
compatibility with the other formatters."
  (when (not (string= "" output))
    (let (*ob-ipython-output-results-prefix*)
      (when (-contains?
	     (s-split " " (cdr (assoc :results (caddr (org-babel-get-src-block-info t))))) "code")
	(setq *ob-ipython-output-results-prefix* ""))
      (concat (s-join "\n"
		      (mapcar (lambda (s)
				(s-concat *ob-ipython-output-results-prefix* s))
			      (s-split "\n" output)))
	      "\n"))))


(defun ob-ipython-format-text/plain (file-or-nil value)
  "Format VALUE for text/plain mime-types.
FILE-OR-NIL is not used in this function."
  (let ((lines (s-lines value))
	(raw (-contains?
	      (s-split " " (cdr (assoc :results (caddr (org-babel-get-src-block-info t))))) "raw")))
    ;; filter out uninteresting lines.
    (setq lines (-filter (lambda (line)
			   (not (-any (lambda (regex)
					(s-matches? regex line))
				      ob-ipython-plain-text-filter-regexps)))
			 lines))
    (when lines
      ;; Add verbatim start string
      (setq lines (mapcar (lambda (s) (s-concat
				       (if raw "" ": ")
				       s))
			  lines))
      (when ob-ipython-show-mime-types
	(setq lines (append '("# text/plain") lines)))
      (s-join "\n" lines))))


(defun ob-ipython-format-text/html (file-or-nil value)
  "Format VALUE for text/html mime-types.
FILE-OR-NIL is not used in this function."
  (s-join "\n"
	  (list (if ob-ipython-show-mime-types "# text/html" "")
		(format "#+BEGIN_EXPORT html\n%s\n#+END_EXPORT" value))))


(defun ob-ipython-format-text/latex (file-or-nil value)
  "Format VALUE for text/latex mime-types.
FILE-OR-NIL is not used in this function."
  (s-join "\n"
	  (list (if ob-ipython-show-mime-types "# text/latex" "")
		(format "#+BEGIN_EXPORT latex\n%s\n#+END_EXPORT" value))))


(defun ob-ipython-format-text/org (file-or-nil value)
  "Format VALUE for text/org mime-types.
FILE-OR-NIL is not used in this function."
  (s-join "\n" (list (if ob-ipython-show-mime-types "# text/org" "")
		     value)))


(defun ob-ipython--generate-file-name (suffix)
  "Generate a file name to store an image in.
I added an md5-hash of the buffer name so you can tell what file
the names belong to. This is useful later to delete files that
are no longer used."
  (s-concat (make-temp-name
	     (concat (f-join ob-ipython-resources-dir (if-let (bf (buffer-file-name))
							  (md5 (expand-file-name bf))
							"scratch"))
		     "-"))
	    suffix))


(defun ob-ipython-format-image/png (file-or-nil value)
  "Format VALUE for image/png mime-types.
FILE-OR-NIL if non-nil is the file to save the image in. If nil,
a filename is generated."
  (let ((file (or file-or-nil (ob-ipython--generate-file-name ".png"))))
    (ob-ipython--write-base64-string file value)
    (s-join "\n" (list
		  (if ob-ipython-show-mime-types "# image/png" "")
		  (format "[[file:%s]]" file)))))


(defun ob-ipython--write-base64-string (file b64-string)
  "Write to FILE the image in B64-STRING.
Note: the original version of this would sometimes hang, so I
rewrote this."
  (if b64-string
      (progn
	(unless (file-directory-p (file-name-directory file))
	  (make-directory (file-name-directory file) t))
	(with-temp-file file
	  (insert (base64-decode-string b64-string))))
    (error "No output was produced to write to a file.")))


(defun ob-ipython-format-image/svg+xml (file-or-nil value)
  "Format VALUE for image/svg+xml mime-types.
FILE-OR-NIL if non-nil is the file to save the image in. If nil,
a filename is generated."
  (let ((file (or file-or-nil (ob-ipython--generate-file-name ".svg"))))
    (ob-ipython--write-string-to-file file value)
    (s-join "\n"
	    (list
	     (if ob-ipython-show-mime-types "# image/svg" "")
	     (format "[[file:%s]]" file)))))


(defun ob-ipython-format-application/javascript (file-or-nil value)
  "Format VALUE for application/javascript mime-types.
FILE-OR-NIL is not used in this function."
  (format "%s#+BEGIN_SRC javascript\n%s\n#+END_SRC"
	  (if ob-ipython-show-mime-types "# application/javascript\n" "")
	  value))


(defun ob-ipython-format-default (file-or-nil value)
  "Default formatter to format VALUE.
This is used for mime-types that don't have a formatter already
defined. FILE-OR-NIL is not used in this function."
  (format "%s%s" (if ob-ipython-show-mime-types
		     (format "\n# %s\n: " (caar values))
		   ": ")
	  (cdar values)))


(defun ob-ipython--render (file-or-nil values)
  "VALUES is a list of (mime-type . value).
FILE-OR-NIL comes from a :ipyfile header value or is nil. It is
used for saving graphic outputs to files of your choice. It
doesn't make sense to me, since you can only save one file this
way, but I have left it in for compatibility."
  (let* ((mime-type (caar values))
	 (format-func (cdr (assoc mime-type ob-ipython-mime-formatters))))
    (if format-func
	(funcall format-func file-or-nil (cdar values))
      ;; fall-through
      (funcall
       (cdr (assoc 'default ob-ipython-mime-formatters))
       (cdar values)))))


;; ** Better exceptions

(defun ob-ipython--extract-data (msgs)
  "This extracts output from func? or func?? in ipython"
  (->> msgs
       (-filter (lambda (msg)
		  (s-equals? "execute_reply"
			     (cdr (assoc 'msg_type msg)))))
       (-mapcat (lambda (msg)
		  (->> msg (assoc 'content) (assoc 'payload) cadr (assoc 'data) cdadr)))))

;; I want an option to get exceptions in the buffer
(defun ob-ipython--eval (service-response)
  (let ((status (ob-ipython--extract-status service-response)))
    (cond ((string= "ok" status)
	   `((:result . ,(ob-ipython--extract-result service-response))
	     (:output . ,(ob-ipython--extract-output service-response))
	     (:data . ,(ob-ipython--extract-data service-response))
	     (:exec-count . ,(ob-ipython--extract-execution-count service-response))))
          ((string= "abort" status) (error "Kernel execution aborted"))
          ((string= "error" status)
	   (if ob-ipython-exception-results
	       (let ((error-content
		      (->> service-response
			   (-filter (lambda (msg) (-contains? '("execute_reply" "inspect_reply")
							      (cdr (assoc 'msg_type msg)))))
			   car
			   (assoc 'content)
			   cdr)))
		 `((:result . ,(ob-ipython--extract-result service-response))
		   (:output . ,(org-no-properties
				(ansi-color-apply
				 (s-join "\n" (cdr (assoc 'traceback error-content))))))
		   (:exec-count . ,(ob-ipython--extract-execution-count service-response))))
	     (error (ob-ipython--extract-error service-response)))))))


;; I also want q to go to the offending line from a traceback buffer
(defun ob-ipython--create-traceback-buffer (traceback)
  "Create a traceback buffer.
Note, this does not work if you run the block async."
  (let ((current-buffer (current-buffer))
	(src (org-element-context))
	(buf (get-buffer-create "*ob-ipython-traceback*")))
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (-each traceback
          (lambda (line) (insert (format "%s\n" line))))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (pop-to-buffer buf)
    (let ((line (re-search-backward "-*> *\\([0-9]*\\) " nil t))
	  line-number)
      (when line
	(setq line-number (string-to-number (match-string 1)))
	(local-set-key "q" `(lambda ()
			      (interactive)
			      (quit-restore-window nil 'bury)
			      (pop-to-buffer ,current-buffer)
			      (goto-char ,(org-element-property :begin src))
			      (forward-line ,line-number)))))))


;; ** inspect from an org buffer
;; This makes inspect work from an org-buffer.

(defun ob-ipython-inspect (buffer pos)
  "Ask a kernel for documentation on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  ;; It is probably helpful to be at the end of a symbol, otherwise you may get
  ;; help on something else.
  (save-excursion
    (when (not (looking-back "\\_>" (line-beginning-position)))
      (forward-symbol 1)
      (setq pos (point))))

  (let ((return (org-in-src-block-p))
	(inspect-buffer))
    (when return
      (org-edit-src-code nil "*ob-ipython-src-edit-inspect*"))
    (let ((code (with-current-buffer buffer
		  (buffer-substring-no-properties (point-min) (point-max)))))
      (-if-let (result (->> (ob-ipython--inspect code pos)
			    (assoc 'text/plain)
			    cdr))
	  (setq inspect-buffer (ob-ipython--create-inspect-buffer result))))

    (when return
      (with-current-buffer "*ob-ipython-src-edit-inspect*"
	(org-edit-src-exit)))
    (when inspect-buffer
      (pop-to-buffer inspect-buffer)
      (goto-char (point-min)))))



;; * eldoc integration

;; This may not be the speediest way to do this, since it runs the
;; ob-ipython-inspect function.
(defun scimax-ob-ipython-signature ()
  "Try to return a function signature for the thing at point."
  (when (and (eql major-mode 'org-mode)
	     (string= (or (get-text-property (point) 'lang) "") "ipython"))
    (save-window-excursion
      (ob-ipython-inspect (current-buffer) (point))
      (when (get-buffer "*ob-ipython-inspect*")
	(with-current-buffer "*ob-ipython-inspect*"
	  (goto-char (point-min))
	  (prog1
	      (cond
	       ((re-search-forward "Signature:" nil t 1)
		(buffer-substring (line-beginning-position) (line-end-position)))
	       ((re-search-forward "Docstring:" nil t 1)
		(forward-line)
		(buffer-substring (line-beginning-position) (line-end-position)))
	       (t
		nil))
	    ;; get rid of this so we don't accidently show old results later
	    (with-current-buffer "*ob-ipython-inspect*"
	      (toggle-read-only)
	      (erase-buffer))))))))


;; The org-eldoc-documentation-function has hard-coded language options, with no
;; obvious way to hook into it for this application. So, I am just advising the
;; function to check for ipython blocks, and run the original function if not in
;; a block.
(defun scimax-ob-ipython-eldoc-advice (orig-func &rest args)
  "Advice function to get eldoc signatures in blocks in org-mode."
  (or (scimax-ob-ipython-signature) (apply orig-func args)))


(defun scimax-ob-ipython-turn-on-eldoc ()
  "Turn on eldoc signatures."
  (interactive)
  (advice-add 'org-eldoc-documentation-function :around #'scimax-ob-ipython-eldoc-advice))


(defun scimax-ob-ipython-turn-off-eldoc ()
  "Turn off eldoc signatures."
  (interactive)
  (advice-remove 'org-eldoc-documentation-function  #'scimax-ob-ipython-eldoc-advice))


(when ob-ipython-eldoc-integration
  (scimax-ob-ipython-turn-on-eldoc))

;; * Completion

;; This makes this function work from an org-buffer.
(defun ob-ipython-completions (buffer pos)
  "Ask a kernel for completions on the thing at POS in BUFFER."
  (interactive (list (current-buffer) (point)))
  (let ((return (org-in-src-block-p))
	completion-buffer)
    (when return
      (org-edit-src-code nil "*ob-ipython-src-edit-completion*"))

    (prog1
	(let* ((code (with-current-buffer buffer
                       (buffer-substring-no-properties (point-min) (point-max))))
               (resp (ob-ipython--complete-request code pos))
               (status (ob-ipython--extract-status resp)))
	  (if (not (string= "ok" status))
              '()
	    (->> resp
		 (-filter (lambda (msg)
			    (-contains? '("complete_reply")
					(cdr (assoc 'msg_type msg)))))
		 (-mapcat (lambda (msg)
			    (->> msg
				 (assoc 'content)
				 cdr))))))
      (when return
	(with-current-buffer "*ob-ipython-src-edit-completion*"
	  (org-edit-src-exit))))))

;; Adapted to enable in org-buffers. Note, to enable this, you have to add
;; (add-to-list 'company-backends 'company-ob-ipython) to an init file. There
;; are also reports that it is slow.

(defun company-ob-ipython (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ob-ipython))
    (prefix (and (or ob-ipython-mode (string= (or (get-text-property (point) 'lang) "") "ipython"))
                 (let ((res (ob-ipython-completions (current-buffer) (1- (point)))))
                   (substring-no-properties (buffer-string)
                                            (cdr (assoc 'cursor_start res))
                                            (cdr (assoc 'cursor_end res))))))
    (candidates (cons :async (lambda (cb)
                               (let ((res (ob-ipython-completions
                                           (current-buffer) (1- (point)))))
                                 (funcall cb (-uniq (cdr (assoc 'matches res))))))))
    (sorted t)
    (doc-buffer (ob-ipython--company-doc-buffer
                 (cdr (assoc 'text/plain (ob-ipython--inspect arg (length arg))))))))


(defun scimax-ob-ipython-complete-ivy ()
  "Use ivy to complete the thing at point."
  (interactive)
  (let* ((result (ob-ipython-completions (current-buffer) (1- (point))))
	 (candidates (-uniq (cdr (assoc 'matches result))))
	 (beg (1+ (cdr (assoc 'cursor_start result))))
	 (end (1+ (cdr (assoc 'cursor_end result)))))
    (ivy-read "Complete: " candidates
	      :action (lambda (candidate)
			(with-ivy-window
			  (setf (buffer-substring beg end) candidate)
			  (forward-char (length candidate)))))))


;; * clickable text buttons

;; This is an experiment to provide clickable buttons. The idea is you put them
;; in a comment line in the block and you can click on them.

(defun ob-ipython-button-font-lock (pattern function help-echo)
  "Creates the font lock function for buttons."
  `(lambda (limit)
     (while (re-search-forward ,pattern limit t)
       (let ((start (match-beginning 0))
	     (end (match-end 0))
	     (map (make-sparse-keymap)))
	 (define-key map [mouse-1] (lambda ()
				     (interactive)
				     (call-interactively ',function)))
	 (define-key map (kbd "<return>")
	   (lambda ()
	     (interactive)

	     (funcall ',function (org-mouse-down-mouse nil))))
	 (add-text-properties start end
			      (list 'face 'link
				    'mouse-face 'highlight
				    'help-echo ,help-echo
				    'local-map map
				    'font-lock-fontified t)))
       t)))

(defun ob-ipython-activate-buttons ()
  "Activate buttons."
  (loop for (text cmd help-echo) in ob-ipython-buttons
	do
	(font-lock-add-keywords
	 nil
	 `((,(ob-ipython-button-font-lock text cmd help-echo) (0  'link t)))
	 t)))

(add-hook 'org-mode-hook 'ob-ipython-activate-buttons t)



;; * redefine org-show-entry

;; This function closes drawers. I redefine it here to avoid that. Maybe we will
;; find a fix for it one day.

(defun org-show-entry ()
  "Show the body directly following this heading.
Show the heading too, if it is currently invisible."
  (interactive)
  (save-excursion
    (ignore-errors
      (org-back-to-heading t)
      (outline-flag-region
       (max (point-min) (1- (point)))
       (save-excursion
	 (if (re-search-forward
	      (concat "[\r\n]\\(" org-outline-regexp "\\)") nil t)
	     (match-beginning 1)
	   (point-max)))
       nil)
      ;; (org-cycle-hide-drawers 'children)
      )))

(provide 'scimax-org-babel-ipython-upstream)

;;; scimax-org-babel-ipython-upstream.el ends here
