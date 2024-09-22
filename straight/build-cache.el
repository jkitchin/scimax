
:tanat

"31.0.50"

#s(hash-table test equal data ("org-elpa" ("2024-09-21 16:29:16" nil (:local-repo nil :package "org-elpa" :type git)) "melpa" ("2024-09-21 16:29:16" nil (:type git :host github :repo "melpa/melpa" :build nil :package "melpa" :local-repo "melpa")) "gnu-elpa-mirror" ("2024-09-21 16:29:16" nil (:type git :host github :repo "emacs-straight/gnu-elpa-mirror" :build nil :package "gnu-elpa-mirror" :local-repo "gnu-elpa-mirror")) "nongnu-elpa" ("2024-09-21 16:29:16" nil (:type git :repo "https://git.savannah.gnu.org/git/emacs/nongnu.git" :depth (full single-branch) :local-repo "nongnu-elpa" :build nil :package "nongnu-elpa")) "el-get" ("2024-09-21 16:29:16" nil (:type git :host github :repo "dimitri/el-get" :build nil :files (:defaults "methods" ("recipes" "recipes/el-get.rcp") "el-get-pkg.el") :flavor melpa :package "el-get" :local-repo "el-get")) "emacsmirror-mirror" ("2024-09-21 16:29:16" nil (:type git :host github :repo "emacs-straight/emacsmirror-mirror" :build nil :package "emacsmirror-mirror" :local-repo "emacsmirror-mirror")) "straight" ("2024-09-21 16:29:16" ("emacs") (:type git :host github :repo "radian-software/straight.el" :files ("straight*.el") :branch "master" :package "straight" :local-repo "straight.el")) "pretty-hydra" ("2024-09-21 16:29:18" ("hydra" "s" "dash" "emacs" "compat") (:type git :flavor melpa :files ("pretty-hydra.el" "pretty-hydra-pkg.el") :host github :repo "jerrypnz/major-mode-hydra.el" :package "pretty-hydra" :local-repo "major-mode-hydra.el")) "hydra" ("2024-09-21 16:29:18" ("cl-lib" "lv") (:type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra" :package "hydra" :local-repo "hydra")) "lv" ("2024-09-21 16:29:18" nil (:flavor melpa :files ("lv.el" "lv-pkg.el") :package "lv" :local-repo "hydra" :type git :repo "abo-abo/hydra" :host github)) "s" ("2024-09-21 16:29:18" nil (:type git :flavor melpa :host github :repo "magnars/s.el" :package "s" :local-repo "s.el")) "dash" ("2024-09-21 16:29:18" ("emacs") (:type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el" :package "dash" :local-repo "dash.el")) "compat" ("2024-09-21 16:29:18" ("emacs" "seq") (:type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git")) :package "compat" :local-repo "compat")) "seq" ("2024-09-21 16:29:18" nil (:type git :host github :repo "emacs-straight/seq" :files ("*" (:exclude ".git")) :package "seq" :local-repo "seq")) "citeproc" ("2024-09-21 16:29:19" ("emacs" "dash" "s" "f" "queue" "string-inflection" "org" "parsebib" "compat") (:type git :flavor melpa :host github :repo "andras-simonyi/citeproc-el" :package "citeproc" :local-repo "citeproc-el")) "f" ("2024-09-21 16:29:19" ("emacs" "s" "dash") (:type git :flavor melpa :host github :repo "rejeep/f.el" :package "f" :local-repo "f.el")) "queue" ("2024-09-21 16:29:19" ("cl-lib") (:type git :host github :repo "emacs-straight/queue" :files ("*" (:exclude ".git")) :package "queue" :local-repo "queue")) "string-inflection" ("2024-09-21 16:29:19" nil (:type git :flavor melpa :host github :repo "akicho8/string-inflection" :package "string-inflection" :local-repo "string-inflection")) "parsebib" ("2024-09-21 16:29:19" ("emacs") (:type git :flavor melpa :host github :repo "joostkremers/parsebib" :package "parsebib" :local-repo "parsebib")) "org-ref" ("2024-09-21 16:29:19" ("org" "dash" "s" "f" "htmlize" "hydra" "avy" "parsebib" "bibtex-completion" "citeproc" "ox-pandoc" "request") (:type git :flavor melpa :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :host github :repo "jkitchin/org-ref" :package "org-ref" :local-repo "org-ref")) "htmlize" ("2024-09-21 16:29:19" ("emacs") (:type git :flavor melpa :host github :repo "hniksic/emacs-htmlize" :package "htmlize" :local-repo "emacs-htmlize")) "avy" ("2024-09-21 16:29:19" ("emacs" "cl-lib") (:type git :flavor melpa :host github :repo "abo-abo/avy" :package "avy" :local-repo "avy")) "bibtex-completion" ("2024-09-21 16:29:19" ("parsebib" "s" "dash" "f" "cl-lib" "biblio" "emacs") (:type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex" :package "bibtex-completion" :local-repo "helm-bibtex")) "biblio" ("2024-09-21 16:29:19" ("emacs" "biblio-core") (:type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el" :package "biblio" :local-repo "biblio.el")) "biblio-core" ("2024-09-21 16:29:19" ("emacs" "let-alist" "seq" "dash") (:flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :package "biblio-core" :local-repo "biblio.el" :type git :repo "cpitclaudel/biblio.el" :host github)) "let-alist" ("2024-09-21 16:29:19" ("emacs") (:type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git")) :package "let-alist" :local-repo "let-alist")) "ox-pandoc" ("2024-09-21 16:29:19" ("org" "emacs" "dash" "ht") (:type git :flavor melpa :host github :repo "emacsorphanage/ox-pandoc" :package "ox-pandoc" :local-repo "ox-pandoc")) "ht" ("2024-09-21 16:29:19" ("dash") (:type git :flavor melpa :host github :repo "Wilfred/ht.el" :package "ht" :local-repo "ht.el")) "request" ("2024-09-21 16:29:19" ("emacs") (:type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request" :package "request" :local-repo "emacs-request")) "rainbow-mode" ("2024-09-21 16:29:20" nil (:type git :host github :repo "emacs-straight/rainbow-mode" :files ("*" (:exclude ".git")) :package "rainbow-mode" :local-repo "rainbow-mode")) "undo-tree" ("2024-09-21 16:29:21" ("queue" "emacs") (:type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git")) :package "undo-tree" :local-repo "undo-tree")) "gnu-elpa-keyring-update" ("2024-09-21 16:29:16" nil (:type git :host github :repo "emacs-straight/gnu-elpa-keyring-update" :files ("*" (:exclude ".git")) :package "gnu-elpa-keyring-update" :local-repo "gnu-elpa-keyring-update"))))

#s(hash-table test equal data ("straight" ((straight-autoloads straight straight-x straight-ert-print-hack) (autoload 'straight-remove-unused-repos "straight" "Remove unused repositories from the repos and build directories.
A repo is considered \"unused\" if it was not explicitly requested via
`straight-use-package' during the current Emacs session.
If FORCE is non-nil do not prompt before deleting repos.

(fn &optional FORCE)" t) (autoload 'straight-get-recipe "straight" "Interactively select a recipe from one of the recipe repositories.
All recipe repositories in `straight-recipe-repositories' will
first be cloned. After the recipe is selected, it will be copied
to the kill ring. With a prefix argument, first prompt for a
recipe repository to search. Only that repository will be
cloned.

From Lisp code, SOURCES should be a subset of the symbols in
`straight-recipe-repositories'. Only those recipe repositories
are cloned and searched. If it is nil or omitted, then the value
of `straight-recipe-repositories' is used. If SOURCES is the
symbol `interactive', then the user is prompted to select a
recipe repository, and a list containing that recipe repository
is used for the value of SOURCES. ACTION may be `copy' (copy
recipe to the kill ring), `insert' (insert at point), or nil (no
action, just return it).

Optional arg FILTER must be a unary function.
It takes a package name as its sole argument.
If it returns nil the candidate is excluded.

(fn &optional SOURCES ACTION FILTER)" t) (autoload 'straight-visit-package-website "straight" "Visit the package RECIPE's website.

(fn RECIPE)" t) (autoload 'straight-visit-package "straight" "Open PACKAGE's local repository directory.
When BUILD is non-nil visit PACKAGE's build directory.

(fn PACKAGE &optional BUILD)" t) (autoload 'straight-use-package "straight" "Register, clone, build, and activate a package and its dependencies.
This is the main entry point to the functionality of straight.el.

MELPA-STYLE-RECIPE is either a symbol naming a package, or a list
whose car is a symbol naming a package and whose cdr is a
property list containing e.g. `:type', `:local-repo', `:files',
and VC backend specific keywords.

First, the package recipe is registered with straight.el. If
NO-CLONE is a function, then it is called with two arguments: the
package name as a string, and a boolean value indicating whether
the local repository for the package is available. In that case,
the return value of the function is used as the value of NO-CLONE
instead. In any case, if NO-CLONE is non-nil, then processing
stops here.

Otherwise, the repository is cloned, if it is missing. If
NO-BUILD is a function, then it is called with one argument: the
package name as a string. In that case, the return value of the
function is used as the value of NO-BUILD instead. In any case,
if NO-BUILD is non-nil, then processing halts here. Otherwise,
the package is built and activated. Note that if the package
recipe has a nil `:build' entry, then NO-BUILD is ignored
and processing always stops before building and activation
occurs.

CAUSE is a string explaining the reason why
`straight-use-package' has been called. It is for internal use
only, and is used to construct progress messages. INTERACTIVE is
non-nil if the function has been called interactively. It is for
internal use only, and is used to determine whether to show a
hint about how to install the package permanently.

Return non-nil when package is initially installed, nil otherwise.

(fn MELPA-STYLE-RECIPE &optional NO-CLONE NO-BUILD CAUSE INTERACTIVE)" t) (autoload 'straight-register-package "straight" "Register a package without cloning, building, or activating it.
This function is equivalent to calling `straight-use-package'
with a non-nil argument for NO-CLONE. It is provided for
convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-no-build "straight" "Register and clone a package without building it.
This function is equivalent to calling `straight-use-package'
with nil for NO-CLONE but a non-nil argument for NO-BUILD. It is
provided for convenience. MELPA-STYLE-RECIPE is as for
`straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-package-lazy "straight" "Register, build, and activate a package if it is already cloned.
This function is equivalent to calling `straight-use-package'
with symbol `lazy' for NO-CLONE. It is provided for convenience.
MELPA-STYLE-RECIPE is as for `straight-use-package'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-use-recipes "straight" "Register a recipe repository using MELPA-STYLE-RECIPE.
This registers the recipe and builds it if it is already cloned.
Note that you probably want the recipe for a recipe repository to
include a nil `:build' property, to unconditionally
inhibit the build phase.

This function also adds the recipe repository to
`straight-recipe-repositories', at the end of the list.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-override-recipe "straight" "Register MELPA-STYLE-RECIPE as a recipe override.
This puts it in `straight-recipe-overrides', depending on the
value of `straight-current-profile'.

(fn MELPA-STYLE-RECIPE)") (autoload 'straight-check-package "straight" "Rebuild a PACKAGE if it has been modified.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. See also `straight-rebuild-package' and
`straight-check-all'.

(fn PACKAGE)" t) (autoload 'straight-check-all "straight" "Rebuild any packages that have been modified.
See also `straight-rebuild-all' and `straight-check-package'.
This function should not be called during init." t) (autoload 'straight-rebuild-package "straight" "Rebuild a PACKAGE.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument RECURSIVE, rebuild
all dependencies as well. See also `straight-check-package' and
`straight-rebuild-all'.

(fn PACKAGE &optional RECURSIVE)" t) (autoload 'straight-rebuild-all "straight" "Rebuild all packages.
See also `straight-check-all' and `straight-rebuild-package'." t) (autoload 'straight-prune-build-cache "straight" "Prune the build cache.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information and any cached
autoloads discarded.") (autoload 'straight-prune-build-directory "straight" "Prune the build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build directories deleted.") (autoload 'straight-prune-build "straight" "Prune the build cache and build directory.
This means that only packages that were built in the last init
run and subsequent interactive session will remain; other
packages will have their build mtime information discarded and
their build directories deleted." t) (autoload 'straight-normalize-package "straight" "Normalize a PACKAGE's local repository to its recipe's configuration.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-normalize-all "straight" "Normalize all packages. See `straight-normalize-package'.
Return a list of recipes for packages that were not successfully
normalized. If multiple packages come from the same local
repository, only one is normalized.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-fetch-package "straight" "Try to fetch a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-package-and-deps "straight" "Try to fetch a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are fetched
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
fetch not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-fetch-all "straight" "Try to fetch all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, fetch not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
fetched. If multiple packages come from the same local
repository, only one is fetched.

PREDICATE, if provided, filters the packages that are fetched. It
is called with the package name as a string, and should return
non-nil if the package should actually be fetched.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-merge-package "straight" "Try to merge a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-package-and-deps "straight" "Try to merge a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are merged
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
merge not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-merge-all "straight" "Try to merge all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, merge not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
merged. If multiple packages come from the same local
repository, only one is merged.

PREDICATE, if provided, filters the packages that are merged. It
is called with the package name as a string, and should return
non-nil if the package should actually be merged.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-pull-package "straight" "Try to pull a PACKAGE from the primary remote.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM, pull
not just from primary remote but also from upstream (for forked
packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-package-and-deps "straight" "Try to pull a PACKAGE and its (transitive) dependencies.
PACKAGE, its dependencies, their dependencies, etc. are pulled
from their primary remotes.

PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'. With prefix argument FROM-UPSTREAM,
pull not just from primary remote but also from upstream (for
forked packages).

(fn PACKAGE &optional FROM-UPSTREAM)" t) (autoload 'straight-pull-all "straight" "Try to pull all packages from their primary remotes.
With prefix argument FROM-UPSTREAM, pull not just from primary
remotes but also from upstreams (for forked packages).

Return a list of recipes for packages that were not successfully
pulled. If multiple packages come from the same local repository,
only one is pulled.

PREDICATE, if provided, filters the packages that are pulled. It
is called with the package name as a string, and should return
non-nil if the package should actually be pulled.

(fn &optional FROM-UPSTREAM PREDICATE)" t) (autoload 'straight-push-package "straight" "Push a PACKAGE to its primary remote, if necessary.
PACKAGE is a string naming a package. Interactively, select
PACKAGE from the known packages in the current Emacs session
using `completing-read'.

(fn PACKAGE)" t) (autoload 'straight-push-all "straight" "Try to push all packages to their primary remotes.

Return a list of recipes for packages that were not successfully
pushed. If multiple packages come from the same local repository,
only one is pushed.

PREDICATE, if provided, filters the packages that are normalized.
It is called with the package name as a string, and should return
non-nil if the package should actually be normalized.

(fn &optional PREDICATE)" t) (autoload 'straight-freeze-versions "straight" "Write version lockfiles for currently activated packages.
This implies first pushing all packages that have unpushed local
changes. If the package management system has been used since the
last time the init-file was reloaded, offer to fix the situation
by reloading the init-file again. If FORCE is
non-nil (interactively, if a prefix argument is provided), skip
all checks and write the lockfile anyway.

Currently, writing version lockfiles requires cloning all lazily
installed packages. Hopefully, this inconvenient requirement will
be removed in the future.

Multiple lockfiles may be written (one for each profile),
according to the value of `straight-profiles'.

(fn &optional FORCE)" t) (autoload 'straight-thaw-versions "straight" "Read version lockfiles and restore package versions to those listed." t) (autoload 'straight-bug-report "straight" "Test straight.el in a clean environment.
ARGS may be any of the following keywords and their respective values:
  - :pre-bootstrap (Form)...
      Forms evaluated before bootstrapping straight.el
      e.g. (setq straight-repository-branch \"develop\")
      Note this example is already in the default bootstrapping code.

  - :post-bootstrap (Form)...
      Forms evaluated in the testing environment after boostrapping.
      e.g. (straight-use-package \\='(example :type git :host github))

  - :interactive Boolean
      If nil, the subprocess will immediately exit after the test.
      Output will be printed to `straight-bug-report--process-buffer'
      Otherwise, the subprocess will be interactive.

  - :preserve Boolean
      If non-nil, the test directory is left in the directory stored in the
      variable `temporary-file-directory'. Otherwise, it is
      immediately removed after the test is run.

  - :executable String
      Indicate the Emacs executable to launch.
      Defaults to the path of the current Emacs executable.

  - :raw Boolean
      If non-nil, the raw process output is sent to
      `straight-bug-report--process-buffer'. Otherwise, it is
      formatted as markdown for submitting as an issue.

  - :user-dir String
      If non-nil, the test is run with `user-emacs-directory' set to STRING.
      Otherwise, a temporary directory is created and used.
      Unless absolute, paths are expanded relative to the variable
      `temporary-file-directory'.

ARGS are accessible within the :pre/:post-bootsrap phases via the
locally bound plist, straight-bug-report-args.

(fn &rest ARGS)" nil t) (function-put 'straight-bug-report 'lisp-indent-function 0) (autoload 'straight-dependencies "straight" "Return a list of PACKAGE's dependencies.

(fn &optional PACKAGE)" t) (autoload 'straight-dependents "straight" "Return a list of PACKAGE's dependents.

(fn &optional PACKAGE)" t) (register-definition-prefixes "straight" '("straight-")) (register-definition-prefixes "straight-ert-print-hack" '("+without-print-limits")) (defvar straight-x-pinned-packages nil "List of pinned packages.") (register-definition-prefixes "straight-x" '("straight-x-")) (provide 'straight-autoloads)) "lv" ((lv-autoloads lv) (register-definition-prefixes "lv" '("lv-")) (provide 'lv-autoloads)) "hydra" ((hydra-autoloads hydra hydra-ox hydra-examples) (autoload 'defhydra "hydra" "Create a Hydra - a family of functions with prefix NAME.

NAME should be a symbol, it will be the prefix of all functions
defined here.

BODY has the format:

    (BODY-MAP BODY-KEY &rest BODY-PLIST)

DOCSTRING will be displayed in the echo area to identify the
Hydra.  When DOCSTRING starts with a newline, special Ruby-style
substitution will be performed by `hydra--format'.

Functions are created on basis of HEADS, each of which has the
format:

    (KEY CMD &optional HINT &rest PLIST)

BODY-MAP is a keymap; `global-map' is used quite often.  Each
function generated from HEADS will be bound in BODY-MAP to
BODY-KEY + KEY (both are strings passed to `kbd'), and will set
the transient map so that all following heads can be called
though KEY only.  BODY-KEY can be an empty string.

CMD is a callable expression: either an interactive function
name, or an interactive lambda, or a single sexp (it will be
wrapped in an interactive lambda).

HINT is a short string that identifies its head.  It will be
printed beside KEY in the echo erea if `hydra-is-helpful' is not
nil.  If you don't even want the KEY to be printed, set HINT
explicitly to nil.

The heads inherit their PLIST from BODY-PLIST and are allowed to
override some keys.  The keys recognized are :exit, :bind, and :column.
:exit can be:

- nil (default): this head will continue the Hydra state.
- t: this head will stop the Hydra state.

:bind can be:
- nil: this head will not be bound in BODY-MAP.
- a lambda taking KEY and CMD used to bind a head.

:column is a string that sets the column for all subsequent heads.

It is possible to omit both BODY-MAP and BODY-KEY if you don't
want to bind anything.  In that case, typically you will bind the
generated NAME/body command.  This command is also the return
result of `defhydra'.

(fn NAME BODY &optional DOCSTRING &rest HEADS)" nil t) (function-put 'defhydra 'lisp-indent-function 'defun) (function-put 'defhydra 'doc-string-elt 3) (register-definition-prefixes "hydra" '("defhydra" "hydra-")) (register-definition-prefixes "hydra-examples" '("hydra-" "org-agenda-cts" "whitespace-mode")) (register-definition-prefixes "hydra-ox" '("hydra-ox")) (provide 'hydra-autoloads)) "s" ((s-autoloads s) (register-definition-prefixes "s" '("s-")) (provide 's-autoloads)) "dash" ((dash-autoloads dash) (autoload 'dash-fontify-mode "dash" "Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

This is a minor mode.  If called interactively, toggle the `Dash-Fontify
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `dash-fontify-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t) (put 'global-dash-fontify-mode 'globalized-minor-mode t) (defvar global-dash-fontify-mode nil "Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.") (custom-autoload 'global-dash-fontify-mode "dash" nil) (autoload 'global-dash-fontify-mode "dash" "Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.

See `dash-fontify-mode' for more information on Dash-Fontify mode.

(fn &optional ARG)" t) (autoload 'dash-register-info-lookup "dash" "Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol]." t) (register-definition-prefixes "dash" '("!cdr" "!cons" "--" "->" "-a" "-butlast" "-c" "-d" "-e" "-f" "-gr" "-i" "-juxt" "-keep" "-l" "-m" "-no" "-o" "-p" "-r" "-s" "-t" "-u" "-value-to-list" "-when-let" "-zip" "dash-")) (provide 'dash-autoloads)) "seq" ((seq-autoloads seq seq-pkg seq-25 seq-24) (register-definition-prefixes "seq-24" '("seq")) (autoload 'seq-subseq "seq-25" "Return the sequence of elements of SEQUENCE from START to END.
END is exclusive.

If END is omitted, it defaults to the length of the sequence.  If
START or END is negative, it counts from the end.  Signal an
error if START or END are outside of the sequence (i.e too large
if positive or too small if negative).

(fn SEQUENCE START &optional END)") (autoload 'seq-take "seq-25" "Return the sequence made of the first N elements of SEQUENCE.
The result is a sequence of the same type as SEQUENCE.

If N is a negative integer or zero, an empty sequence is
returned.

(fn SEQUENCE N)") (autoload 'seq-sort-by "seq-25" "Sort SEQUENCE transformed by FUNCTION using PRED as the comparison function.
Elements of SEQUENCE are transformed by FUNCTION before being
sorted.  FUNCTION must be a function of one argument.

(fn FUNCTION PRED SEQUENCE)") (autoload 'seq-filter "seq-25" "Return a list of all the elements in SEQUENCE for which PRED returns non-nil.

(fn PRED SEQUENCE)") (autoload 'seq-remove "seq-25" "Return a list of all the elements in SEQUENCE for which PRED returns nil.

(fn PRED SEQUENCE)") (autoload 'seq-remove-at-position "seq-25" "Return a copy of SEQUENCE with the element at index N removed.

N is the (zero-based) index of the element that should not be in
the result.

The result is a sequence of the same type as SEQUENCE.

(fn SEQUENCE N)") (autoload 'seq-reduce "seq-25" "Reduce the function FUNCTION across SEQUENCE, starting with INITIAL-VALUE.

Return the result of calling FUNCTION with INITIAL-VALUE and the
first element of SEQUENCE, then calling FUNCTION with that result
and the second element of SEQUENCE, then with that result and the
third element of SEQUENCE, etc.  FUNCTION will be called with
INITIAL-VALUE (and then the accumulated value) as the first
argument, and the elements from SEQUENCE as the second argument.

If SEQUENCE is empty, return INITIAL-VALUE and FUNCTION is not called.

(fn FUNCTION SEQUENCE INITIAL-VALUE)") (autoload 'seq-every-p "seq-25" "Return non-nil if PRED returns non-nil for all the elements of SEQUENCE.

(fn PRED SEQUENCE)") (autoload 'seq-some "seq-25" "Return non-nil if PRED returns non-nil for at least one element of SEQUENCE.
If the value is non-nil, it is the first non-nil value returned by PRED.

(fn PRED SEQUENCE)") (autoload 'seq-find "seq-25" "Return the first element in SEQUENCE for which PRED returns non-nil.
If no such element is found, return DEFAULT.

Note that `seq-find' has an ambiguity if the found element is
identical to DEFAULT, as in that case it is impossible to know
whether an element was found or not.

(fn PRED SEQUENCE &optional DEFAULT)") (autoload 'seq-position "seq-25" "Return the (zero-based) index of the first element in SEQUENCE \"equal\" to ELT.
\"Equality\" is defined by the function TESTFN, which defaults to `equal'.

(fn SEQUENCE ELT &optional TESTFN)") (autoload 'seq-positions "seq-25" "Return list of indices of SEQUENCE elements for which TESTFN returns non-nil.

TESTFN is a two-argument function which is called with each element of
SEQUENCE as the first argument and ELT as the second.
TESTFN defaults to `equal'.

The result is a list of (zero-based) indices.

(fn SEQUENCE ELT &optional TESTFN)") (autoload 'seq-uniq "seq-25" "Return a list of the elements of SEQUENCE with duplicates removed.
TESTFN is used to compare elements, and defaults to `equal'.

(fn SEQUENCE &optional TESTFN)") (autoload 'seq-union "seq-25" "Return a list of all the elements that appear in either SEQUENCE1 or SEQUENCE2.
\"Equality\" of elements is defined by the function TESTFN, which
defaults to `equal'.

(fn SEQUENCE1 SEQUENCE2 &optional TESTFN)") (autoload 'seq-intersection "seq-25" "Return a list of all the elements that appear in both SEQUENCE1 and SEQUENCE2.
\"Equality\" of elements is defined by the function TESTFN, which
defaults to `equal'.

(fn SEQUENCE1 SEQUENCE2 &optional TESTFN)") (autoload 'seq-group-by "seq-25" "Apply FUNCTION to each element of SEQUENCE.
Separate the elements of SEQUENCE into an alist using the results as
keys.  Keys are compared using `equal'.

(fn FUNCTION SEQUENCE)") (autoload 'seq-max "seq-25" "Return the largest element of SEQUENCE.
SEQUENCE must be a sequence of numbers or markers.

(fn SEQUENCE)") (autoload 'seq-random-elt "seq-25" "Return a randomly chosen element from SEQUENCE.
Signal an error if SEQUENCE is empty.

(fn SEQUENCE)") (register-definition-prefixes "seq-25" '("seq-")) (provide 'seq-autoloads)) "compat" ((compat-autoloads compat compat-pkg compat-macs compat-30 compat-29 compat-28 compat-27 compat-26 compat-25) (register-definition-prefixes "compat" '("compat-")) (register-definition-prefixes "compat-macs" '("compat-")) (provide 'compat-autoloads)) "pretty-hydra" ((pretty-hydra-autoloads pretty-hydra) (autoload 'pretty-hydra-define "pretty-hydra" "Define a pretty hydra with given NAME, BODY options and HEADS-PLIST.
The generated hydra has a nice-looking docstring which is a table
with columns of command keys and hints.

NAME should be a symbol and is passed to `defhydra' as is.

BODY is the same as that in `defhydra', withe the following
pretty hydra specific ones:

  - `:separator' a single char used to generate the separator
    line.

  - `:title' a string that's added to the beginning of the
    docstring as a title of the hydra.

  - `:formatter' a function that takes the generated docstring
    and return a decorated one.  It can be used to further
    customize the hydra docstring.

  - `:quit-key' a key of list of keys for quitting the hydra.
    When specified, invisible head(s) are created with the
    specified keys for quitting the hydra.

HEADS-PLIST is a plist of columns of hydra heads.  The keys of
the plist should be column names.  The values should be lists of
hydra heads.  Each head has exactly the same syntax as that of
`defhydra', except hint is required for the head to appear in the
docstring.  The following additional options are supported:

  - `:width' the max width of a dynamic hint, used to calculate
    the final width of the entire column.  It is ignored when the
    hint is a string.

  - `:toggle' when specified, it makes the head a toggle and adds
    an indicator to the end of the hint for the status of the
    toggle.  The value of this option can be a symbol, an s-exp
    or t.  The toggle status is read from the given variable, by
    evaluating the given expression or checking the `cmd' as if
    it's a variable.  The latter is especially useful for minior
    modes, e.g.

       (\"n\" `linum-mode' \"line number\" :toggle t)

(fn NAME BODY HEADS-PLIST)" nil t) (function-put 'pretty-hydra-define 'lisp-indent-function 'defun) (autoload 'pretty-hydra-define+ "pretty-hydra" "Redefine an existing pretty-hydra by adding new HEADS-PLIST.
If heads are added to a column already in NAME, the heads are
appended to that column.  Existing BODY is replaced with the new
one if specified.  Arguments are the same as `pretty-hydra-define'.

(fn NAME BODY HEADS-PLIST)" nil t) (function-put 'pretty-hydra-define+ 'lisp-indent-function 'defun) (autoload 'pretty-hydra-toggle "pretty-hydra" "Create a dynamic hint that look like a radio button with given NAME.
Radio is considered on when STATUS is non-nil, otherwise off.

(fn NAME STATUS)") (register-definition-prefixes "pretty-hydra" '("pretty-hydra-")) (provide 'pretty-hydra-autoloads)) "f" ((f-autoloads f f-shortdoc) (register-definition-prefixes "f" '("f-")) (provide 'f-autoloads)) "queue" ((queue-autoloads queue queue-pkg) (register-definition-prefixes "queue" '("make-queue" "queue-")) (provide 'queue-autoloads)) "string-inflection" ((string-inflection-autoloads string-inflection) (autoload 'string-inflection-ruby-style-cycle "string-inflection" "foo_bar => FOO_BAR => FooBar => foo_bar" t) (autoload 'string-inflection-elixir-style-cycle "string-inflection" "foo_bar => FooBar => foo_bar" t) (autoload 'string-inflection-python-style-cycle "string-inflection" "foo_bar => FOO_BAR => FooBar => foo_bar" t) (autoload 'string-inflection-java-style-cycle "string-inflection" "fooBar => FOO_BAR => FooBar => fooBar" t) (autoload 'string-inflection-all-cycle "string-inflection" "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar" t) (autoload 'string-inflection-toggle "string-inflection" "toggle foo_bar <=> FooBar" t) (autoload 'string-inflection-camelcase "string-inflection" "FooBar format" t) (autoload 'string-inflection-lower-camelcase "string-inflection" "fooBar format" t) (autoload 'string-inflection-underscore "string-inflection" "foo_bar format" t) (autoload 'string-inflection-capital-underscore "string-inflection" "Foo_Bar format" t) (autoload 'string-inflection-upcase "string-inflection" "FOO_BAR format" t) (autoload 'string-inflection-kebab-case "string-inflection" "foo-bar format" t) (register-definition-prefixes "string-inflection" '("string-inflection-")) (provide 'string-inflection-autoloads)) "parsebib" ((parsebib-autoloads parsebib) (register-definition-prefixes "parsebib" '("parsebib-")) (provide 'parsebib-autoloads)) "citeproc" ((citeproc-autoloads citeproc citeproc-test-human citeproc-term citeproc-subbibs citeproc-style citeproc-sort citeproc-s citeproc-rt citeproc-proc citeproc-prange citeproc-number citeproc-name citeproc-macro citeproc-locale citeproc-lib citeproc-itemgetters citeproc-itemdata citeproc-generic-elements citeproc-formatters citeproc-disamb citeproc-date citeproc-context citeproc-cite citeproc-choose citeproc-bibtex citeproc-biblatex) (register-definition-prefixes "citeproc" '("citeproc-")) (register-definition-prefixes "citeproc-biblatex" '("citeproc-blt-")) (register-definition-prefixes "citeproc-bibtex" '("citeproc-bt-")) (register-definition-prefixes "citeproc-choose" '("citeproc-")) (register-definition-prefixes "citeproc-cite" '("citeproc-")) (register-definition-prefixes "citeproc-context" '("citeproc-")) (register-definition-prefixes "citeproc-date" '("citeproc-")) (register-definition-prefixes "citeproc-disamb" '("citeproc-")) (register-definition-prefixes "citeproc-formatters" '("citeproc-f")) (register-definition-prefixes "citeproc-generic-elements" '("citeproc-")) (register-definition-prefixes "citeproc-itemdata" '("citeproc-itd-")) (register-definition-prefixes "citeproc-itemgetters" '("citeproc-")) (register-definition-prefixes "citeproc-lib" '("citeproc-")) (register-definition-prefixes "citeproc-locale" '("citeproc-locale-")) (register-definition-prefixes "citeproc-macro" '("citeproc-")) (register-definition-prefixes "citeproc-name" '("citeproc-")) (register-definition-prefixes "citeproc-number" '("citeproc-")) (register-definition-prefixes "citeproc-prange" '("citeproc-prange-")) (register-definition-prefixes "citeproc-proc" '("citeproc-proc-")) (register-definition-prefixes "citeproc-rt" '("citeproc-rt-")) (register-definition-prefixes "citeproc-s" '("citeproc-s-")) (register-definition-prefixes "citeproc-sort" '("citeproc-")) (register-definition-prefixes "citeproc-style" '("citeproc-")) (register-definition-prefixes "citeproc-subbibs" '("citeproc-sb-")) (register-definition-prefixes "citeproc-term" '("citeproc-term-")) (register-definition-prefixes "citeproc-test-human" '("citeproc-test-human-")) (provide 'citeproc-autoloads)) "htmlize" ((htmlize-autoloads htmlize) (autoload 'htmlize-buffer "htmlize" "Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively (or if optional INTERACTIVE is non-nil),
the new buffer is selected in the current window.  The title of the
generated document will be set to the buffer's file name or, if that
is not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

(fn &optional BUFFER INTERACTIVE)" t) (autoload 'htmlize-region "htmlize" "Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

(fn BEG END &optional INTERACTIVE)" t) (autoload 'htmlize-file "htmlize" "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

(fn FILE &optional TARGET)" t) (autoload 'htmlize-many-files "htmlize" "Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

(fn FILES &optional TARGET-DIRECTORY)" t) (autoload 'htmlize-many-files-dired "htmlize" "HTMLize dired-marked files.

(fn ARG &optional TARGET-DIRECTORY)" t) (register-definition-prefixes "htmlize" '("htmlize-")) (provide 'htmlize-autoloads)) "avy" ((avy-autoloads avy) (autoload 'avy-process "avy" "Select one of CANDIDATES using `avy-read'.
Use OVERLAY-FN to visualize the decision overlay.
CLEANUP-FN should take no arguments and remove the effects of
multiple OVERLAY-FN invocations.

(fn CANDIDATES &optional OVERLAY-FN CLEANUP-FN)") (autoload 'avy-goto-char "avy" "Jump to the currently visible CHAR.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-char-in-line "avy" "Jump to the currently visible CHAR in the current line.

(fn CHAR)" t) (autoload 'avy-goto-char-2 "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn CHAR1 CHAR2 &optional ARG BEG END)" t) (autoload 'avy-goto-char-2-above "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-goto-char-2-below "avy" "Jump to the currently visible CHAR1 followed by CHAR2.
This is a scoped version of `avy-goto-char-2', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR1 CHAR2 &optional ARG)" t) (autoload 'avy-isearch "avy" "Jump to one of the current isearch candidates." t) (autoload 'avy-goto-word-0 "avy" "Jump to a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-whitespace-end "avy" "Jump to the end of a whitespace sequence.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.

(fn ARG &optional BEG END)" t) (autoload 'avy-goto-word-1 "avy" "Jump to the currently visible CHAR at a word start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.
BEG and END narrow the scope where candidates are searched.
When SYMBOL is non-nil, jump to symbol start instead of word start.

(fn CHAR &optional ARG BEG END SYMBOL)" t) (autoload 'avy-goto-word-1-above "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-1-below "avy" "Jump to the currently visible CHAR at a word start.
This is a scoped version of `avy-goto-word-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1 "avy" "Jump to the currently visible CHAR at a symbol start.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-above "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer up to point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-symbol-1-below "avy" "Jump to the currently visible CHAR at a symbol start.
This is a scoped version of `avy-goto-symbol-1', where the scope is
the visible part of the current buffer following point.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-subword-0 "avy" "Jump to a word or subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).

When PREDICATE is non-nil it's a function of zero parameters that
should return true.

BEG and END narrow the scope where candidates are searched.

(fn &optional ARG PREDICATE BEG END)" t) (autoload 'avy-goto-subword-1 "avy" "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it).
The case of CHAR is ignored.

(fn CHAR &optional ARG)" t) (autoload 'avy-goto-word-or-subword-1 "avy" "Forward to `avy-goto-subword-1' or `avy-goto-word-1'.
Which one depends on variable `subword-mode'." t) (autoload 'avy-goto-line "avy" "Jump to a line start in current buffer.

When ARG is 1, jump to lines currently visible, with the option
to cancel to `goto-line' by entering a number.

When ARG is 4, negate the window scope determined by
`avy-all-windows'.

Otherwise, forward to `goto-line' with ARG.

(fn &optional ARG)" t) (autoload 'avy-goto-line-above "avy" "Goto visible line above the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-line-below "avy" "Goto visible line below the cursor.
OFFSET changes the distance between the closest key to the cursor and
the cursor
When BOTTOM-UP is non-nil, display avy candidates from top to bottom

(fn &optional OFFSET BOTTOM-UP)" t) (autoload 'avy-goto-end-of-line "avy" "Call `avy-goto-line' and move to the end of the line.

(fn &optional ARG)" t) (autoload 'avy-copy-line "avy" "Copy a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-move-line "avy" "Move a selected line above the current line.
ARG lines can be used.

(fn ARG)" t) (autoload 'avy-copy-region "avy" "Select two lines and copy the text between them to point.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-move-region "avy" "Select two lines and move the text between them above the current line." t) (autoload 'avy-kill-region "avy" "Select two lines and kill the region between them.

The window scope is determined by `avy-all-windows' or
`avy-all-windows-alt' when ARG is non-nil.

(fn ARG)" t) (autoload 'avy-kill-ring-save-region "avy" "Select two lines and save the region between them to the kill ring.
The window scope is determined by `avy-all-windows'.
When ARG is non-nil, do the opposite of `avy-all-windows'.

(fn ARG)" t) (autoload 'avy-kill-whole-line "avy" "Select line and kill the whole selected line.

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

\\[universal-argument] 3 \\[avy-kil-whole-line] kill three lines
starting from the selected line.  \\[universal-argument] -3

\\[avy-kill-whole-line] kill three lines backward including the
selected line.

(fn ARG)" t) (autoload 'avy-kill-ring-save-whole-line "avy" "Select line and save the whole selected line as if killed, but don’t kill it.

This command is similar to `avy-kill-whole-line', except that it
saves the line(s) as if killed, but does not kill it(them).

With a numerical prefix ARG, kill ARG line(s) starting from the
selected line.  If ARG is negative, kill backward.

If ARG is zero, kill the selected line but exclude the trailing
newline.

(fn ARG)" t) (autoload 'avy-setup-default "avy" "Setup the default shortcuts.") (autoload 'avy-goto-char-timer "avy" "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it).

(fn &optional ARG)" t) (autoload 'avy-transpose-lines-in-region "avy" "Transpose lines in the active region." t) (register-definition-prefixes "avy" '("avy-")) (provide 'avy-autoloads)) "let-alist" ((let-alist-autoloads let-alist let-alist-pkg) (autoload 'let-alist "let-alist" "Let-bind dotted symbols to their cdrs in ALIST and execute BODY.
Dotted symbol is any symbol starting with a `.'.  Only those present
in BODY are let-bound and this search is done at compile time.

For instance, the following code

  (let-alist alist
    (if (and .title .body)
        .body
      .site
      .site.contents))

essentially expands to

  (let ((.title (cdr (assq \\='title alist)))
        (.body  (cdr (assq \\='body alist)))
        (.site  (cdr (assq \\='site alist)))
        (.site.contents (cdr (assq \\='contents (cdr (assq \\='site alist))))))
    (if (and .title .body)
        .body
      .site
      .site.contents))

If you nest `let-alist' invocations, the inner one can't access
the variables of the outer one.  You can, however, access alists
inside the original alist by using dots inside the symbol, as
displayed in the example above.

Note that there is no way to differentiate the case where a key
is missing from when it is present, but its value is nil.  Thus,
the following form evaluates to nil:

    (let-alist \\='((some-key . nil))
      .some-key)

(fn ALIST &rest BODY)" nil t) (function-put 'let-alist 'lisp-indent-function 1) (register-definition-prefixes "let-alist" '("let-alist--")) (provide 'let-alist-autoloads)) "biblio-core" ((biblio-core-autoloads biblio-core) (autoload 'biblio-lookup "biblio-core" "Perform a search using BACKEND, and QUERY.
Prompt for any missing or nil arguments.  BACKEND should be a
function obeying the interface described in the docstring of
`biblio-backends'.  Returns the buffer in which results will be
inserted.

(fn &optional BACKEND QUERY)" t) (register-definition-prefixes "biblio-core" '("biblio-")) (provide 'biblio-core-autoloads)) "biblio" ((biblio-autoloads biblio-pkg biblio biblio-ieee biblio-hal biblio-download biblio-doi biblio-dissemin biblio-dblp biblio-crossref biblio-arxiv) (autoload 'biblio-arxiv-backend "biblio-arxiv" "A arXiv backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)") (add-hook 'biblio-init-hook #'biblio-arxiv-backend) (autoload 'biblio-arxiv-lookup "biblio-arxiv" "Start an arXiv search for QUERY, prompting if needed.

(fn &optional QUERY)" t) (defalias 'arxiv-lookup 'biblio-arxiv-lookup) (register-definition-prefixes "biblio-arxiv" '("biblio-arxiv-")) (autoload 'biblio-crossref-backend "biblio-crossref" "A CrossRef backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)") (add-hook 'biblio-init-hook #'biblio-crossref-backend) (autoload 'biblio-crossref-lookup "biblio-crossref" "Start a CrossRef search for QUERY, prompting if needed.

(fn &optional QUERY)" t) (defalias 'crossref-lookup 'biblio-crossref-lookup) (register-definition-prefixes "biblio-crossref" '("biblio-crossref-")) (autoload 'biblio-dblp-backend "biblio-dblp" "A DBLP backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)") (add-hook 'biblio-init-hook #'biblio-dblp-backend) (autoload 'biblio-dblp-lookup "biblio-dblp" "Start a DBLP search for QUERY, prompting if needed.

(fn &optional QUERY)" t) (defalias 'dblp-lookup 'biblio-dblp-lookup) (register-definition-prefixes "biblio-dblp" '("biblio-dblp--")) (autoload 'biblio-dissemin-lookup "biblio-dissemin" "Retrieve a record by DOI from Dissemin, and display it.
Interactively, or if CLEANUP is non-nil, pass DOI through
`biblio-cleanup-doi'.

(fn DOI &optional CLEANUP)" t) (defalias 'dissemin-lookup 'biblio-dissemin-lookup) (autoload 'biblio-dissemin--register-action "biblio-dissemin" "Add Dissemin to list of `biblio-selection-mode' actions.") (add-hook 'biblio-selection-mode-hook #'biblio-dissemin--register-action) (register-definition-prefixes "biblio-dissemin" '("biblio-dissemin--")) (autoload 'biblio-doi-insert-bibtex "biblio-doi" "Insert BibTeX entry matching DOI.

(fn DOI)" t) (register-definition-prefixes "biblio-doi" '("biblio-doi-" "doi-insert-bibtex")) (autoload 'biblio-download--register-action "biblio-download" "Add download to list of `biblio-selection-mode' actions.") (add-hook 'biblio-selection-mode-hook #'biblio-download--register-action) (register-definition-prefixes "biblio-download" '("biblio-download-")) (autoload 'biblio-hal-backend "biblio-hal" "A HAL backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)") (add-hook 'biblio-init-hook #'biblio-hal-backend) (autoload 'biblio-hal-lookup "biblio-hal" "Start a HAL search for QUERY, prompting if needed.

(fn &optional QUERY)" t) (defalias 'hal-lookup 'biblio-hal-lookup) (register-definition-prefixes "biblio-hal" '("biblio-hal--")) (autoload 'biblio-ieee-backend "biblio-ieee" "A IEEE Xplore backend for biblio.el.
COMMAND, ARG, MORE: See `biblio-backends'.

(fn COMMAND &optional ARG &rest MORE)") (add-hook 'biblio-init-hook #'biblio-ieee-backend) (autoload 'biblio-ieee-lookup "biblio-ieee" "Start a IEEE search for QUERY, prompting if needed.

(fn &optional QUERY)" t) (defalias 'ieee-lookup 'biblio-ieee-lookup) (register-definition-prefixes "biblio-ieee" '("biblio-ieee--")) (provide 'biblio-autoloads)) "bibtex-completion" ((bibtex-completion-autoloads bibtex-completion) (put 'bibtex-completion-bibliography 'safe-local-variable 'stringp) (put 'bibtex-completion-notes-global-mode 'globalized-minor-mode t) (defvar bibtex-completion-notes-global-mode nil "Non-nil if Bibtex-Completion-Notes-Global mode is enabled.
See the `bibtex-completion-notes-global-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `bibtex-completion-notes-global-mode'.") (custom-autoload 'bibtex-completion-notes-global-mode "bibtex-completion" nil) (autoload 'bibtex-completion-notes-global-mode "bibtex-completion" "Toggle Bibtex-Completion-Notes mode in all buffers.
With prefix ARG, enable Bibtex-Completion-Notes-Global mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Bibtex-Completion-Notes mode is enabled in all buffers where
`bibtex-completion-notes-mode' would do it.

See `bibtex-completion-notes-mode' for more information on
Bibtex-Completion-Notes mode.

(fn &optional ARG)" t) (register-definition-prefixes "bibtex-completion" '("bibtex-completion-")) (provide 'bibtex-completion-autoloads)) "ht" ((ht-autoloads ht) (register-definition-prefixes "ht" 'nil) (provide 'ht-autoloads)) "ox-pandoc" ((ox-pandoc-autoloads ox-pandoc) (autoload 'org-pandoc-export-to-asciidoc "ox-pandoc" "Export to asciidoc.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-asciidoc-and-open "ox-pandoc" "Export to asciidoc and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-asciidoc "ox-pandoc" "Export as asciidoc.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer "ox-pandoc" "Export to beamer.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-and-open "ox-pandoc" "Export to beamer and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-beamer "ox-pandoc" "Export as beamer.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-pdf "ox-pandoc" "Export to beamer-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-beamer-pdf-and-open "ox-pandoc" "Export to beamer-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-commonmark "ox-pandoc" "Export to commonmark.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-commonmark-and-open "ox-pandoc" "Export to commonmark and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-commonmark "ox-pandoc" "Export as commonmark.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context "ox-pandoc" "Export to context.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-and-open "ox-pandoc" "Export to context and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-context "ox-pandoc" "Export as context.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-pdf "ox-pandoc" "Export to context-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-context-pdf-and-open "ox-pandoc" "Export to context-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook4 "ox-pandoc" "Export to docbook4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook4-and-open "ox-pandoc" "Export to docbook4 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-docbook4 "ox-pandoc" "Export as docbook4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook5 "ox-pandoc" "Export to docbook5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docbook5-and-open "ox-pandoc" "Export to docbook5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-docbook5 "ox-pandoc" "Export as docbook5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docx "ox-pandoc" "Export to docx.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-docx-and-open "ox-pandoc" "Export to docx and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dokuwiki "ox-pandoc" "Export to dokuwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dokuwiki-and-open "ox-pandoc" "Export to dokuwiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-dokuwiki "ox-pandoc" "Export as dokuwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dzslides "ox-pandoc" "Export to dzslides.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-dzslides-and-open "ox-pandoc" "Export to dzslides and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-dzslides "ox-pandoc" "Export as dzslides.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub2 "ox-pandoc" "Export to epub2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub2-and-open "ox-pandoc" "Export to epub2 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub3 "ox-pandoc" "Export to epub3.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-epub3-and-open "ox-pandoc" "Export to epub3 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-fb2 "ox-pandoc" "Export to fb2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-fb2-and-open "ox-pandoc" "Export to fb2 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-fb2 "ox-pandoc" "Export as fb2.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-gfm "ox-pandoc" "Export to gfm.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-gfm-and-open "ox-pandoc" "Export to gfm and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-gfm "ox-pandoc" "Export as gfm.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-haddock "ox-pandoc" "Export to haddock.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-haddock-and-open "ox-pandoc" "Export to haddock and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-haddock "ox-pandoc" "Export as haddock.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html4 "ox-pandoc" "Export to html4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html4-and-open "ox-pandoc" "Export to html4 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-html4 "ox-pandoc" "Export as html4.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5 "ox-pandoc" "Export to html5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-and-open "ox-pandoc" "Export to html5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-html5 "ox-pandoc" "Export as html5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-pdf "ox-pandoc" "Export to html5-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-html5-pdf-and-open "ox-pandoc" "Export to html5-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-icml "ox-pandoc" "Export to icml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-icml-and-open "ox-pandoc" "Export to icml and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-icml "ox-pandoc" "Export as icml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jats "ox-pandoc" "Export to jats.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jats-and-open "ox-pandoc" "Export to jats and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-jats "ox-pandoc" "Export as jats.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-jira "ox-pandoc" "Export to jira.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-jira "ox-pandoc" "Export as jira.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-json "ox-pandoc" "Export to json.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-json-and-open "ox-pandoc" "Export to json and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-json "ox-pandoc" "Export as json.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex "ox-pandoc" "Export to latex.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-and-open "ox-pandoc" "Export to latex and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-latex "ox-pandoc" "Export as latex.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-pdf "ox-pandoc" "Export to latex-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-latex-pdf-and-open "ox-pandoc" "Export to latex-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-man "ox-pandoc" "Export to man.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-man-and-open "ox-pandoc" "Export to man and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-man "ox-pandoc" "Export as man.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown "ox-pandoc" "Export to markdown.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown-and-open "ox-pandoc" "Export to markdown and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown "ox-pandoc" "Export as markdown.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_mmd "ox-pandoc" "Export to markdown_mmd.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_mmd-and-open "ox-pandoc" "Export to markdown_mmd and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_mmd "ox-pandoc" "Export as markdown_mmd.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_phpextra "ox-pandoc" "Export to markdown_phpextra.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_phpextra-and-open "ox-pandoc" "Export to markdown_phpextra and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_phpextra "ox-pandoc" "Export as markdown_phpextra.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_strict "ox-pandoc" "Export to markdown_strict.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-markdown_strict-and-open "ox-pandoc" "Export to markdown_strict and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-markdown_strict "ox-pandoc" "Export as markdown_strict.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-mediawiki "ox-pandoc" "Export to mediawiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-mediawiki-and-open "ox-pandoc" "Export to mediawiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-mediawiki "ox-pandoc" "Export as mediawiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms "ox-pandoc" "Export to ms.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-and-open "ox-pandoc" "Export to ms and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-ms "ox-pandoc" "Export as ms.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-pdf "ox-pandoc" "Export to ms-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-ms-pdf-and-open "ox-pandoc" "Export to ms-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-muse "ox-pandoc" "Export to muse.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-muse-and-open "ox-pandoc" "Export to muse and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-muse "ox-pandoc" "Export as muse.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-native "ox-pandoc" "Export to native.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-native-and-open "ox-pandoc" "Export to native and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-native "ox-pandoc" "Export as native.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-odt "ox-pandoc" "Export to odt.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-odt-and-open "ox-pandoc" "Export to odt and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opendocument "ox-pandoc" "Export to opendocument.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opendocument-and-open "ox-pandoc" "Export to opendocument and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-opendocument "ox-pandoc" "Export as opendocument.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opml "ox-pandoc" "Export to opml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-opml-and-open "ox-pandoc" "Export to opml and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-opml "ox-pandoc" "Export as opml.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-org "ox-pandoc" "Export to org.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-org-and-open "ox-pandoc" "Export to org and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-org "ox-pandoc" "Export as org.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-plain "ox-pandoc" "Export to plain.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-plain-and-open "ox-pandoc" "Export to plain and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-plain "ox-pandoc" "Export as plain.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-pptx "ox-pandoc" "Export to pptx.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-pptx-and-open "ox-pandoc" "Export to pptx and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-revealjs "ox-pandoc" "Export to revealjs.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-revealjs-and-open "ox-pandoc" "Export to revealjs and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-revealjs "ox-pandoc" "Export as revealjs.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rst "ox-pandoc" "Export to rst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rst-and-open "ox-pandoc" "Export to rst and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-rst "ox-pandoc" "Export as rst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rtf "ox-pandoc" "Export to rtf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-rtf-and-open "ox-pandoc" "Export to rtf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-rtf "ox-pandoc" "Export as rtf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-s5 "ox-pandoc" "Export to s5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-s5-and-open "ox-pandoc" "Export to s5 and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-s5 "ox-pandoc" "Export as s5.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slideous "ox-pandoc" "Export to slideous.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slideous-and-open "ox-pandoc" "Export to slideous and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-slideous "ox-pandoc" "Export as slideous.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slidy "ox-pandoc" "Export to slidy.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-slidy-and-open "ox-pandoc" "Export to slidy and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-slidy "ox-pandoc" "Export as slidy.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-tei "ox-pandoc" "Export to tei.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-tei-and-open "ox-pandoc" "Export to tei and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-tei "ox-pandoc" "Export as tei.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-texinfo "ox-pandoc" "Export to texinfo.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-texinfo-and-open "ox-pandoc" "Export to texinfo and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-texinfo "ox-pandoc" "Export as texinfo.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-textile "ox-pandoc" "Export to textile.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-textile-and-open "ox-pandoc" "Export to textile and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-textile "ox-pandoc" "Export as textile.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-typst "ox-pandoc" "Export to typst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-typst-and-open "ox-pandoc" "Export to typst and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-typst "ox-pandoc" "Export as typst.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-typst-pdf "ox-pandoc" "Export to typst-pdf.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-typst-pdf-and-open "ox-pandoc" "Export to typst-pdf and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-zimwiki "ox-pandoc" "Export to zimwiki.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-to-zimwiki-and-open "ox-pandoc" "Export to zimwiki and open.

(fn &optional A S V B E)" t) (autoload 'org-pandoc-export-as-zimwiki "ox-pandoc" "Export as zimwiki.

(fn &optional A S V B E)" t) (register-definition-prefixes "ox-pandoc" '("org-pandoc-")) (provide 'ox-pandoc-autoloads)) "request" ((request-autoloads request) (autoload 'request-response-header "request" "Fetch the values of RESPONSE header field named FIELD-NAME.

It returns comma separated values when the header has multiple
field with the same name, as :RFC:`2616` specifies.

Examples::

  (request-response-header response
                           \"content-type\") ; => \"text/html; charset=utf-8\"
  (request-response-header response
                           \"unknown-field\") ; => nil

(fn RESPONSE FIELD-NAME)") (autoload 'request-response-headers "request" "Return RESPONSE headers as an alist.
I would have chosen a function name that wasn't so suggestive that
`headers` is a member of the `request-response` struct, but
as there's already precedent with `request-response-header', I
hew to consistency.

(fn RESPONSE)") (autoload 'request "request" "Main entry requesting URL with property list SETTINGS as follow.

==================== ========================================================
Keyword argument      Explanation
==================== ========================================================
TYPE          (string)   type of request to make: POST/GET/PUT/DELETE
PARAMS         (alist)   set \"?key=val\" part in URL
DATA    (string/alist)   data to be sent to the server
FILES          (alist)   files to be sent to the server (see below)
PARSER        (symbol)   a function that reads current buffer and return data
HEADERS        (alist)   additional headers to send with the request
ENCODING      (symbol)   encoding for request body (utf-8 by default)
SUCCESS     (function)   called on success
ERROR       (function)   called on error
COMPLETE    (function)   called on both success and error
TIMEOUT       (number)   timeout in second
STATUS-CODE    (alist)   map status code (int) to callback
SYNC            (bool)   If non-nil, wait until request is done. Default is nil.
==================== ========================================================


* Callback functions

Callback functions STATUS, ERROR, COMPLETE and `cdr\\='s in element of
the alist STATUS-CODE take same keyword arguments listed below.  For
forward compatibility, these functions must ignore unused keyword
arguments (i.e., it\\='s better to use `&allow-other-keys\\=' [#]_).::

    (CALLBACK                      ; SUCCESS/ERROR/COMPLETE/STATUS-CODE
     :data          data           ; whatever PARSER function returns, or nil
     :error-thrown  error-thrown   ; (ERROR-SYMBOL . DATA), or nil
     :symbol-status symbol-status  ; success/error/timeout/abort/parse-error
     :response      response       ; request-response object
     ...)

.. [#] `&allow-other-keys\\=' is a special \"markers\" available in macros
   in the CL library for function definition such as `cl-defun\\=' and
   `cl-function\\='.  Without this marker, you need to specify all arguments
   to be passed.  This becomes problem when request.el adds new arguments
   when calling callback functions.  If you use `&allow-other-keys\\='
   (or manually ignore other arguments), your code is free from this
   problem.  See info node `(cl) Argument Lists\\=' for more information.

Arguments data, error-thrown, symbol-status can be accessed by
`request-response-data\\=', `request-response-error-thrown\\=',
`request-response-symbol-status\\=' accessors, i.e.::

    (request-response-data RESPONSE)  ; same as data

Response object holds other information which can be accessed by
the following accessors:
`request-response-status-code\\=',
`request-response-url\\=' and
`request-response-settings\\='

* STATUS-CODE callback

STATUS-CODE is an alist of the following format::

    ((N-1 . CALLBACK-1)
     (N-2 . CALLBACK-2)
     ...)

Here, N-1, N-2,... are integer status codes such as 200.


* FILES

FILES is an alist of the following format::

    ((NAME-1 . FILE-1)
     (NAME-2 . FILE-2)
     ...)

where FILE-N is a list of the form::

    (FILENAME &key PATH BUFFER STRING MIME-TYPE)

FILE-N can also be a string (path to the file) or a buffer object.
In that case, FILENAME is set to the file name or buffer name.

Example FILES argument::

    `((\"passwd\"   . \"/etc/passwd\")                ; filename = passwd
      (\"scratch\"  . ,(get-buffer \"*scratch*\"))    ; filename = *scratch*
      (\"passwd2\"  . (\"password.txt\" :file \"/etc/passwd\"))
      (\"scratch2\" . (\"scratch.txt\"  :buffer ,(get-buffer \"*scratch*\")))
      (\"data\"     . (\"data.csv\"     :data \"1,2,3\\n4,5,6\\n\")))

.. note:: FILES is implemented only for curl backend for now.
   As furl.el_ supports multipart POST, it should be possible to
   support FILES in pure elisp by making furl.el_ another backend.
   Contributions are welcome.

   .. _furl.el: https://code.google.com/p/furl-el/


* PARSER function

PARSER function takes no argument and it is executed in the
buffer with HTTP response body.  The current position in the HTTP
response buffer is at the beginning of the buffer.  As the HTTP
header is stripped off, the cursor is actually at the beginning
of the response body.  So, for example, you can pass `json-read\\='
to parse JSON object in the buffer.  To fetch whole response as a
string, pass `buffer-string\\='.

When using `json-read\\=', it is useful to know that the returned
type can be modified by `json-object-type\\=', `json-array-type\\=',
`json-key-type\\=', `json-false\\=' and `json-null\\='.  See docstring of
each function for what it does.  For example, to convert JSON
objects to plist instead of alist, wrap `json-read\\=' by `lambda\\='
like this.::

    (request
     \"https://...\"
     :parser (lambda ()
               (let ((json-object-type \\='plist))
                 (json-read)))
     ...)

This is analogous to the `dataType\\=' argument of jQuery.ajax_.
Only this function can access to the process buffer, which
is killed immediately after the execution of this function.

* SYNC

Synchronous request is functional, but *please* don\\='t use it
other than testing or debugging.  Emacs users have better things
to do rather than waiting for HTTP request.  If you want a better
way to write callback chains, use `request-deferred\\='.

If you can\\='t avoid using it (e.g., you are inside of some hook
which must return some value), make sure to set TIMEOUT to
relatively small value.

Due to limitation of `url-retrieve-synchronously\\=', response slots
`request-response-error-thrown\\=', `request-response-history\\=' and
`request-response-url\\=' are unknown (always nil) when using
synchronous request with `url-retrieve\\=' backend.

* Note

API of `request\\=' is somewhat mixture of jQuery.ajax_ (Javascript)
and requests.request_ (Python).

.. _jQuery.ajax: https://api.jquery.com/jQuery.ajax/
.. _requests.request: https://docs.python-requests.org

(fn URL &rest SETTINGS &key (PARAMS nil) (DATA nil) (HEADERS nil) (ENCODING \\='utf-8) (ERROR nil) (SYNC nil) (RESPONSE (make-request-response)) &allow-other-keys)") (function-put 'request 'lisp-indent-function 'defun) (autoload 'request-untrampify-filename "request" "Return FILE as the local file name.

(fn FILE)") (autoload 'request-abort "request" "Abort request for RESPONSE (the object returned by `request').
Note that this function invoke ERROR and COMPLETE callbacks.
Callbacks may not be called immediately but called later when
associated process is exited.

(fn RESPONSE)") (register-definition-prefixes "request" '("request-")) (provide 'request-autoloads)) "org-ref" ((org-ref-autoloads x2bib org-ref org-ref-wos org-ref-worldcat org-ref-utils org-ref-url-utils org-ref-scopus org-ref-scifinder org-ref-sci-id org-ref-refproc org-ref-ref-links org-ref-pubmed org-ref-publish org-ref-pgk org-ref-pdf org-ref-natbib-bbl-citeproc org-ref-misc-links org-ref-latex org-ref-label-link org-ref-ivy org-ref-isbn org-ref-helm org-ref-glossary org-ref-extract org-ref-export org-ref-core org-ref-compat org-ref-citation-links org-ref-bibtex org-ref-bibliography-links org-ref-arxiv openalex nist-webbook doi-utils contrib) (register-definition-prefixes "contrib" '("org-ref-")) (autoload 'doi-utils-async-download-pdf "doi-utils" "Download the PDF for bibtex entry at point asynchronously.
It is not fully async, only the download is. Fully async is
harder because you need to run `doi-utils-get-pdf-url' async
too. " t) (autoload 'doi-utils-get-bibtex-entry-pdf "doi-utils" "Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved, by the name
%s.pdf where %s is the bibtex label. Files will not be
overwritten. The pdf will be checked to make sure it is a pdf,
and not some html failure page. You must have permission to
access the pdf. We open the pdf at the end if
`doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked.

(fn &optional ARG)" t) (autoload 'doi-utils-add-bibtex-entry-from-doi "doi-utils" "Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the initial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use.

(fn DOI &optional BIBFILE)" t) (autoload 'doi-utils-doi-to-org-bibtex "doi-utils" "Convert a DOI to an ‘org-bibtex’ form and insert it at point.

(fn DOI)" t) (autoload 'bibtex-set-field "doi-utils" "Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'.

(fn FIELD VALUE &optional NODELIM)" t) (autoload 'doi-utils-update-bibtex-entry-from-doi "doi-utils" "Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost.

(fn DOI)" t) (autoload 'doi-utils-update-field "doi-utils" "Update the field at point in the bibtex entry.
Data is retrieved from the doi in the entry." t) (autoload 'doi-utils-wos "doi-utils" "Open Web of Science entry for DOI.

(fn DOI)" t) (autoload 'doi-utils-wos-citing "doi-utils" "Open Web of Science citing articles entry for DOI.
May be empty if none are found.

(fn DOI)" t) (autoload 'doi-utils-wos-related "doi-utils" "Open Web of Science related articles page for DOI.

(fn DOI)" t) (autoload 'doi-utils-ads "doi-utils" "Open ADS entry for DOI

(fn DOI)" t) (autoload 'doi-utils-open "doi-utils" "Open DOI in browser.

(fn DOI)" t) (autoload 'doi-utils-open-bibtex "doi-utils" "Search through variable `bibtex-completion-bibliography' for DOI.

(fn DOI)" t) (autoload 'doi-utils-crossref "doi-utils" "Search DOI in CrossRef.

(fn DOI)" t) (autoload 'doi-utils-google-scholar "doi-utils" "Google scholar the DOI.

(fn DOI)" t) (autoload 'doi-utils-pubmed "doi-utils" "Search Pubmed for the DOI.

(fn DOI)" t) (autoload 'doi-utils-crossref-citation-query "doi-utils" "Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches. Choose one with completion." t) (autoload 'doi-utils-debug "doi-utils" "Generate an org-buffer showing data about DOI.

(fn DOI)" t) (autoload 'doi-utils-add-entry-from-crossref-query "doi-utils" "Search Crossref with QUERY and use completion to select an entry to add to BIBTEX-FILE.

(fn QUERY BIBTEX-FILE)" t) (register-definition-prefixes "doi-utils" '("*doi-utils-" "agu-pdf-url" "aip-pdf-url" "aps-pdf-url" "arxiv-pdf-url" "chemistry-europe-pdf-url" "copernicus-" "crossref-add-bibtex-entry" "doi-" "ecs" "frontiers-pdf-url" "generic-full-pdf-url" "highwire-pdf-url" "ieee" "iop-pdf-url" "jneurosci-pdf-url" "jstor-pdf-url" "linkinghub-elsevier-pdf-url" "nature-pdf-url" "osa-pdf-url" "pnas-pdf-url" "rsc-pdf-url" "rss-pdf-url" "sage-pdf-url" "science-" "siam-pdf-url" "springer-" "tandfonline-pdf-url" "wiley-pdf-url-2")) (autoload 'nist-webbook-formula "nist-webbook" "Search NIST webbook for FORMULA.

(fn FORMULA)" t) (autoload 'nist-webbook-name "nist-webbook" "Search NIST webbook for NAME.

(fn NAME)" t) (register-definition-prefixes "openalex" '("oa-" "org-ref-citation-hydra")) (autoload 'arxiv-add-bibtex-entry "org-ref-arxiv" "Add bibtex entry for ARXIV-NUMBER to BIBFILE.

(fn ARXIV-NUMBER BIBFILE)" t) (autoload 'arxiv-get-pdf "org-ref-arxiv" "Retrieve a pdf for ARXIV-NUMBER and save it to PDF.

(fn ARXIV-NUMBER PDF)" t) (autoload 'arxiv-get-pdf-add-bibtex-entry "org-ref-arxiv" "Add bibtex entry for ARXIV-NUMBER to BIBFILE.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key.

(fn ARXIV-NUMBER BIBFILE PDFDIR)" t) (register-definition-prefixes "org-ref-arxiv" '("arxiv-")) (register-definition-prefixes "org-ref-bibliography-links" '("org-ref-")) (autoload 'org-ref-bibtex-generate-longtitles "org-ref-bibtex" "Generate longtitles.bib which are @string definitions.
The full journal names are in `org-ref-bibtex-journal-abbreviations'." t) (autoload 'org-ref-bibtex-generate-shorttitles "org-ref-bibtex" "Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `org-ref-bibtex-journal-abbreviations'." t) (autoload 'org-ref-stringify-journal-name "org-ref-bibtex" "Replace journal name in a bibtex entry with a string.
The strings are defined in
`org-ref-bibtex-journal-abbreviations'.  The optional arguments KEY,
START and END allow you to use this with `bibtex-map-entries'

(fn &optional KEY START END)" t) (autoload 'org-ref-set-journal-string "org-ref-bibtex" "Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `org-ref-bibtex-journal-abbreviations'.

(fn FULL-JOURNAL-NAME)" t) (autoload 'org-ref-replace-nonascii "org-ref-bibtex" "Replace non-ascii characters with LaTeX representations in a
bibtex entry." t) (autoload 'org-ref-title-case "org-ref-bibtex" "Convert a bibtex entry title and booktitle to title-case.
Convert only if the entry type is a member of the list
`org-ref-title-case-types'. The arguments KEY, START and END are
optional, and are only there so you can use this function with
`bibtex-map-entries' to change all the title entries in articles and
books.

(fn &optional KEY START END)" t) (autoload 'org-ref-title-case-article "org-ref-bibtex" "Convert a bibtex entry article or book title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles and books.

(fn &optional KEY START END)" t) (autoload 'org-ref-sentence-case-article "org-ref-bibtex" "Convert a bibtex entry article title to sentence-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles.

(fn &optional KEY START END)" t) (autoload 'org-ref-bibtex-next-entry "org-ref-bibtex" "Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing.

(fn &optional N)" t) (autoload 'org-ref-bibtex-previous-entry "org-ref-bibtex" "Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back.

(fn &optional N)" t) (autoload 'org-ref-bibtex-visible-entry "org-ref-bibtex" "Jump to visible entry." t) (autoload 'org-ref-bibtex-visible-field "org-ref-bibtex" "Jump to visible field." t) (autoload 'org-ref-bibtex-format-url-if-doi "org-ref-bibtex" "Hook function to format url to follow the current DOI conventions." t) (autoload 'org-ref-bibtex-wos "org-ref-bibtex" "Open bibtex entry in Web Of Science if there is a DOI." t) (autoload 'org-ref-bibtex-wos-citing "org-ref-bibtex" "Open citing articles for bibtex entry in Web Of Science if
there is a DOI." t) (autoload 'org-ref-bibtex-wos-related "org-ref-bibtex" "Open related articles for bibtex entry in Web Of Science if
there is a DOI." t) (autoload 'org-ref-bibtex-crossref "org-ref-bibtex" "Open the bibtex entry in Crossref by its doi." t) (autoload 'org-ref-bibtex-google-scholar "org-ref-bibtex" "Open the bibtex entry at point in google-scholar by its doi." t) (autoload 'org-ref-bibtex-pubmed "org-ref-bibtex" "Open the bibtex entry at point in Pubmed by its doi." t) (autoload 'org-ref-bibtex-pdf "org-ref-bibtex" "Open the pdf for the bibtex entry at point.
Thin wrapper to get `org-ref-bibtex' to open pdf, because it
calls functions with a DOI argument.

(fn &optional _)" t) (autoload 'org-ref-bibtex-assoc-pdf-with-entry "org-ref-bibtex" "Prompt for pdf associated with entry at point and rename it.
Check whether a pdf already exists in `bibtex-completion-library' with the
name '[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it in
a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'.

(fn &optional PREFIX)" t) (autoload 'org-ref-email-bibtex-entry "org-ref-bibtex" "Email current bibtex entry at point and pdf if it exists." t) (autoload 'org-ref-set-bibtex-keywords "org-ref-bibtex" "Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords.

(fn KEYWORDS &optional ARG)" t) (autoload 'org-ref-extract-bibtex-blocks "org-ref-bibtex" "Extract all bibtex blocks in buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u), which
will clobber the file.

(fn BIBFILE)" t) (autoload 'org-ref-open-bibtex-pdf "org-ref-bibtex" "Open pdf for a bibtex entry, if it exists." t) (autoload 'org-ref-open-bibtex-notes "org-ref-bibtex" "From a bibtex entry, open the notes if they exist." t) (autoload 'org-ref-open-in-browser "org-ref-bibtex" "Open the bibtex entry at point in a browser using the url field or doi field." t) (autoload 'org-ref-build-full-bibliography "org-ref-bibtex" "Build pdf of all bibtex entries, and open it." t) (autoload 'org-ref-sort-bibtex-entry "org-ref-bibtex" "Sort fields of entry in standard order." t) (autoload 'org-ref-downcase-bibtex-entry "org-ref-bibtex" "Downcase the entry type and fields." t) (autoload 'org-ref-clean-bibtex-entry "org-ref-bibtex" "Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'." t) (register-definition-prefixes "org-ref-bibtex" '("orcb-" "org-ref-")) (autoload 'org-ref-delete-citation-at-point "org-ref-citation-links" "Delete the citation or reference at point." t) (autoload 'org-ref-replace-citation-at-point "org-ref-citation-links" "Replace the citation at point." t) (autoload 'org-ref-edit-pre-post-notes "org-ref-citation-links" "Edit the pre/post notes at point.

if you are not on a key, or with optional prefix
arg COMMON, edit the common prefixes instead.

(fn &optional COMMON)" t) (autoload 'org-ref-change-cite-type "org-ref-citation-links" "Change the cite type of citation link at point." t) (autoload 'org-ref-sort-citation-link "org-ref-citation-links" "Replace link at point with sorted link by year." t) (autoload 'org-ref-next-key "org-ref-citation-links" "Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one." t) (autoload 'org-ref-previous-key "org-ref-citation-links" "Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one." t) (autoload 'org-ref-jump-to-visible-key "org-ref-citation-links" "Jump to a visible key with avy." t) (autoload 'org-ref-insert-cite-link "org-ref-citation-links" "Insert a cite link with completion.
Optional prefix arg SET-TYPE to choose the cite type.

(fn &optional SET-TYPE)" t) (register-definition-prefixes "org-ref-citation-links" '("org-ref-")) (register-definition-prefixes "org-ref-compat" '("org-ref-to-org-cite")) (autoload 'org-ref-insert-link "org-ref-core" "Insert an org-ref link.
If no prefix ARG insert a cite.
If one prefix ARG insert a ref.
If two prefix ARGs insert a label.

This is a generic function. Specific backends might
provide their own version.

(fn ARG)" t) (autoload 'org-ref-help "org-ref-core" "Open the `org-ref' manual." t) (register-definition-prefixes "org-ref-core" '("org-ref-")) (register-definition-prefixes "org-ref-export" '("org-ref")) (register-definition-prefixes "org-ref-extract" '("org-ref-")) (register-definition-prefixes "org-ref-glossary" '("or-" "org-ref-")) (autoload 'org-ref-cite-insert-helm "org-ref-helm" "Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications).

(fn &optional ARG LOCAL-BIB INPUT)" t) (register-definition-prefixes "org-ref-helm" '("org-ref-helm-source-")) (autoload 'org-ref-isbn-clean-bibtex-entry "org-ref-isbn" "Clean a bibtex entry inserted via `isbn-to-bibtex'.
See functions in `org-ref-isbn-clean-bibtex-entry-hook'." t) (autoload 'isbn-to-bibtex-lead "org-ref-isbn" "Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file.

(fn ISBN)" t) (autoload 'isbn-to-bibtex "org-ref-isbn" "Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from www.ebook.de.

(fn ISBN BIBFILE)" t) (register-definition-prefixes "org-ref-isbn" '("isbn-to-bibtex-open-library" "org-ref-isbn-" "oricb-")) (register-definition-prefixes "org-ref-ivy" '("org-ref-")) (autoload 'org-ref-insert-label-link "org-ref-label-link" "Insert a new label with completion.
The completion helps ensure you use a unique label." t) (register-definition-prefixes "org-ref-latex" '("org-ref-" "orl-")) (autoload 'org-ref-list-of-figures "org-ref-misc-links" "Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections.

(fn &optional ARG)" t) (autoload 'org-ref-list-of-tables "org-ref-misc-links" "Generate a buffer with a list of tables.
ARG does nothing.

(fn &optional ARG)" t) (autoload 'org-ref-index "org-ref-misc-links" "Open an *index* buffer with links to index entries.
PATH is required for the org-link, but it does nothing here.

(fn &optional PATH)" t) (register-definition-prefixes "org-ref-misc-links" '("org-ref-idxproc")) (register-definition-prefixes "org-ref-natbib-bbl-citeproc" '("org-")) (autoload 'org-ref-pdf-to-bibtex "org-ref-pdf" "Add pdf of current buffer to bib file and save pdf. The pdf
should be open in Emacs using the `pdf-tools' package." t) (autoload 'org-ref-pdf-debug-pdf "org-ref-pdf" "Try to debug getting a doi from a pdf.
Opens a buffer with the pdf converted to text, and `occur' on the
variable `org-ref-pdf-doi-regex'.

(fn PDF-FILE)" t) (autoload 'org-ref-pdf-crossref-lookup "org-ref-pdf" "Lookup highlighted text in PDFView in CrossRef." t) (register-definition-prefixes "org-ref-pdf" '("org-ref-" "pdftotext-executable")) (register-definition-prefixes "org-ref-publish" '("org-ref-publish-")) (autoload 'pubmed-insert-bibtex-from-pmid "org-ref-pubmed" "Insert a bibtex entry at point derived from PMID.
You must clean the entry after insertion.

(fn PMID)" t) (autoload 'pubmed "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed in a browser." t) (autoload 'pubmed-advanced "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed/advanced in a browser." t) (autoload 'pubmed-simple-search "org-ref-pubmed" "Open QUERY in Pubmed in a browser.

(fn QUERY)" t) (autoload 'pubmed-clinical "org-ref-pubmed" "Open http://www.ncbi.nlm.nih.gov/pubmed/clinical in a browser." t) (autoload 'pubmed-clinical-search "org-ref-pubmed" "Open QUERY in pubmed-clinical.

(fn QUERY)" t) (register-definition-prefixes "org-ref-pubmed" '("pubmed-")) (register-definition-prefixes "org-ref-ref-links" '("org-ref-")) (register-definition-prefixes "org-ref-refproc" '("org-ref-")) (autoload 'scifinder "org-ref-scifinder" "Open https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf in a browser." t) (autoload 'scopus-related-by-keyword-url "org-ref-scopus" "Return a Scopus url to articles related by keyword for DOI.

(fn DOI)" t) (autoload 'scopus-related-by-author-url "org-ref-scopus" "Return a Scopus url to articles related by author for DOI.

(fn DOI)" t) (autoload 'scopus-related-by-references-url "org-ref-scopus" "Return a Scopus url to articles related by references for DOI.

(fn DOI)" t) (autoload 'scopus-open-eid "org-ref-scopus" "Open article with EID in browser.

(fn EID)" t) (autoload 'scopus-basic-search "org-ref-scopus" "Open QUERY as a basic title-abstract-keyword search at scopus.com.

(fn QUERY)" t) (autoload 'scopus-advanced-search "org-ref-scopus" "Open QUERY as an advanced search at scopus.com.

(fn QUERY)" t) (register-definition-prefixes "org-ref-scopus" '("*hydra-eid*" "*scopus-api-key*" "scopus")) (autoload 'org-ref-url-debug-url "org-ref-url-utils" "Open a buffer to URL with all doi patterns highlighted.

(fn URL)" t) (autoload 'org-ref-url-html-to-bibtex "org-ref-url-utils" "Convert URL to a bibtex or biblatex entry in BIBFILE.
If URL is the first in the kill ring, use it. Otherwise, prompt for
one in the minibuffer.

(fn BIBFILE &optional URL)" t) (register-definition-prefixes "org-ref-url-utils" '("org-ref-")) (autoload 'org-ref-version "org-ref-utils" "Provide a version string for org-ref.
Copies the string to the clipboard." t) (autoload 'org-ref-debug "org-ref-utils" "Print some debug information to a buffer." t) (autoload 'org-ref-open-pdf-at-point "org-ref-utils" "Open the pdf for bibtex key under point if it exists." t) (autoload 'org-ref-add-pdf-at-point "org-ref-utils" "Add the pdf for bibtex key under point if it exists.

Similar to org-ref-bibtex-assoc-pdf-with-entry prompt for pdf
associated with bibtex key at point and rename it.  Check whether a
pdf already exists in `bibtex-completion-library' with the name
'[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it
in a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'.

(fn &optional PREFIX)" t) (autoload 'org-ref-open-url-at-point "org-ref-utils" "Open the url for bibtex key under point." t) (autoload 'org-ref-open-notes-at-point "org-ref-utils" "Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program.

(fn &optional THEKEY)" t) (autoload 'org-ref-open-citation-at-point "org-ref-utils" "Open bibtex file to key at point." t) (autoload 'org-ref-copy-entry-as-summary "org-ref-utils" "Copy the bibtex entry for the citation at point as a summary." t) (autoload 'org-ref-ads-at-point "org-ref-utils" "Open the doi in ADS for bibtex key under point." t) (autoload 'org-ref-wos-at-point "org-ref-utils" "Open the doi in wos for bibtex key under point." t) (autoload 'org-ref-wos-citing-at-point "org-ref-utils" "Open the doi in wos citing articles for bibtex key under point." t) (autoload 'org-ref-wos-related-at-point "org-ref-utils" "Open the doi in wos related articles for bibtex key under point." t) (autoload 'org-ref-google-scholar-at-point "org-ref-utils" "Search google scholar for bibtex key under point using the title." t) (autoload 'org-ref-biblio-at-point "org-ref-utils" "Do a biblio search for bibtex key under point using the title." t) (autoload 'org-ref-pubmed-at-point "org-ref-utils" "Open the doi in pubmed for bibtex key under point." t) (autoload 'org-ref-crossref-at-point "org-ref-utils" "Open the doi in crossref for bibtex key under point." t) (autoload 'org-ref-email-at-point "org-ref-utils" "Email the citation(s) at point." t) (autoload 'org-ref-find-non-ascii-characters "org-ref-utils" "Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files." t) (autoload 'org-ref-extract-bibtex-to-file "org-ref-utils" "Extract all bibtex entries for citations buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u),
which will CLOBBER the file.

(fn BIBFILE &optional CLOBBER)" t) (autoload 'org-ref-extract-bibtex-entries "org-ref-utils" "Extract the bibtex entries in the current buffer into a bibtex src block." t) (autoload 'org-ref-extract-cited-pdfs "org-ref-utils" "Copy PDFs in citations in current buffer to NEWDIR.

(fn NEWDIR)" t) (register-definition-prefixes "org-ref-utils" '("ords" "org-ref")) (register-definition-prefixes "org-ref-worldcat" '("worldcat-query-all")) (autoload 'wos-search "org-ref-wos" "Open the word at point or selection in Web of Science as a topic query." t) (autoload 'wos "org-ref-wos" "Open Web of Science search page in a browser." t) (register-definition-prefixes "org-ref-wos" '("*wos-" "wos-")) (autoload 'ris2bib "x2bib" "Convert RISFILE to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
If VERBOSE is non-nil show command output.
If the region is active, assume it is a ris entry
and convert it to bib format in place.

(fn RISFILE &optional VERBOSE)" t) (autoload 'medxml2bib "x2bib" "Convert MEDFILE (in Pubmed xml) to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
Display output if VERBOSE is non-nil.

(fn MEDFILE &optional VERBOSE)" t) (autoload 'clean-entries "x2bib" "Map over bibtex entries and clean them." t) (provide 'org-ref-autoloads)) "rainbow-mode" ((rainbow-mode-autoloads rainbow-mode rainbow-mode-pkg) (autoload 'rainbow-mode "rainbow-mode" "Colorize strings that represent colors.

This will fontify with colors the string like \"#aabbcc\" or \"blue\".

This is a minor mode.  If called interactively, toggle the `Rainbow
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `rainbow-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t) (register-definition-prefixes "rainbow-mode" '("rainbow-")) (provide 'rainbow-mode-autoloads)) "undo-tree" ((undo-tree-autoloads undo-tree undo-tree-pkg) (autoload 'undo-tree-mode "undo-tree" "Toggle undo-tree mode.

With no argument, this command toggles the mode.
A positive prefix argument turns the mode on.
A negative prefix argument turns it off.

Undo-tree-mode replaces Emacs' standard undo feature with a more
powerful yet easier to use version, that treats the undo history
as what it is: a tree.

The following keys are available in `undo-tree-mode':

  \\{undo-tree-mode-map}

Within the undo-tree visualizer, the following keys are available:

  \\{undo-tree-visualizer-mode-map}

This is a minor mode.  If called interactively, toggle the `Undo-Tree
mode' mode.  If the prefix argument is positive, enable the mode, and if
it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `undo-tree-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t) (put 'global-undo-tree-mode 'globalized-minor-mode t) (defvar global-undo-tree-mode nil "Non-nil if Global Undo-Tree mode is enabled.
See the `global-undo-tree-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-undo-tree-mode'.") (custom-autoload 'global-undo-tree-mode "undo-tree" nil) (autoload 'global-undo-tree-mode "undo-tree" "Toggle Undo-Tree mode in all buffers.
With prefix ARG, enable Global Undo-Tree mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Undo-Tree mode is enabled in all buffers where
`turn-on-undo-tree-mode' would do it.

See `undo-tree-mode' for more information on Undo-Tree mode.

(fn &optional ARG)" t) (register-definition-prefixes "undo-tree" '("*undo-tree-id-counter*" "buffer-undo-tree" "turn-on-undo-tree-mode" "undo-")) (provide 'undo-tree-autoloads)) "gnu-elpa-keyring-update" ((gnu-elpa-keyring-update-autoloads gnu-elpa-keyring-update gnu-elpa-keyring-update-pkg) (defvar gnu-elpa-keyring-update--keyring (expand-file-name "etc/gnu-elpa.gpg-keyring" (file-name-directory (or (if (fboundp 'macroexp-file-name) (macroexp-file-name) load-file-name) "")))) (autoload 'gnu-elpa-keyring-update "gnu-elpa-keyring-update" "Import new GNU ELPA keys (if any) into package.el's keyring.") (eval-after-load 'package `(and (bound-and-true-p package-user-dir) (file-directory-p package-user-dir) (let ((ts (expand-file-name "gnu-elpa.timestamp" (or (bound-and-true-p package-gnupghome-dir) (expand-file-name "gnupg" package-user-dir)))) (kr gnu-elpa-keyring-update--keyring)) (and (file-writable-p ts) (file-readable-p kr) (file-newer-than-file-p kr ts) (gnu-elpa-keyring-update))))) (register-definition-prefixes "gnu-elpa-keyring-update" '("gnu-elpa-keyring-update--keyring")) (provide 'gnu-elpa-keyring-update-autoloads))))

#s(hash-table test eq data (org-elpa #s(hash-table test equal data (version 15 "melpa" nil "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" nil "emacsmirror-mirror" nil "straight" nil "org" (org :type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :local-repo "org" :depth full :pre-build (straight-recipes-org-elpa--build) :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))) "pretty-hydra" nil "hydra" nil "cl-lib" nil "lv" nil "s" nil "dash" nil "compat" nil "seq" nil "citeproc" nil "f" nil "queue" nil "string-inflection" nil "parsebib" nil "org-ref" nil "htmlize" nil "avy" nil "bibtex-completion" nil "biblio" nil "biblio-core" nil "let-alist" nil "ox-pandoc" nil "ht" nil "request" nil "rainbow-mode" nil "undo-tree" nil "gnu-elpa-keyring-update" nil)) melpa #s(hash-table test equal data (version 2 "gnu-elpa-mirror" nil "nongnu-elpa" nil "el-get" (el-get :type git :flavor melpa :files (:defaults "methods" ("recipes" "recipes/el-get.rcp") "el-get-pkg.el") :host github :repo "dimitri/el-get") "emacsmirror-mirror" nil "straight" nil "citeproc" (citeproc :type git :flavor melpa :host github :repo "andras-simonyi/citeproc-el") "dash" (dash :type git :flavor melpa :files ("dash.el" "dash.texi" "dash-pkg.el") :host github :repo "magnars/dash.el") "s" (s :type git :flavor melpa :host github :repo "magnars/s.el") "f" (f :type git :flavor melpa :host github :repo "rejeep/f.el") "queue" nil "cl-lib" nil "string-inflection" (string-inflection :type git :flavor melpa :host github :repo "akicho8/string-inflection") "parsebib" (parsebib :type git :flavor melpa :host github :repo "joostkremers/parsebib") "compat" nil "seq" nil "org-ref" (org-ref :type git :flavor melpa :files (:defaults "org-ref.org" "org-ref.bib" "citeproc" "org-ref-pkg.el") :host github :repo "jkitchin/org-ref") "htmlize" (htmlize :type git :flavor melpa :host github :repo "hniksic/emacs-htmlize") "hydra" (hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra") "lv" (lv :type git :flavor melpa :files ("lv.el" "lv-pkg.el") :host github :repo "abo-abo/hydra") "avy" (avy :type git :flavor melpa :host github :repo "abo-abo/avy") "bibtex-completion" (bibtex-completion :type git :flavor melpa :files ("bibtex-completion.el" "bibtex-completion-pkg.el") :host github :repo "tmalsburg/helm-bibtex") "biblio" (biblio :type git :flavor melpa :files (:defaults (:exclude "biblio-core.el") "biblio-pkg.el") :host github :repo "cpitclaudel/biblio.el") "biblio-core" (biblio-core :type git :flavor melpa :files ("biblio-core.el" "biblio-core-pkg.el") :host github :repo "cpitclaudel/biblio.el") "let-alist" nil "ox-pandoc" (ox-pandoc :type git :flavor melpa :host github :repo "emacsorphanage/ox-pandoc") "ht" (ht :type git :flavor melpa :host github :repo "Wilfred/ht.el") "request" (request :type git :flavor melpa :files ("request.el" "request-pkg.el") :host github :repo "tkf/emacs-request") "rainbow-mode" nil "undo-tree" nil "pretty-hydra" (pretty-hydra :type git :flavor melpa :files ("pretty-hydra.el" "pretty-hydra-pkg.el") :host github :repo "jerrypnz/major-mode-hydra.el") "gnu-elpa-keyring-update" nil)) gnu-elpa-mirror #s(hash-table test equal data (version 3 "nongnu-elpa" nil "emacsmirror-mirror" nil "straight" nil "cl-lib" nil "compat" (compat :type git :host github :repo "emacs-straight/compat" :files ("*" (:exclude ".git"))) "seq" (seq :type git :host github :repo "emacs-straight/seq" :files ("*" (:exclude ".git"))) "queue" (queue :type git :host github :repo "emacs-straight/queue" :files ("*" (:exclude ".git"))) "let-alist" (let-alist :type git :host github :repo "emacs-straight/let-alist" :files ("*" (:exclude ".git"))) "rainbow-mode" (rainbow-mode :type git :host github :repo "emacs-straight/rainbow-mode" :files ("*" (:exclude ".git"))) "undo-tree" (undo-tree :type git :host github :repo "emacs-straight/undo-tree" :files ("*" (:exclude ".git"))) "gnu-elpa-keyring-update" (gnu-elpa-keyring-update :type git :host github :repo "emacs-straight/gnu-elpa-keyring-update" :files ("*" (:exclude ".git"))))) nongnu-elpa #s(hash-table test equal data (version 4 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) el-get #s(hash-table test equal data (version 2 "emacsmirror-mirror" nil "straight" nil "cl-lib" nil)) emacsmirror-mirror #s(hash-table test equal data (version 2 "straight" (straight :type git :host github :repo "emacsmirror/straight") "cl-lib" nil))))

("undo-tree" "rainbow-mode" "request" "ht" "ox-pandoc" "let-alist" "biblio-core" "biblio" "bibtex-completion" "avy" "htmlize" "org-ref" "parsebib" "string-inflection" "queue" "f" "citeproc" "seq" "compat" "dash" "s" "lv" "cl-lib" "hydra" "pretty-hydra" "gnu-elpa-keyring-update" "org" "emacs" "straight" "emacsmirror-mirror" "el-get" "nongnu-elpa" "gnu-elpa-mirror" "melpa" "org-elpa")

t
