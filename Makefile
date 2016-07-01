emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

INIT = init.el

test: clean
	${CASK_EXEC} ert-runner

unit:
	${CASK_EXEC} ${emacs} -Q -batch -L "." -l ${INIT} -l org -l org-ref.el -l test/org-ref-test.el --eval "(ert t)"


compile:
	${CASK_EXEC} ${emacs} -Q -batch -l ${INIT} -L "." -f batch-byte-compile *.el

clean:
	rm -f *.elc

.PHONY:	all test package clean-elc test-melpa
