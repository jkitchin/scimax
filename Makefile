emacs ?= emacs
CASK ?= cask
CASK_EXEC ?= ${CASK} exec
EL_SOURCES = *.el
SOURCES =   ${EL_SOURCES}

INIT = init.el

test: clean
	${CASK_EXEC} ert-runner --reporter ert -l test/test.el

build:
	${CASK_EXEC} ${emacs} -batch -l ${INIT}

compile:
	${CASK_EXEC} ${emacs} -Q -batch -l ${INIT} -L "." -f batch-byte-compile *.el

clean:
	rm -f *.elc

nouser:
	${CASK_EXEC} ${emacs} -Q --eval="(setq scimax-load-user-dir nil)" -l ${INIT}

org:
	mkdir -p elpa-`date +%F`
	find "elpa" -name "org-plus*" -type d -exec mv {} elpa-`date +%F` \;
	${CASK_EXEC} ${emacs} -l ${INIT}
travis:
	open https://travis-ci.org/jkitchin/scimax

github:
	open https://github.com/jkitchin/scimax

.PHONY:	all test package clean-elc test-melpa
