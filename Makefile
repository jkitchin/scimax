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

# [2023-04-27 Thu] I am not real sure what this does, maybe it moves an old org out, and installs a new one?
# org-plus isn't used anymore I think. I am commenting this out for now, and maybe will remove it eventually.
#org:
#	mkdir -p elpa-`date +%F`
#	find "elpa" -name "org-plus*" -type d -exec mv {} elpa-`date +%F` \;
#	${CASK_EXEC} ${emacs} -l ${INIT}

github:
	open https://github.com/jkitchin/scimax

.PHONY:	all test package clean-elc test-melpa
