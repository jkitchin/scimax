EMACS ?= emacs


all:
	${EMACS} -Q -l init.el test.org &

clean:
	rm -fr elpa
	${EMACS} -Q -l init.el test.org

