EMACS ?= emacs

CURL=curl --silent -L
ERT_URL=http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el?h=emacs-24
ERT=ert
CL_URL=https://raw.githubusercontent.com/emacsmirror/cl-lib/master/cl-lib.el
CL=cl-lib

.PHONY: test test-deferred test-concurrent compile clean print-deps travis-ci

test: test-deferred test-deferred-compiled test-concurrent test-concurrent-compiled

test-deferred:
	$(EMACS) -batch -Q -L . -l deferred.el -l test/test-deferred.el -f ert-run-tests-batch-and-exit

test-deferred-compiled: deferred.elc
	$(EMACS) -batch -Q -L . -l deferred.elc -l test/test-deferred.el -f ert-run-tests-batch-and-exit

test-concurrent:
	$(EMACS) -batch -Q -L . -l concurrent.el -l test/test-concurrent.el -f 'cc:test-all'

test-concurrent-compiled: concurrent.elc
	$(EMACS) -batch -Q -L . -l concurrent.elc -l test/test-concurrent.el -f 'cc:test-all'

compile: deferred.elc concurrent.elc

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<

clean:
	rm -rfv *.elc

print-deps:
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps
	$(MAKE) clean test
	$(MAKE) compile test
