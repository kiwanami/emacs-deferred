EMACS ?= emacs

test: test-concurrent

test-concurrent:
	$(EMACS) --batch -Q -L . -l test-concurrent.el -f 'cc:test-all'

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
