EMACS ?= emacs

test: test-concurrent

test-concurrent:
	$(EMACS) --batch -Q -L . -l concurrent.el -l test-concurrent.el -f 'cc:test-all'
# FIXME: Remove "-l concurrent.el"

print-deps:
	@echo "----------------------- Dependencies -----------------------"
	$(EMACS) --version
	@echo "------------------------------------------------------------"

travis-ci: print-deps test
