EMACS ?= emacs

# Elpaca repos directory (sibling packages)
ELPACA_REPOS := $(dir $(CURDIR))

# Dependencies on load-path
LOAD_PATH := -L $(CURDIR) \
             -L $(ELPACA_REPOS)gptel \
             -L $(ELPACA_REPOS)transient/lisp

.PHONY: test test-verbose clean

test:
	$(EMACS) --batch $(LOAD_PATH) \
	  -l gptel-plus-test.el \
	  -f ert-run-tests-batch-and-exit

test-verbose:
	$(EMACS) --batch $(LOAD_PATH) \
	  -l gptel-plus-test.el \
	  --eval '(ert-run-tests-batch-and-exit t)'

clean:
	rm -f *.elc
