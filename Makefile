EMACS ?= emacs
LIBS = $(wildcard subtle*.el)
BLTZ ?= ../bltzread
RENDERED = subtle-manual-rendered

.PHONY: all compile checkdoc test lint info clean

all: compile checkdoc test

compile:
	$(EMACS) --batch -L . --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(LIBS)

# checkdoc only prints; fail when any file has a complaint.
checkdoc:
	@status=0; \
	for f in $(LIBS); do \
	  out=$$($(EMACS) --batch -L . --eval '(require (quote checkdoc))' \
	    --eval "(checkdoc-file \"$$f\")" 2>&1 | grep '^subtle.*\.el:[0-9]'); \
	  if [ -n "$$out" ]; then echo "$$out"; status=1; fi; \
	done; exit $$status

test:
	$(EMACS) --batch -L . -L $(BLTZ) -l test/subtle-test.el \
	  -f ert-run-tests-batch-and-exit

# package-lint is not part of Emacs: install it first (M-x package-install).
lint:
	@$(EMACS) --batch --eval "(unless (require 'package-lint nil t) \
	  (message \"package-lint is not installed\") (kill-emacs 1))" \
	  --eval "(setq package-lint-main-file \"subtle.el\")" \
	  -L . -f package-lint-batch-and-exit $(LIBS)

# Rebuild the Info manual: render the key placeholders of the Org source
# with the default keys, then export.
info:
	$(EMACS) --batch -L . --eval "(require 'ox-texinfo)" \
	  --eval "(require 'subtle-doc)" \
	  --eval "(subtle-doc-render-file \"subtle-manual\" \"$(RENDERED).org\")" \
	  --eval "(with-current-buffer (find-file-noselect \"$(RENDERED).org\") \
	    (org-texinfo-export-to-texinfo))" \
	  && makeinfo --no-split -o subtle.info $(RENDERED).texi; \
	status=$$?; rm -f $(RENDERED).org $(RENDERED).texi; exit $$status

clean:
	rm -f *.elc $(RENDERED).org $(RENDERED).texi
