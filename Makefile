IDRIS ?= idris
LIB   := tf-random

.PHONY: lib clean check clobber install rebuild docs docs-clean

lib:
	$(IDRIS) --build $(LIB).ipkg

clean:
	$(IDRIS) --clean $(LIB).ipkg
	find . -name "*~" -delete

check: clobber
	$(IDRIS) --checkpkg $(LIB).ipkg

clobber: clean docs-clean
	find . -name "*.ibc" -delete

install:
	$(IDRIS) --install $(LIB).ipkg

rebuild: clean lib

docs: build docs-clean
	$(IDRIS) --mkdoc $(LIB).ipkg \
	&& rm -rf docs >/dev/null \
	&& mv $(LIB)_doc docs

docs-clean:
	rm -rf $(LIB)_doc docs >/dev/null
