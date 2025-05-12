BINARIES := $(wildcard files/*)

# destination dir (e.g. ~/bin)
DESTDIR    := $(HOME)/.local/bin

PREFIX     :=

.PHONY: install uninstall

install:
	@echo "Installing binaries into $(DESTDIR)..."
	@mkdir -p "$(DESTDIR)"
	@for f in $(BINARIES); do \
	  name=$$(basename $$f); \
	  echo " ↳ $$name → $(DESTDIR)/$(PREFIX)$$name"; \
	  ln -sf "$(CURDIR)/$$f" "$(DESTDIR)/$(PREFIX)$$name"; \
	done
	@echo "Done."

uninstall:
	@echo "Removing binaries from $(DESTDIR)..."
	@for f in $(BINARIES); do \
	  name=$$(basename $$f); \
	  echo " ↳ $(PREFIX)$$name"; \
	  rm -f "$(DESTDIR)/$(PREFIX)$$name"; \
	done
	@echo "Done."

