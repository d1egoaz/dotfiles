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
	@if [ -n "$$DEVBOX_PACKAGES_DIR" ] && [ -x "$$DEVBOX_PACKAGES_DIR/bin/pinentry-mac" ]; then \
	  echo " ↳ pinentry-mac → $(DESTDIR)/pinentry-mac"; \
	  ln -sf "$$DEVBOX_PACKAGES_DIR/bin/pinentry-mac" "$(DESTDIR)/pinentry-mac"; \
	else \
	  echo " ⚠️  pinentry-mac not linked: DEVBOX_PACKAGES_DIR not set or binary missing."; \
	fi
	@echo "Done."

uninstall:
	@echo "Removing binaries from $(DESTDIR)..."
	@for f in $(BINARIES); do \
	  name=$$(basename $$f); \
	  echo " ↳ $(PREFIX)$$name"; \
	  rm -f "$(DESTDIR)/$(PREFIX)$$name"; \
	done
	@echo "Done."

