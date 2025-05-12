# all .plist files in cwd
PLISTS := $(wildcard *.plist)

# destination dir
LAUNCH_AGENTS_DIR := $(HOME)/Library/LaunchAgents

# prefix for symlink names
PREFIX := ca.diegoa.

.PHONY: install

install:
	@echo "Installing LaunchAgents into $(LAUNCH_AGENTS_DIR)..."
	@for f in $(PLISTS); do \
	  echo " ↳ $$f → $(PREFIX)$$f"; \
	  ln -sf "$(CURDIR)/$$f" "$(LAUNCH_AGENTS_DIR)/$(PREFIX)$$f"; \
	done
	@echo "Done."

