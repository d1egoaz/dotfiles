.PHONY: devbox-pull devbox-backup devbox-diff nix-office-mbp nix-personal-mbp nix-personal-mini nix-switch nix-check nix-install-darwin nix-fmt nix-update nix-gc

# ============================================================================
# Nix System Management
# ============================================================================

nix-gc:
	nix-collect-garbage; nix-store --gc

nix-update:
	cd nix && nix flake update

nix-fmt:
	cd nix && nix fmt

nix-check:
	cd nix && nix flake check

# ============================================================================
# macOS (nix-darwin) Systems
# ============================================================================

# Install nix-darwin (run this first on macOS)
nix-install-darwin:
	@echo "🚀 Installing nix-darwin..."
	@echo "📝 Note: This will prompt for sudo password during system activation"
	sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#$$(if [ "$$(whoami)" = "diego.albeiroalvarezzuluag" ]; then echo "office-mbp"; else echo "personal-mbp"; fi)

# Individual macOS hosts
nix-office-mbp:
	$(MAKE) nix-darwin-switch HOST=office-mbp

nix-personal-mbp:
	$(MAKE) nix-darwin-switch HOST=personal-mbp

nix-personal-mini:
	$(MAKE) nix-darwin-switch HOST=personal-mini

# macOS system rebuild with diff
nix-darwin-switch:
	@echo "💻 Rebuilding Darwin system: $(HOST)"
	@PREV_GEN=$$(sudo nix-env -p /nix/var/nix/profiles/system --list-generations | awk '{print $$1}' | tail -1); \
	cd nix && sudo darwin-rebuild switch --flake .#$(HOST) --verbose; \
	NEW_GEN=$$(sudo nix-env -p /nix/var/nix/profiles/system --list-generations | awk '{print $$1}' | tail -1); \
	if [ "$$PREV_GEN" != "$$NEW_GEN" ]; then \
		echo "📦 Changes between generations $$PREV_GEN → $$NEW_GEN:"; \
		sudo nix store diff-closures /nix/var/nix/profiles/system-$$PREV_GEN-link /nix/var/nix/profiles/system-$$NEW_GEN-link; \
	else \
		echo "✅ No new generation created."; \
	fi

# ============================================================================
# Auto-Detection and Smart Rebuild
# ============================================================================

# Auto-detect current machine and rebuild
nix-switch:
	@echo "🔍 Auto-detecting macOS system..."
	@if [ "$$(uname)" = "Darwin" ]; then \
		HOST_NAME=$${HOST:-}; \
		if [ -z "$$HOST_NAME" ]; then \
			CURRENT_USER=$$(id -un); \
			if [ "$$CURRENT_USER" = "diego.albeiroalvarezzuluag" ]; then \
				HOST_NAME=office-mbp; \
			else \
				MACHINE_TYPE=$$(sysctl -n hw.model 2>/dev/null || echo "unknown"); \
				if echo "$$MACHINE_TYPE" | grep -qi "mini"; then \
					HOST_NAME=personal-mini; \
				else \
					HOST_NAME=personal-mbp; \
				fi; \
			fi; \
		fi; \
		echo "💻 Detected macOS host: $$HOST_NAME"; \
		$(MAKE) nix-darwin-switch HOST=$$HOST_NAME; \
	else \
		echo "❌ This configuration only supports macOS systems"; \
		exit 1; \
	fi

# ============================================================================
# Legacy Devbox Commands (for migration reference)
# ============================================================================

devbox-pull:
	devbox global pull ~/dotfiles/devbox/devbox.json

devbox-backup:
	cp $$DEVBOX_PROJECT_ROOT/devbox.json ~/dotfiles/devbox/

devbox-diff:
	delta ~/dotfiles/devbox/devbox.json $$DEVBOX_PROJECT_ROOT/devbox.json
