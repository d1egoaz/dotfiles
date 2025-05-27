.PHONY: devbox-pull devbox-backup devbox-diff nix-office-mbp nix-personal-mbp nix-personal-mini nix-personal-server nix-switch nix-check nix-install-darwin

# Check if darwin-rebuild is available
check-darwin:
	@if ! command -v darwin-rebuild >/dev/null 2>&1; then \
		echo "❌ nix-darwin not installed. Run 'make nix-install-darwin' first."; \
		exit 1; \
	fi

nix-gc:
	nix-collect-garbage; nix-store --gc

# Install nix-darwin (run this first on macOS)
nix-install-darwin:
	@echo "🚀 Installing nix-darwin..."
	@echo "📝 Note: This will prompt for sudo password during system activation"
	sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake ./nix#$$(if [ "$$(whoami)" = "diego.albeiroalvarezzuluag" ]; then echo "office-mbp"; else echo "personal-mbp"; fi)

# macOS machines (using darwin-rebuild)
nix-office-mbp: check-darwin
	cd nix && sudo darwin-rebuild switch --flake .#office-mbp

nix-personal-mbp: check-darwin
	cd nix && sudo darwin-rebuild switch --flake .#personal-mbp

nix-personal-mini: check-darwin
	cd nix && sudo darwin-rebuild switch --flake .#personal-mini

# Linux server (Ubuntu Server + Nix - no system rebuild needed)
# Note: personal-server uses Ubuntu Server + Nix package manager,
# not NixOS, so no system rebuild is needed. Use home-manager directly.
nix-personal-server:
	@echo "Ubuntu Server + Nix detected. Use home-manager directly:"
	@echo "  nix run home-manager/release-25.05 -- switch --flake ~/dotfiles/nix#diego@personal-server"

# Auto-detect current machine and rebuild
nix-switch:
	@echo "Detecting current machine..."
	@if [ "$$(uname)" = "Darwin" ]; then \
		if ! command -v darwin-rebuild >/dev/null 2>&1; then \
			echo "❌ nix-darwin not installed. Installing now..."; \
			$(MAKE) nix-install-darwin; \
		else \
			if [ "$$(whoami)" = "diego.albeiroalvarezzuluag" ]; then \
				echo "Building for office MacBook Pro..."; \
				cd nix && sudo darwin-rebuild switch --flake .#office-mbp; \
			else \
				echo "Building for personal MacBook Pro..."; \
				cd nix && sudo darwin-rebuild switch --flake .#personal-mbp; \
			fi \
		fi \
	else \
		echo "Ubuntu Server + Nix detected. Use home-manager directly:"; \
		echo "  nix run home-manager/release-25.05 -- switch --flake ~/dotfiles/nix#diego@personal-server"; \
	fi

# Check flake configuration without building
nix-check:
	cd nix && nix flake check

# ============================================================================
# Legacy Devbox Commands (for migration reference)
# ============================================================================

devbox-pull:
	devbox global pull ~/dotfiles/devbox/devbox.json

devbox-backup:
	cp $$DEVBOX_PROJECT_ROOT/devbox.json ~/dotfiles/devbox/

devbox-diff:
	delta ~/dotfiles/devbox/devbox.json $$DEVBOX_PROJECT_ROOT/devbox.json
