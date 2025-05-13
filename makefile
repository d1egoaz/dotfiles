.PHONY: devbox-pull devbox-copy-dotfiles

devbox-pull:
	devbox global pull ~/dotfiles/devbox/devbox.json

devbox-copy-dotfiles:
	cp $$DEVBOX_PROJECT_ROOT/devbox.json ~/dotfiles/devbox/
