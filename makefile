.PHONY: devbox-pull devbox-copy-dotfiles

devbox-pull:
	devbox global pull ~/dotfiles/devbox/devbox.json

devbox-backup:
	cp $$DEVBOX_PROJECT_ROOT/devbox.json ~/dotfiles/devbox/

devbox-diff:
	delta ~/dotfiles/devbox/devbox.json $$DEVBOX_PROJECT_ROOT/devbox.json

