# Diego's Nix Configuration

A minimal, multi-machine Nix configuration using nix-darwin for macOS and NixOS for Linux.

## Machines

- **office-mbp**: Office MacBook Pro M-chip (user: diego.albeiroalvarezzuluag)
- **personal-mbp**: Personal MacBook Pro M-chip (user: diego)
- **personal-mini**: Personal Mac Mini M-chip (user: diego)
- **personal-server**: Personal Linux server (user: diego)

## Bootstrap

### macOS (nix-darwin)

First, install Nix using the Determinate Systems installer:
```bash
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install
```

Note: If you use Determinate Nix, you'll need to set `nix.enable = false;` in your darwin configuration since Determinate manages Nix itself.

Then bootstrap your machine:

**Office MacBook Pro:**
```bash
nix run nix-darwin -- switch --flake github:yourusername/dotfiles#office-mbp
```

**Personal MacBook Pro:**
```bash
nix run nix-darwin -- switch --flake github:yourusername/dotfiles#personal-mbp
```

**Personal Mac Mini:**
```bash
nix run nix-darwin -- switch --flake github:yourusername/dotfiles#personal-mini
```

### Linux (NixOS)

**Personal Server:**
```bash
sudo nixos-rebuild switch --flake github:yourusername/dotfiles#personal-server
```

## Local Development

Clone this repository and make changes:

```bash
git clone https://github.com/yourusername/dotfiles.git
cd dotfiles/nix
```

### Apply changes locally

**macOS:**
```bash
darwin-rebuild switch --flake .#office-mbp
# or
darwin-rebuild switch --flake .#personal-mbp
# or
darwin-rebuild switch --flake .#personal-mini
```

**Linux:**
```bash
sudo nixos-rebuild switch --flake .#personal-server
```

## Configuration Structure

```
nix/
├── flake.nix              # Main flake configuration
├── lib/
│   └── mksystem.nix       # System builder function
├── machines/
│   ├── macbook-pro.nix    # macOS machine config
│   ├── mac-mini.nix       # Mac Mini config (inherits from macbook-pro)
│   └── linux-server.nix   # Linux server config
└── users/
    └── diego/
        ├── darwin.nix     # macOS user config
        ├── nixos.nix      # Linux user config
        └── home-manager.nix # Home Manager config (cross-platform)
```

## Next Steps

1. Update the GitHub URL in this README with your actual repository
2. Update the email in `users/diego/home-manager.nix`
3. Adjust timezone in `machines/linux-server.nix`
4. Add more packages and configurations as needed
5. Consider moving configurations from your `stow` directory to Nix

## Adding Packages

Add packages to `users/diego/home-manager.nix` for user-level packages, or to the respective machine configuration for system-level packages.

## Migrating from Stow

You can gradually migrate configurations from your `stow` directory by:
1. Converting shell configurations to Nix
2. Moving dotfiles to Home Manager
3. Replacing Homebrew packages with Nix packages where possible
