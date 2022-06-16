# Shelly

My [dotfiles](./files/home), [shell scripts](./scripts), and [packages](./nix).

Note that this is a "note to self". I don't imagine anyone else will want to
install my dotfiles :). It could provide some inspiration for your own
dotfile management.

## Installation

### Install dotfiles

1. First, move any existing dotfiles aside. For example,

```bash
mkdir dotfiles.bup && mv .profile .bashrc .bash_logout .zshrc dotfiles.bup/
```

2. Install prerequisites

You'll need `curl`, `git`, and `stow`. For example, on Ubuntu this will
install the prerequisites:

```bash
sudo apt install -y curl git stow
```

3. Bootstrap my dotfiles on a new machine like this:

``` sh-session
bash <(curl -s https://raw.githubusercontent.com/steshaw/shelly/main/scripts/shelly-bootstrap)
```

or
``` sh-session
bash <(curl -sSL https://is.gd/shellybootstrap)
```

This downloads this repo and links my dotfiles.

Stop right here for a minimal setup, or install packages for a complete
setup.

### Install packages (optional)

1.  Install [Nix](https://nixos.org/nix)

    On NixOS, Nix is already installed.

    For other Linux systems:

    ```bash
    sh <(curl -fsSL https://nixos.org/nix/install) --daemon
    ```

    For macOS systems:

    You may need to set the umask to the traditional value:

    ```bash
    umask 022
    ```

    Install Nix with the following options:

    ```bash
    sh <(curl -fsSL https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume --daemon
    ```

2.  Install [Homebrew](https://brew.sh) on macOS

    This is used primarily to install desktop applications via Homebrew
    Cask.

    ```bash
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    ```

4.  Install packages

    This will install packages.

    ```bash
    shelly-bootstrap-pkgs
    ```

### Set up SSH+Git+GitHub (optional)

1.  Generate an SSH key:

    ```bash
    generate-ssh-key
    ```

2.  Authenticate with GitHub:

    ```bash
    gh auth login
    ```

3.  Clone my repos

    ```bash
    cd ~/Code && mr checkout
    ```
