# Shelly

My [dotfiles](./dotfiles),
[shell scripts](./scripts), and
[packages](./nix).

## Installation

### Install dotfiles

Bootstrap my dotfiles on a new machine like this:

``` sh-session
$ bash <(curl -s https://raw.githubusercontent.com/steshaw/shelly/master/scripts/bootstrap)
```

This downloads this repo and links my dotfiles. Open a new terminal.
Stop right here for a minimal setup, or install packages for a complete
setup.

### Install packages (optional)

1.  Install [Nix](https://nixos.org/nix)

    On NixOS, Nix is already installed.

    For other Linux systems:

    ``` sh-session
    $ sh <(curl -fsSL https://nixos.org/nix/install) --daemon
    ```

    For macOS systems:

    ``` sh-session
    $ umask 022
    $ sh <(curl -fsSL https://nixos.org/nix/install) --darwin-use-unencrypted-nix-store-volume --daemon
    ```

2.  Install [Homebrew](https://brew.sh) on macOS

    This is used primarily to install desktop applications via Homebrew
    Cask.

    ``` sh-session
    $ /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    ```

3.  Generate a new SSH and add to GitHub

    ``` sh-session
    $ generate-ssh-key
    $ GITHUB_TOKEN=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX github-add-key
    ```

4.  Run post-bootstrap

    This will install packages and clone my Git repos.

    ``` sh-session
    $ post-bootstrap
    ```
