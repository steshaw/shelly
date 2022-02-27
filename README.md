# Shelly

My [dotfiles](./files/home), [shell scripts](./scripts), and [packages](./nix).

Note that this is a "note to self". I don't imagine anyone else will want to
install my dotfiles :). It could provide some inspiration for your own
dotfile management.

## Installation

### Install dotfiles

First, move any existing dotfiles aside. For example,

```bash
mkdir dotfiles.bup && mv .profile .bashrc .bash_logout dotfiles.bup/
```

Bootstrap my dotfiles on a new machine like this:

``` sh-session
bash <(curl -s https://raw.githubusercontent.com/steshaw/shelly/main/scripts/shelly-bootstrap)
```

This downloads this repo and links my dotfiles. Open a new terminal.
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

3.  Add a new SSH key to GitHub

    First, generate the SSH key:
    ```bash
    generate-ssh-key
    ```

    Then add it to GitHub. You will require a GitHub token to achieve this
    at the command line:
    ```bash
    GITHUB_TOKEN=XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX github-add-key
    ```

4.  Run post-bootstrap

    This will install packages and clone my Git repos.

    ```bash
    post-bootstrap
    ```
