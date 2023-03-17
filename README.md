# 🐚 Shelly

My [dotfiles](./files/home), [shell scripts](./script), and [packages](./nix).

This is a "note to self". I don't imagine anyone else will want to install
my dotfiles 😁. It could provide some inspiration for your own dotfile
management.


## Install dotfiles

1. First, move any existing dotfiles aside. For example, on a fresh machine,
   you might need something like:

```bash
mkdir dotfiles.bup && mv .profile .bashrc .bash_logout .zshrc dotfiles.bup/
```

2. Install prerequisites

You'll need `curl`, `git`, and `stow`. For example, on Ubuntu, issue the
following command:

```bash
sudo apt install -y curl git stow
```

3. Run the bootstrap script

``` sh-session
bash <(curl -s https://raw.githubusercontent.com/steshaw/shelly/main/script/shelly-bootstrap)
```

This downloads this repo and links my dotfiles.

You can stop right here for a minimal setup.


## Install packages (optional)

1.  Install [Nix](https://nixos.org/nix)

    On NixOS, Nix is already installed. For other systems, follow the
    [official installation instructions](https://nixos.org/download.html) or
    try those that follow:

    Linux systems:

    ```bash
    sh <(curl -fsSL https://nixos.org/nix/install) --daemon --yes
    ```

    For macOS systems:

    You may need to set the umask to the traditional value:

    ```bash
    umask 022
    ```

    Install Nix with the following options:

    ```bash
    sh <(curl -L https://nixos.org/nix/install)
    ```

2.  Install [Homebrew](https://brew.sh) (macOS only)

    Homebrew is used primarily to install desktop applications.

    ```bash
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
    ```

3.  Install packages

    This will install packages.

    ```bash
    shelly-bootstrap-pkgs
    ```


## Additional setup (optional)

### SSH

Generate an SSH key:

```bash
generate-ssh-key
```

### GitHub

Authenticate with GitHub:

```bash
gh auth login
```

### Myrepos

Clone my repos

```bash
cd ~/Code && mr checkout
```

### GNU Privacy Guard

On existing machine:

  1. `git-signing-key export`
  2. `rsync-mv git-gpg-key-*.gpg new-machine:`
  3. `ssh new-machine`
  4. `git-signing-key import`
  5. `rm git-gpg-key-*.gpg`

### Doom Emacs

```bash
doom install
```
