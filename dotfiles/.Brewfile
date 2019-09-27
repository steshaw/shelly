#
# Usage: brew bundle --global
#

nix = File.file?("#{ENV['HOME']}/.nix-channels")

def laptop?
  OS.mac?
end

brew 'agda' if !nix
if false then
  brew 'asdf'

  # asdf dependencies
  brew 'coreutils'
  brew 'automake'
  brew 'autoconf'
  brew 'openssl'
  brew 'libyaml'
  brew 'readline'
  brew 'libxslt'
  brew 'libtool'
  brew 'unixodbc'
  brew 'unzip'
  brew 'curl'
end

brew 'coq' if !nix

# bash
# Will rely on Linux distro's version of bash, along with completions.
if OS.mac?
  brew 'bash' if !nix
  brew 'bash-completion@2' if !nix
end

brew 'bat' if !nix
brew 'coreutils' if !nix
brew 'ctags' if !nix # exuberant ctags
brew 'elm' if false
brew 'emacs' if !nix
brew 'fd' if !nix
brew 'findutils' if !nix
brew 'fswatch' if false # Using Facebook's watchman currently.
brew 'fzf' if !nix
brew 'ghc' if !nix
brew 'gist' if !nix
brew 'git' if !nix
brew 'gitmoji' if !nix # Currently installed by Nix overlay via Yarn (in an evil way).
brew 'gpg2' if !nix
brew 'graphviz' if false
brew 'haskell-stack' if !nix
brew 'heroku-toolbelt' if false
brew 'htop' if !nix
brew 'httpie' if !nix
brew 'hub' if !nix
brew 'idris' if laptop?
brew 'imagemagick' if false
brew 'jq' if !nix
brew 'mariadb' if false
brew 'mr' if !nix
brew 'neofetch' if !nix
brew 'neovim' if !nix
brew 'nim' if false
brew 'node' if false
brew 'ocaml' if false
brew 'opam' if false
brew 'peco' if !nix
brew 'pandoc' if !nix
brew 'pijul' if false
brew 'pinentry-mac' if OS.mac?
brew 'purescript' if false
brew 'python' if false
brew 'qt' if false
brew 'rbenv' if false # Preferring asdf (when it works).

# rcm from Thoughbot.
if false
  tap 'thoughtbot/formulae'
  brew 'rcm'
end

brew 'reattach-to-user-namespace' if false
brew 'ripgrep' if !nix
brew 'rlwrap' if !nix
brew 'ruby-build' if false
brew 'rust' if false # Favour rustup.
brew 'sbt' if false
brew 'scala' if false
brew 'scalariform' if false
brew 'shellcheck' if !nix
brew 'sphinx-doc' if false
brew 'terminal-notifier'

# the_searchers -- preferring ripgrep for now.
if false
  brew 'ack' # beyondgrep.com — written in Perl 5.
  brew 'sift'
  brew 'the_silver_searcher' # aka ag.
  brew 'the_platinum_searcher' # aka pt.
  brew 'ucg' # aka Universal Code Grep.
  # Don't forget about `git grep`. Still pretty good :-).
end

brew 'tidy-html5'
brew 'tig'
brew 'tmux' if !nix
brew 'tree' if !nix

# vala
if false
  brew 'vala'
  brew 'gtk+3'
end

brew 'vim' if false
brew 'watchman' if !nix

# weechat
if false
  # Weechat needs aspell and curl to be installed first.
  brew 'aspell'
  brew 'curl'
  brew 'weechat', args: ['with-aspell', 'with-curl']
end

brew 'wget' if !nix
brew 'yarn' if !nix
brew 'youtube-dl' if !nix
brew 'zsh' if !nix

#############################################

if OS.mac?
  cask '1password'
  cask 'adobe-digital-editions'
  cask 'alacritty' if false
  cask 'android-file-transfer'
  cask 'aquamacs' if false
  cask 'atom' if false
  cask 'balenaetcher' if false
  cask 'caffeine'
  cask 'calibre'
  cask 'docker'
  cask 'dropbox'
  # emacs-mac
  if false
    tap 'railwaycat/emacsmacport'
    cask 'emacs-mac-spacemacs-icon'
  end
  cask 'firefox'
  cask 'flux'
  cask 'font-fira-code' if false
  cask 'font-firacode-nerd-font-mono'
  if false
    cask 'font-firamono-nerd-font-mono'
    cask 'font-iosevka-nerd-font-mono'
    cask 'font-monoid-nerd-font-mono'
  end
  cask 'google-chrome'
  cask 'google-cloud-sdk'
  cask 'google-drive-file-stream'
  cask 'graphviz' if false
  cask 'horndis' if false
  cask 'iina' if false
  cask 'inkscape' if false
  cask 'integrity' if false # In favour of linkchecker.
  cask 'intellij-idea-ce' if false
  cask 'iterm2'
  cask 'java' if false
  cask 'karabiner-elements'
  cask 'key-codes' if false
  cask 'keybase'
  cask 'kindle'
  cask 'kitty' if false
  cask 'launchbar' if false
  cask 'mactex' if false
  cask 'miniconda' if false
  cask 'ngrok'
  cask 'nomachine'
  cask 'overdrive-media-console' if false # OverDrive no longer support AudiBook downloads :(.
  cask 'parallels' if false
  cask 'proximity' if false
  cask 'psequel'
  cask 'rescuetime'
  cask 'rocketcake' if false
  cask 'sequel-pro' if false
  cask 'skype'
  cask 'slack' if false # In favour of web or Spacemacs slack layer.
  cask 'smlnj'
  cask 'spectacle'
  cask 'teamviewer' if false
  cask 'telegram' if false
  cask 'virtualbox' if false
  cask 'visual-studio-code' if false
  cask 'visual-studio-code-insiders'
  cask 'vlc' if false
  cask 'xquartz' if false
  cask 'yed' if false
  cask 'zotero' if false
end

# vim: filetype=ruby
