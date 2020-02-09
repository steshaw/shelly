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
brew 'idris' if !nix
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
brew 'pinentry-mac' if false && OS.mac? # Currently broken
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
brew 'speedtest-cli' if !nix
brew 'sphinx-doc' if false
brew 'terminal-notifier' if false && OS.mac? # Currently broken

# the_searchers -- preferring ripgrep for now.
if false
  brew 'ack' # beyondgrep.com — written in Perl 5.
  brew 'sift'
  brew 'the_silver_searcher' # aka ag.
  brew 'the_platinum_searcher' # aka pt.
  brew 'ucg' # aka Universal Code Grep.
  # Don't forget about `git grep`. Still pretty good :-).
end

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
  # Casks as at 09-Feb-2020.
  cask 'amethyst'
  cask 'android-platform-tools'
  cask 'brave-browser'
  cask 'caffeine'
  cask 'docker'
  cask 'dropbox'
  cask 'firefox'
  cask 'flux'
  cask 'font-firacode-nerd-font'
  cask 'font-sourcecodepro-nerd-font'
  cask 'google-chrome'
  cask 'google-cloud-sdk'
  cask 'google-drive-file-stream'
  cask 'grammarly'
  cask 'intellij-idea-ce'
  cask 'iterm2'
  cask 'karabiner-elements'
  cask 'keybase'
  cask 'kindle'
  cask 'lastpass'
  cask 'rescuetime'
  cask 'signal'
  cask 'skype'
  cask 'spectacle'
  cask 'visual-studio-code'
  cask 'zotero'

  # The best Emacs port for macOS.
  if false
    tap 'railwaycat/emacsmacport'
    cask 'emacs-mac-spacemacs-icon'
  end

  # Other fonts.
  if false
    cask 'font-iosevka-nerd-font-mono'
    cask 'font-monoid-nerd-font-mono'
  end
end

# vim: filetype=ruby
