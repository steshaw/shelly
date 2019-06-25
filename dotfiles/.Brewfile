# TODO: Change this to use Brew bundle DSL so that the shell commands are
# delayed until later (like the other DSL commands: brew, cask, tap).
def s(cmd)
  raise "Calling command failed with status #{$?}. Command was `#{cmd}`" unless system cmd
end

def laptop?
 OS.mac?
end

brew 'agda' if laptop?
brew 'asdf' if false

# coq
if laptop?
  brew 'coq'
else
  # Don't need coq on Linux for now.
  if false
    # coq is broken in Linuxbrew atm.
    puts "Installing coq ..."
    s 'sudo apt-get -qqy install coq'
  end
end

# bash
# Will rely on Linux distro's version of bash, along with completions.
if OS.mac?
  brew 'bash'
  brew 'bash-completion@2'
end

brew 'bat'
brew 'coreutils'
brew 'ctags' # exuberant ctags
brew 'elm' if laptop?
brew 'emacs'
brew 'fd'
brew 'findutils' if false
brew 'fswatch' if false # Using Facebook's watchman currently.
brew 'fzf'
brew 'ghc' if laptop?
brew 'gist'
brew 'git'
brew 'gitmoji'
brew 'gpg2'
brew 'graphviz' if false
brew 'haskell-stack' if laptop? # Broken in Linuxbrew.
brew 'heroku-toolbelt' if false
brew 'htop'
brew 'httpie'
brew 'hub'
brew 'idris' if laptop?
brew 'imagemagick' if false
brew 'jq'
brew 'mariadb' if false
brew 'mr'
brew 'neofetch'
brew 'neovim' if false
brew 'nim' if laptop?
brew 'node' if laptop?
brew 'ocaml' if laptop?
brew 'opam' if laptop?
brew 'peco'
brew 'pandoc'
brew 'pijul' if laptop?
brew 'pinentry-mac' if OS.mac?
brew 'postgresql' if laptop?
brew 'purescript' if laptop?
brew 'python'
brew 'qt' if false
brew 'rbenv' if false # Preferring asdf (when it works).

# rcm from Thoughbot.
if false
  tap 'thoughtbot/formulae'
  brew 'rcm'
end

brew 'reattach-to-user-namespace' if false
brew 'redis' if laptop?
brew 'ripgrep'
brew 'rlwrap'
brew 'ruby-build' if false
brew 'rust' if laptop?
brew 'sbt' if false
brew 'scala' if false
brew 'scalariform' if false
brew 'shellcheck'
brew 'sphinx-doc' if laptop?

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
brew 'tmux'
brew 'tree'

# vala
if false
  brew 'vala'
  brew 'gtk+3'
end

brew 'vim'
brew 'watchman'

# weechat
if false
  # Weechat needs aspell and curl to be installed first.
  brew 'aspell'
  brew 'curl'
  brew 'weechat', args: ['with-aspell', 'with-curl']
end

brew 'wget'
brew 'yarn'
brew 'youtube-dl' if laptop?
brew 'zsh'

#############################################

# Install vim plugins.
s 'vim-plug-install'

if OS.mac?
  cask '1password'
  cask 'adobe-digital-editions'
  cask 'android-file-transfer'
  cask 'aquamacs' if false
  cask 'atom' if false
  cask 'caffeine'
  cask 'calibre'
  cask 'miniconda' if false
  cask 'docker'
  cask 'dropbox'
  # emacs-mac
  if false
    tap 'railwaycat/emacsmacport'
    cask 'emacs-mac-spacemacs-icon'
  end
  cask 'firefox'
  cask 'flux'
  cask 'font-fira-code'
  cask 'google-chrome'
  cask 'google-cloud-sdk'
  cask 'google-drive-file-stream'
  cask 'graphviz' if false
  cask 'horndis' if false
  cask 'iina' if false
  cask 'intellij-idea-ce' if false
  cask 'iterm2'
  cask 'java' if false
  cask 'karabiner-elements' if false
  cask 'keybase'
  cask 'key-codes' if false
  cask 'kindle'
  cask 'kitty' if false
  cask 'launchbar' if false
  cask 'mactex' if false
  cask 'nomachine' if false
  cask 'parallels' if false
  cask 'proximity' if false
  cask 'psequel'
  cask 'rescuetime'
  cask 'sequel-pro' if false
  cask 'skype'
  cask 'spectacle'
  cask 'virtualbox' if false
  cask 'visual-studio-code'
  cask 'yed' if false
  cask 'zotero'
end

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

# vim: filetype=ruby
