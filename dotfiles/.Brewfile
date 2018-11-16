def s(cmd)
  raise "Calling command failed with status #{$?}. Command was `#{cmd}`" unless system cmd
end

brew 'agda'
if OS.mac?
  brew 'coq'
else
  # coq is broken in Linuxbrew atm.
  puts "Installing coq ..."
  s 'sudo apt-get -qqy install coq'
end
brew 'elm'
brew 'emacs'
brew 'git'
brew 'gist'
brew 'gpg2'
brew 'haskell-stack' if OS.mac? # Broken in Linuxbrew.
brew 'hub'
brew 'htop'
brew 'httpie'
brew 'jq'
brew 'mr'
brew 'neovim'
brew 'nim'
brew 'ocaml'
brew 'peco'
brew 'pinentry-mac' if OS.mac?
brew 'python'
brew 'purescript'
brew 'ripgrep'
brew 'rlwrap'
brew 'scala'
brew 'shellcheck'
brew 'sphinx-doc'
brew 'tidy-html5'
brew 'tig'
brew 'tmux'
brew 'tree'
brew 'vim'
brew 'watchman'
brew 'zsh'

# Install vim plugins.
s 'vim-plug-install'

if OS.mac?
  brew 'bash'
  brew 'bash-completion@2'

  cask '1password'
  cask 'adobe-digital-editions'
  cask 'android-file-transfer'
  cask 'bittorrent'
  cask 'caffeine'
  cask 'docker'
  cask 'dropbox'
  cask 'firefox'
  cask 'flux'
  cask 'font-fira-code'
  cask 'google-chrome'
  cask 'google-cloud-sdk'
  cask 'google-drive-file-stream'
  cask 'iina'
  cask 'iterm2'
  cask 'java'
  cask 'karabiner-elements'
  cask 'keybase'
  cask 'kindle'
  cask 'kitty'
  cask 'psequel'
  cask 'rescuetime'
  cask 'sequel-pro'
  cask 'skype'
  cask 'spectacle'
  cask 'visual-studio-code'
  cask 'zotero'
end

# vim: filetype=ruby
