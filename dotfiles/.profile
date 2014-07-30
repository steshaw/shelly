#
# .bash_profile
#

prependPaths() {
  for path in "$@"; do
    [[ -d ${path} ]] && PATH=${path}:${PATH}
  done
}

homeFromBin() {
  command=$1
  bin=$(which $command)
  if [ -n "$bin" ]; then
    bin=$(RealPath $bin)
    echo $(dirname $(dirname $bin))
  fi
}

#
# Setup shelly app-path.
#
shellyBin="${HOME}/Projects/shelly/bin"
if [ -d "${shellyBin}" ] ; then
  shellyPath=${shellyBin}/ShellyPath
  [ -f ${shellyPath} ] && . ${shellyPath}

  PATH="${shellyBin}:${PATH}"
fi

#
# bash-completions
#
if [ -x "$(which brew)" ]; then
  bashCompletion="$(brew --prefix)/etc/bash_completion"
else
  completions="/etc/profile.d/bash_completion.sh"
fi
if [ -f "${completions}/etc/bash_completion" ]; then
  source "${prefix}/etc/bash_completion"
fi

#
# Java.
#
if [ "$(uname)" = 'Darwin' ]; then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi
sync-env-to-plist JAVA_HOME

#
# Scala.
#
SCALA_HOME="$(homeFromBin scala)"
if [ -n $SCALA_HOME ]; then
  export SCALA_HOME
  sync-env-to-plist SCALA_HOME
fi

#
# JRebel.
#
export REBEL_HOME=/Applications/ZeroTurnaround/JRebel
export REBEL_JAR=$REBEL_HOME/jrebel.jar
export WITH_REBEL="-Drebel.license=${HOME}/Downloads/jrebel.lic -noverify -javaagent:$REBEL_JAR"
sync-env-to-plist REBEL_HOME REBEL_JAR WITH_REBEL

#
# Maven.
#
M2_HOME="$(homeFromBin mvn)"
if [ -n $M2_HOME ]; then
  export M2_HOME
  export MAVEN_OPTS="-Xmx512m -XX:MaxPermSize=128m"
  sync-env-to-plist M2_HOME
fi

#
# Sather.
#
satherHome=/home/steshaw/.shelly/apps/sather
if [ -d "$satherHome" ]; then
  export SATHER_HOME=$satherHome
  sync-env-to-plist SATHER_HOME
fi

#
# Haxe
#
if [ -x "$(which brew)" ]; then
  HAXE_STD_PATH="$(brew --prefix)/lib/haxe/std"
  if [[ -d ${HAXE_STD_PATH} ]]; then
    export HAXE_STD_PATH
    sync-env-to-plist HAXE_STD_PATH
  fi
fi

#
# NekoVM
#
#nekoLib="${HOME}/.shelly/local/nekovm-trunk/lib/neko"
#[[ -d $nekoLib ]] && export NEKOPATH=${nekoLib}

#
# RVM
#
# Load RVM into a shell session *as a function*
#
[ -s "/Users/steshaw/.rvm/scripts/rvm" ] && source "/Users/steshaw/.rvm/scripts/rvm"

#
# Antlr
#
if [[ -d ${HOME}/.shelly/local/antlr ]]; then
  alias antlr4="java -jar ${HOME}/.shelly/local/antlr/antlr-4.1-complete.jar"
  alias grun="java -cp .:${HOME}/.shelly/local/antlr/antlr-4.1-complete.jar org.antlr.v4.runtime.misc.TestRig"
fi

#
# Emacs 24
#
macEmacsBin=/Applications/Emacs.app/Contents/MacOS/bin
[[ -d $macEmacsBin ]] && PATH=${macEmacsBin}:${PATH}

#
# Homebrew
#
prependPaths /usr/local/bin /usr/local/sbin
if [ -x "$(which brew)" ]; then
  export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
  sync-env-to-plist PKG_CONFIG_PATH
fi

#
# Haskell
#
cabalBinDir=${HOME}/.cabal/bin
[ -d $cabalBinDir ] && PATH=$cabalBinDir:$PATH

haskellBinDir=~/Library/Haskell/bin
[ -d $haskellBinDir ] && PATH=$haskellBinDir:$PATH

#
# Add GHC 7.8.3 to the PATH, via http://ghcformacosx.github.io/
#
export GHC_DOT_APP="/Applications/ghc-7.8.3.app"
if [[ -d "$GHC_DOT_APP" ]]; then
  export PATH="${GHC_DOT_APP}/Contents/bin:${PATH}"
fi

#
# Add ~/bin to PATH
#
homeBin="${HOME}/bin"
if [[ -d "${homeBin}" ]]; then
  PATH="$homeBin:$PATH"
fi

#
# Nix
#
nixsh=~/.nix-profile/etc/profile.d/nix.sh
[[ -e ${nixsh} ]] && source ${nixsh}

#
# Explicitly call the .bashrc
#
# if running bash
if [[ -n "$BASH_VERSION" ]]; then
  bashrc="$HOME/.bashrc"
  [[ -f ${bashrc} ]] && source ${bashrc}
fi

sync-env-to-plist PATH
