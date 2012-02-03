#
# .bash_profile
#

function homeFromBin() {
  command=$1
  bin=$(which $command)
  if [[ -n $bin ]]; then
    bin=$(real-path $bin)
    echo $(dirname $(dirname $bin))
  fi
}

#
# Setup PATH. Shelly apps and ~/bin.
#
appPath=~/bin/app-path
[[ -f ${appPath} ]] && . ${appPath}

PATH=~/bin:$PATH
sync-env-to-plist PATH

#
# Homebrew
#
if [[ -x $(which brew) ]]; then
  export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig
  sync-env-to-plist PKG_CONFIG_PATH
fi

#
# Haskell
#
cabalBinDir=~/.cabal/bin
[[ -d $cabalBinDir ]] && PATH=$cabalBinDir:$PATH

haskellBinDir=~/Library/Haskell/bin
[[ -d $haskellBinDir ]] && PATH=$haskellBinDir:$PATH

#
# Java.
#
if [[ $(uname) == 'Darwin' ]]; then
  export JAVA_HOME=$(/usr/libexec/java_home)
fi
sync-env-to-plist JAVA_HOME

#
# Scala.
#
SCALA_HOME=$(homeFromBin scala)
if [[ -n $SCALA_HOME ]]; then
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
M2_HOME=$(homeFromBin mvn)
if [[ -n $M2_HOME ]]; then
  export M2_HOME
  sync-env-to-plist M2_HOME
fi

#
# Sather.
#
satherHome=/home/steshaw/.shelly/apps/sather
if [[ -d $satherHome ]]; then
  export SATHER_HOME=$satherHome
  sync-env-to-plist SATHER_HOME
fi

#
# Haxe
#
if [[ -x $(which brew) ]]; then
  export HAXE_LIBRARY_PATH="$(brew --prefix)/share/haxe/std"
  sync-env-to-plist HAXE_LIBRARY_PATH
fi

#
# On Mac OS seem to need to explicitly call the .bashrc
#
bashrc=~/.bashrc
[[ $(uname) == 'Darwin' && -f ${bashrc} ]] && . ${bashrc}
