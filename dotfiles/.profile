#
# .bash_profile
#

homeFromBin() {
  command=$1
  bin=$(which $command)
  if [ -n "$bin" ]; then
    bin=$(real-path $bin)
    echo $(dirname $(dirname $bin))
  fi
}

#
# Setup PATH. Shelly apps and ~/bin.
#
appPath=~/bin/app-path
[ -f ${appPath} ] && . ${appPath}

homeBin="${HOME}/bin"
if [ -d "${homeBin}" ] ; then
  PATH="$homeBin:$PATH"
fi
sync-env-to-plist PATH

#
# Homebrew
#
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
# sbt
#
export SBT_OPTS="-Dhttp.proxyHost=proxy.bne.flitech.net -Dhttp.proxyPort=3128 -Dhttp.nonProxyHosts=localhost|127.0.0.0/8|*.flitech.net|*.fcl.internal|*.fcltest"
sync-env-to-plist SBT_OPTS

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
  export HAXE_LIBRARY_PATH="$(brew --prefix)/share/haxe/std"
  sync-env-to-plist HAXE_LIBRARY_PATH
fi

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
# Setup COOL compiler class.
#
PATH=/usr/class/cs143/cool/bin:$PATH

#
# Explicitly call the .bashrc
#
# if running bash
if [ -n "$BASH_VERSION" ]; then
  bashrc="$HOME/.bashrc"
  [ -f ${bashrc} ] && . ${bashrc}
fi

#
# Ephox etools
#
PATH="${PATH}:${HOME}/bin.etools"
