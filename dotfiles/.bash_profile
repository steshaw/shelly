echo Executing .bash_profile…
echo dollar0=$0

#
# Setup PATH. Shelly apps and ~/bin.
#
app_path=~/bin/app-path
[[ -f ${app_path} ]] && . ${app_path}

PATH=~/bin:$PATH
sync-env-to-plist PATH

#
# Haskell
#
#cabalBinDir=~/.cabal/bin
#[[ -d $cabalBinDir ]] && PATH=$cabalBinDir:$PATH

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
scalaBin=$(which scala)
if [[ -n $scalaBin ]]; then
  export SCALA_HOME=$(dirname $(dirname $scalaBin))
fi
sync-env-to-plist SCALA_HOME

#
# JRebel.
#
#export REBEL_HOME=~/.shelly/apps/jrebel-3.6.1
#export REBEL_JAR=$REBEL_HOME/jrebel.jar
#export WITH_REBEL="-Drebel.license=${HOME}/Downloads/javarebel.lic -noverify -javaagent:$REBEL_JAR"
#sync-env-to-plist REBEL_HOME REBEL_JAR WITH_REBEL

#
# Maven.
#
mavenDir=~/.shelly/apps/maven
if [[ -d $mavenDir ]]; then
  export M2_HOME=$mavenDir
  sync-env-to-plist PATH M2_HOME
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
export HAXE_LIBRARY_PATH="$(brew --prefix)/share/haxe/std"
sync-env-to-plist HAXE_LIBRARY_PATH

#
# On Mac OS seem to need to explicitly call the .bashrc
#
bashrc=~/.bashrc
[[ $(uname) == 'Darwin' && -f ${bashrc} ]] && . ${bashrc}
