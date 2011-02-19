# Make sure prompt is exported to subshells.
export PS1

app_path=~/bin/app-path
[[ -f ${app_path} ]] && . ${app_path}

# On Mac OS seem to need to explicitly call the .bashrc
bashrc=~/.bashrc
[[ $(uname) == 'Darwin' && -f ${bashrc} ]] && . ${bashrc}

PATH=~/bin:$PATH

if [[ $(uname) == 'Darwin' ]]; then
#  export JAVA_HOME=/Library/Java/Home
  export JAVA_HOME=$(/usr/libexec/java_home)
fi
export SCALA_HOME=~/.shelly/apps/scala
mavenDir=~/.shelly/apps/maven
if [[ -d $mavenDir ]]; then
  export M2_HOME=$mavenDir
fi
export SATHER_HOME=/home/steshaw/.shelly/apps/sather

sync-env-to-plist PATH JAVA_HOME SCALA_HOME M2_HOME
