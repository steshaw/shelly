exec >/tmp/xsession.log 2>&1

set -x

echo 'PATH {'
echo $PATH | tr : '\n'
echo '}'

PATH=~/Code/steshaw/shelly/scripts:$PATH

echo 'new PATH {'
echo $PATH | tr : '\n'
echo '}'

xserver-setup-commands

# Delegate to original xsession.
"$@"
