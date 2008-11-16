# Customized environment variables
export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Home"
export SVN_EDITOR="/usr/bin/e -w"
export RI="--format ansi -T"
export MITSCHEME_LIBRARY_PATH="/opt/local/lib/mit-scheme"

# The locales, used only on special issue
export LC_ALL="C"

# Customized command alias
alias qlf='qlmanage -p "$@" >& /dev/null'
alias flushdns='dscacheutil -flushcache'

alias rm='rm -i'
alias ls='ls -Gw'
alias l='ls -Gwl'
alias ll='ls -Gwla'

alias x='exit'
##
# DELUXE-USR-LOCAL-BIN-INSERT
# (do not remove this comment)
##
echo $PATH | grep -q -s "/usr/local/bin"
if [ $? -eq 1 ] ; then
    PATH=$PATH:/usr/local/bin
    export PATH
fi
