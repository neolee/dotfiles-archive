### environment

## iterm2 shell integration
# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

## homebrew init
eval "$(/opt/homebrew/bin/brew shellenv)"

# homebrew config
export HOMEBREW_NO_INSTALL_CLEANUP=1
export LIBRARY_PATH="$LIBRARY_PATH:$HOMEBREW_PREFIX/lib"
export INCLUDE_PATH="$INCLUDE_PATH:$HOMEBREW_PREFIX/include"

## command line editors
export EDITOR="/usr/local/bin/mate -w"
export SVN_EDITOR="/usr/local/bin/mate -w"
export LESSEDIT="/usr/local/bin/mate -l %lm %f"
# export EDITOR="/usr/local/bin/subl -w"
# export SVN_EDITOR="/usr/local/bin/subl -w"
# export LESSEDIT="/usr/local/bin/subl %f"

## environment variables
export ANDROID="$HOME/Library/Android/sdk"

export GO_HOME="$HOMEBREW_PREFIX/bin/go"
export GOPATH="$HOME/Code/Go/Packages"
export GOBIN="$GOPATH/bin"

## java config
# ↓↓↓ 18.0.2 (arm64) "Homebrew" - "OpenJDK 18.0.2" ↓↓↓
# export JAVA_HOME="`/usr/libexec/java_home`"
# ↓↓↓ 17.0.4 (arm64) "Oracle Corporation" - "GraalVM EE 22.2.0" ↓↓↓
export JAVA_HOME="`/usr/libexec/java_home -v 17`"
# ↓↓↓ 11.0.16.1 (arm64) "Homebrew" - "OpenJDK 11.0.16.1" ↓↓↓
# export JAVA_HOME="`/usr/libexec/java_home -v 11`"

# export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java11-22.2.0/Contents/Home"
export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java17-22.2.0/Contents/Home"

## other locations
export NPM_LOCAL_BIN="./node_modules/.bin"
export DSCL_TOOLS="$HOME/Code/Repo/ds-cmdline/tools"
export USER_TOOLS="$NPM_LOCAL_BIN:$DSCL_TOOLS"

## for graalvm's llvm toolchain
export LLVM_TOOLCHAIN=$($GRAAL_HOME/bin/lli --print-toolchain-path)

## python
# modules
export PYTHONPATH="."
# virtualenv
export VIRTUALENVWRAPPER_PYTHON="$HOMEBREW_PREFIX/bin/python"
export WORKON_HOME="$HOME/.envs"
export PROJECT_HOME="$HOME/Code/Python/Projects"
source $HOMEBREW_PREFIX/bin/virtualenvwrapper.sh

## rbenv init
# export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
# eval "$(rbenv init -)"

## tensorflow
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export NODE_PATH="$HOMEBREW_PREFIX/lib/node_modules"
export PKG_CONFIG_PATH="/usr/X11/lib/pkgconfig"
export RI="--format ansi -T"

## my path
# switch to gnu grep (from homebrew)
export PATH="$HOMEBREW_PREFIX/opt/grep/libexec/gnubin:$PATH"
# rust
source $HOME/.cargo/env
# other stuffs
export PATH="$USER_TOOLS:$HOME/.local/bin:$HOME/.emacs.d/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$GRAAL_HOME/bin:$GO_HOME/libexec/bin:$GOBIN:$ANDROID/platform-tools:$ANDROID/tools:$PATH"

## antigen init and config
source $HOMEBREW_PREFIX/share/antigen/antigen.zsh
antigen init $HOME/.antigenrc

### term things

## customized command alias
alias grep='grep --color'
alias rm='rm -i'
alias ls='ls -Gw'
alias l='ls -Gwl'
alias ll='ls -Gwla'
alias psgrep='ps -all -A | grep'
alias diff='colordiff'
alias x='exit'
alias git='git --no-pager'

# remote ssh shortcuts
alias zion='ssh neo@10.0.0.2 -p 12381'
alias arch='ssh neo@192.168.99.100'

alias linode="mosh --client=$HOMEBREW_PREFIX/bin/mosh-client --server=/usr/bin/mosh-server neo@paradigmx.net -- tmux a"
alias tifa="mosh --client=$HOMEBREW_PREFIX/bin/mosh-client --server=/usr/bin/mosh-server neo@tifa.paradigmx.net"
alias stargate="ssh neo@108.61.222.29"
alias pie="ssh wetime_op@pie.wetime.com -p 2288"

# maintenance
alias lsports="sudo lsof -nP -i4TCP | grep LISTEN"

# mac os x
alias qlf="qlmanage -p "$@" >& /dev/null"
# alias flushdns="dscacheutil -flushcache"
# alias flushdns="sudo discoveryutil udnsflushcache""
alias flushdns="sudo killall -HUP mDNSResponder"
alias rebuildreg="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user"

# editors
alias es="$HOMEBREW_PREFIX/bin/emacs --daemon"
alias esquit="$HOMEBREW_PREFIX/bin/emacsclient -e '(kill-emacs)'"
alias ec="$HOMEBREW_PREFIX/bin/emacsclient-wrapper"
alias e="mate"
# alias e="subl -n"

# misc
alias u8="unicorn -p 8000"
alias u8d="unicorn -p 8000 -D"
alias npmls="npm list --depth=0"
alias npmlsg="npm list -g --depth=0"
alias gitls="git ls-files | xargs wc -l"
alias pip-upgrade="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"
alias pip3-upgrade="pip3 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip3 install -U"
alias cabal-upgrade="cabal list --simple-output --installed | awk "{print $1}" | uniq | xargs -I {} cabal install {} --reinstall"

# mock display command(from imagemagick) with Preview.app
alias -g "display"="open -a Preview.app -f"

# shortcuts
alias -g "..."="../.."
alias -g "...."="../../.."
alias -g "....."="../../../.."
 
# hash -d mrp='/Users/neo/Code/Repo/mrp'

## time format
TIMEFMT='%J   %U  user %S system %P cpu %*E total'$'\n'\
'avg shared (code):         %X KB'$'\n'\
'avg unshared (data/stack): %D KB'$'\n'\
'total (sum):               %K KB'$'\n'\
'max memory:                %M KB'$'\n'\
'page faults from disk:     %F'$'\n'\
'other page faults:         %R'

## completions
fpath=(/opt/homebrew/zsh-completions $fpath)

## display some stuff when the shell starts
print "${terminfo[smul]}OS:\t$OSTYPE${terminfo[rmul]}
${terminfo[smul]}MACH:\t$MACHTYPE${terminfo[rmul]}
${terminfo[smul]}CPU:\t$CPUTYPE${terminfo[rmul]}"
