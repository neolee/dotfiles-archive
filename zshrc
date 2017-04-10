# iterm shell integration
source ~/.iterm2_shell_integration.`basename $SHELL`

## oh-my-zsh settings
export ZSH="/Users/neo/.oh-my-zsh"
ZSH_THEME="agnoster"
plugins=(git)
source $ZSH/oh-my-zsh.sh

## my own stuff

## customized environment variables

## locales hacking, used only on special issue
# export LC_ALL="C"
export LC_ALL="en_US.UTF-8"

# command line editors
# export GIT_SSH="/usr/local/bin/git-ssh-wrapper"
export EDITOR="/usr/local/bin/mate -w"
export SVN_EDITOR="/usr/local/bin/mate -w"
export LESSEDIT="/usr/local/bin/mate -l %lm %f"
# export EDITOR="/usr/local/bin/subl -w"
# export SVN_EDITOR="/usr/local/bin/subl -w"
# export LESSEDIT="/usr/local/bin/subl %f"

export GOPATH="/Users/neo/Code/Go/Packages"
export ANDROID="/Users/neo/Library/Android/sdk"
export CABAL_HOME="/Users/neo/.cabal"
export STORM_HOME="/Users/neo/Code/Repo/storm"
export ANACONDA_HOME="/Users/neo/Code/Anaconda3"

# using jenv to manage Java VMs
export JENV_ROOT="/Users/neo/.jenv"

export PATH="$PATH:$GOPATH/bin:$ANDROID/platform-tools:$ANDROID/tools:$JENV_ROOT/bin:$HOME/.rvm/bin:$STORM_HOME/bin:$CABAL_HOME/bin"

## compiler setting for Homebrew
export HOMEBREW="/usr/local"
# export LD_LIBRARY_PATH="$HOMEBREW/lib:/usr/lib"
# export DYLD_FALLBACK_LIBRARY_PATH="$HOMEBREW/lib"
# export C_INCLUDE_PATH="$HOMEBREW/include"
# export CPLUS_INCLUDE_PATH="$HOMEBREW/include"

## docker config
export DOCKER_HOST="tcp://192.168.99.100:2376"
export DOCKER_CERT_PATH="/Users/neo/.docker/machine/machines/default"
export DOCKER_TLS_VERIFY=1

## tensorflow config
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export PKG_CONFIG_PATH=/usr/X11/lib/pkgconfig
export NODE_PATH="/usr/local/lib/node_modules"
export RI="--format ansi -T"

unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

## customized command alias
alias grep='grep --color'
alias rm='rm -i'
alias ls='ls -Gw'
alias l='ls -Gwl'
alias ll='ls -Gwla'
alias psgrep='ps -all -A |grep'
alias diff='colordiff'
alias x='exit'

# remote ssh shortcuts
alias blackhole='ssh neo@blackhole.local'
alias zion='ssh neo@10.0.0.2 -p 12381'
alias neoix='ssh neo@192.168.99.100 -p 12810'

# alias paradigmx='ssh neo@paradigmx.net'
# alias codearena='ssh neo@codearena.org'
alias paradigmx='mosh --client=/usr/local/bin/mosh-client --server=/usr/bin/mosh-server neo@paradigmx.net -- tmux a'
alias codearena='mosh --client=/usr/local/bin/mosh-client --server=/usr/bin/mosh-server neo@codearena.org -- tmux a'

# mac os x
alias qlf='qlmanage -p "$@" >& /dev/null'
# alias flushdns='dscacheutil -flushcache'
# alias flushdns='sudo discoveryutil udnsflushcache'
alias flushdns='sudo killall -HUP mDNSResponder'
alias rebuildreg='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

# editors
alias es='/usr/local/bin/emacs --daemon'
alias esquit="/usr/local/bin/emacsclient -e '(kill-emacs)'"
alias emacs='/usr/local/bin/emacsclient -c -n'
alias e='mate'
alias s='subl -n'

# misc
alias u8='unicorn -p 8000'
alias u8d='unicorn -p 8000 -D'
alias closure-compiler='JENV_VERSION=oracle64-1.8.0 closure-compiler'
alias npmls='npm ls "$@" | grep "^[└├]" | sed "s/─┬/──/g"'
alias npmlsg='npm ls -g "$@" | grep "^[└├]" | sed "s/─┬/──/g"'
alias gitls='git ls-files | xargs wc -l'
alias pip-upgrade='pip freeze --local | grep -v "^\-e" | cut -d = -f 1  | xargs pip install -U'
alias pip_pypy-upgrade='pip_pypy freeze --local | grep -v "^\-e" | cut -d = -f 1  | xargs pip_pypy install -U'
alias cabal-upgrade="cabal list --simple-output --installed | awk '{print $1}' | uniq | xargs -I {} cabal install {} --reinstall"
alias ihaskell='IHaskell notebook -i /usr/local/bin/ipython'

alias tf-activate='source ~/Code/Python/Env/tensorflow/bin/activate'
alias tf3-activate='source ~/Code/Python/Env/tensorflow3/bin/activate'
alias tensorboard='tensorboard --logdir /tmp/tensorflow_logs/example'

# fix: for issue in Octopress vs. zsh
alias rake='noglob rake'

## zsh global alias
alias -g '...'='../..'
alias -g '....'='../../..'
alias -g '.....'='../../../..'

## zsh path alias
# hash -d mrp="/Users/neo/Code/Repo/mrp"

## zsh completion setting
fpath=(/usr/local/share/zsh-completions $fpath)

zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort access
zstyle ':completion:*' list-suffixes true
zstyle ':completion:*' matcher-list '' '+m:{a-z}={A-Z}' 'r:|[._-]=** r:|=**' 'l:|=* r:|=*'
zstyle ':completion:*' preserve-prefix '//[^/]##/'
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:::::' completer _complete _approximate
zstyle ':completion:*:approximate:*' max-errors 2
zstyle :compinstall filename '/Users/neo/.zshrc'
zstyle ':completion:*' completer _complete _prefix
zstyle ':completion::prefix-1:*' completer _complete
zstyle ':completion:incremental:*' completer _complete _correct
zstyle ':completion:predict:*' completer _complete # Completion caching
zstyle ':completion::complete:*' use-cache 1
zstyle ':completion::complete:*' cache-path ~/.zsh/cache/$HOST # Expand partial paths
zstyle ':completion:*' expand 'yes'
zstyle ':completion:*' squeeze-slashes 'yes' # Include non-hidden directories in globbed file completions
zstyle ':completion:*:complete:-command-::commands' ignored-patterns '*\~' # Separate matches into groups
zstyle ':completion:*:matches' group 'yes' # Describe each match group.
zstyle ':completion:*:descriptions' format "%B---- %d%b" # Messages/warnings format
zstyle ':completion:*:messages' format '%B%U---- %d%u%b'
zstyle ':completion:*:warnings' format '%B%U---- no match for: %d%u%b' # Describe options in full
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description '%d'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*'   force-list always

# jenv init for aotucompletion
eval "$(jenv init -)"

local _myhosts
_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

# enable HOME & END key
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
case $TERM in (xterm*)
	bindkey '\e[H' beginning-of-line
	bindkey '\e[F' end-of-line ;;
esac

## display some stuff when the shell starts
print "${terminfo[smul]}OS:\t$OSTYPE${terminfo[rmul]}
${terminfo[smul]}MACH:\t$MACHTYPE${terminfo[rmul]}
${terminfo[smul]}CPU:\t$CPUTYPE${terminfo[rmul]}"
