## emacs tramp fix
[[ $TERM == 'dumb' ]] && unsetopt zle && PS1='$ ' && return

## locales
# export LC_ALL='C'
export LANG='en_US.UTF-8'
export LC_ALL='en_US.UTF-8'

## term compatible
export TERM='xterm-256color'

## iterm shell integration
if [[ -n `basename $SHELL` ]]; then
  source ~/.iterm2_shell_integration.`basename $SHELL`
else
  source ~/.iterm2_shell_integration.zsh
fi

## oh-my-zsh
export ZSH='/Users/neo/.oh-my-zsh'

# themes setting for powerlevel9k
POWERLEVEL9K_MODE='nerdfont-fontconfig'
POWERLEVEL9K_BATTERY_CHARGING='yellow'
POWERLEVEL9K_BATTERY_CHARGED='green'
POWERLEVEL9K_BATTERY_DISCONNECTED='$DEFAULT_COLOR'
POWERLEVEL9K_BATTERY_LOW_THRESHOLD='10'
POWERLEVEL9K_BATTERY_LOW_COLOR='red'
POWERLEVEL9K_BATTERY_ICON='\uf1e6 '
POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=''
POWERLEVEL9K_MULTILINE_SECOND_PROMPT_PREFIX='\uf0da'
POWERLEVEL9K_VCS_MODIFIED_BACKGROUND='yellow'
POWERLEVEL9K_VCS_UNTRACKED_BACKGROUND='yellow'
POWERLEVEL9K_HOME_ICON='\uf015 '
POWERLEVEL9K_HOME_SUB_ICON='\uf07c '
POWERLEVEL9K_FOLDER_ICON='\uf115 '
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(status os_icon context dir vcs)
# POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(time load ram background_jobs virtualenv rbenv rvm)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(background_jobs virtualenv)
POWERLEVEL9K_SHORTEN_STRATEGY='truncate_middle'
POWERLEVEL9K_SHORTEN_DIR_LENGTH=4
# POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073 %m.%d.%y}"
POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073  %m-%d}"
POWERLEVEL9K_STATUS_VERBOSE=false
POWERLEVEL9K_PROMPT_ON_NEWLINE=false

ZSH_THEME='powerlevel9k/powerlevel9k'
# ZSH_THEME='agnoster'
plugins=(git)

# oh-my-zsh init
source $ZSH/oh-my-zsh.sh

## zsh init
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
alias zion='ssh neo@10.0.0.2 -p 12381'
alias arch='ssh neo@192.168.99.100'

alias linode='mosh --client=/usr/local/bin/mosh-client --server=/usr/bin/mosh-server neo@paradigmx.net -- tmux a'
alias daito='ssh neo@144.202.78.78'

# mac os x
alias qlf='qlmanage -p "$@" >& /dev/null'
# alias flushdns='dscacheutil -flushcache'
# alias flushdns='sudo discoveryutil udnsflushcache'
alias flushdns='sudo killall -HUP mDNSResponder'
alias rebuildreg='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

# editors
alias es='/usr/local/bin/emacs --daemon'
alias esquit='/usr/local/bin/emacsclient -e "(kill-emacs)"'
alias ec='/usr/local/bin/emacsclient-wrapper'
alias e='mate'
alias s='subl -n'

# misc
alias u8='unicorn -p 8000'
alias u8d='unicorn -p 8000 -D'
alias npmls='npm list --depth=0'
alias npmlsg='npm list -g --depth=0'
alias gitls='git ls-files | xargs wc -l'
alias pip-upgrade='pip freeze --local | grep -v "^\-e" | cut -d = -f 1  | xargs pip install -U'
alias cabal-upgrade='cabal list --simple-output --installed | awk "{print $1}" | uniq | xargs -I {} cabal install {} --reinstall'
alias ihaskell='IHaskell notebook -i /usr/local/bin/ipython'

alias tf-activate='workon tensorflow'

# fix: for issue in Octopress vs. zsh
alias rake='noglob rake'

# mock display command(from imagemagick) with Preview.app
alias -g 'display'='open -a Preview.app -f'

# shortcuts
alias -g '...'='../..'
alias -g '....'='../../..'
alias -g '.....'='../../../..'
 
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

local _myhosts
_myhosts=( ${${${${(f)"$(<$HOME/.ssh/known_hosts)"}:#[0-9]*}%%\ *}%%,*} )
zstyle ':completion:*' hosts $_myhosts

## enable HOME & END key
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
