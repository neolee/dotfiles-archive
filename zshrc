## antigen init and config
source /opt/homebrew/share/antigen/antigen.zsh
antigen init $HOME/.antigenrc

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

alias linode='mosh --client=/opt/homebrew/bin/mosh-client --server=/usr/bin/mosh-server neo@paradigmx.net -- tmux a'
alias tifa='mosh --client=/opt/homebrew/bin/mosh-client --server=/usr/bin/mosh-server neo@tifa.paradigmx.net'
alias stargate='ssh neo@108.61.222.29'
alias pie='ssh wetime_op@pie.wetime.com -p 2288'

# maintenance
alias lsports='sudo lsof -nP -i4TCP | grep LISTEN'

# mac os x
alias qlf='qlmanage -p "$@" >& /dev/null'
# alias flushdns='dscacheutil -flushcache'
# alias flushdns='sudo discoveryutil udnsflushcache'
alias flushdns='sudo killall -HUP mDNSResponder'
alias rebuildreg='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

# editors
alias es='/opt/homebrew/bin/emacs --daemon'
alias esquit='/opt/homebrew/bin/emacsclient -e "(kill-emacs)"'
alias ec='/opt/homebrew/bin/emacsclient-wrapper'
alias e='mate'
# alias e='subl -n'

# misc
alias u8='unicorn -p 8000'
alias u8d='unicorn -p 8000 -D'
alias npmls='npm list --depth=0'
alias npmlsg='npm list -g --depth=0'
alias gitls='git ls-files | xargs wc -l'
alias pip-upgrade='pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U'
alias pip3-upgrade='pip3 list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip3 install -U'
alias cabal-upgrade='cabal list --simple-output --installed | awk "{print $1}" | uniq | xargs -I {} cabal install {} --reinstall'

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
fpath=(/opt/homebrew/zsh-completions $fpath)

## display some stuff when the shell starts
print "${terminfo[smul]}OS:\t$OSTYPE${terminfo[rmul]}
${terminfo[smul]}MACH:\t$MACHTYPE${terminfo[rmul]}
${terminfo[smul]}CPU:\t$CPUTYPE${terminfo[rmul]}"
