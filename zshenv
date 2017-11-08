## term compatible
export TERM="xterm-256color"

## oh-my-zsh settings
export ZSH="/Users/neo/.oh-my-zsh"

## themes setting for powerlevel9k
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
POWERLEVEL9K_SHORTEN_STRATEGY="truncate_middle"
POWERLEVEL9K_SHORTEN_DIR_LENGTH=4
# POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073 %m.%d.%y}"
POWERLEVEL9K_TIME_FORMAT="%D{\uf017 %H:%M \uf073  %m-%d}"
POWERLEVEL9K_STATUS_VERBOSE=false
POWERLEVEL9K_PROMPT_ON_NEWLINE=false

ZSH_THEME="powerlevel9k/powerlevel9k"
# ZSH_THEME="agnoster"
plugins=(git)

## customized environment variables

# locales hacking, used only on special issue
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

export ANDROID="/Users/neo/Library/Android/sdk"
export GO_HOME="/usr/local/opt/go"
export ANACONDA_HOME="/Users/neo/Code/Python/Env/anaconda2"
export PYTHONPATH="/usr/local" # for Caffe2
export CABAL_HOME="/Users/neo/.cabal"
export STORM_HOME="/Users/neo/Code/Repo/storm"
export CARGO_HOME="/Users/neo/.cargo"

# manage Java VMs
export JAVA_HOME="`/usr/libexec/java_home -v 1.8.0`"

export PATH="$PATH:$HOME/.rvm/bin:$ANDROID/platform-tools:$ANDROID/tools:$GO_HOME/libexec/bin:$CABAL_HOME/bin:$CARGO_HOME/bin"

## compiler setting for Homebrew
export HOMEBREW="/usr/local"
# export LD_LIBRARY_PATH="$HOMEBREW/lib:/usr/lib"
# export DYLD_FALLBACK_LIBRARY_PATH="$HOMEBREW/lib"
# export C_INCLUDE_PATH="$HOMEBREW/include"
# export CPLUS_INCLUDE_PATH="$HOMEBREW/include"

## library for Intel MKL
# export MKL_ROOT="/opt/intel/mkl"
# export LD_LIBRARY_PATH="$MKL_ROOT/lib：$LD_LIBRARY_PATH"
# export DYLD_LIBRARY_PATH="$MKL_ROOT/lib:$DYLD_LIBRARY_PATH"

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
