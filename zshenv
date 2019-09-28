## environment

## command line editors
export EDITOR="/usr/local/bin/mate -w"
export SVN_EDITOR="/usr/local/bin/mate -w"
export LESSEDIT="/usr/local/bin/mate -l %lm %f"
# export EDITOR="/usr/local/bin/subl -w"
# export SVN_EDITOR="/usr/local/bin/subl -w"
# export LESSEDIT="/usr/local/bin/subl %f"

## environment variables
export ANDROID="$HOME/Library/Android/sdk"

export GO_HOME='/usr/local/opt/go'
export GOPATH="$HOME/Code/Go/Packages"
export GOBIN="$GOPATH/bin"

export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-19.1.1/Contents/Home"

## java config
# GraalVM installed at /Library/Java/JavaVirtualMachines and 
# has newer version than Java SE 8 (1.8.0_202)
export JAVA_HOME="`/usr/libexec/java_home -v 1.8`"
# export JAVA_HOME="`/usr/libexec/java_home -v 1.8.0_202`"

export NPM_LOCAL_BIN="./node_modules/.bin"
export DSCL_TOOLS="$HOME/Code/Repo/ds-cmdline/tools"
export USER_TOOLS="$NPM_LOCAL_BIN:$DSCL_TOOLS"

export PATH="$HOME/.local/bin:$GRAAL_HOME/bin:$GO_HOME/libexec/bin:$GOBIN:$ANDROID/platform-tools:$ANDROID/tools:$USER_TOOLS:$PATH"

## setting for homebrew
export HOMEBREW='/usr/local'
export HOMEBREW_NO_INSTALL_CLEANUP=1
# export LD_LIBRARY_PATH="$HOMEBREW/lib:/usr/lib"
# export DYLD_FALLBACK_LIBRARY_PATH="$HOMEBREW/lib"
# export C_INCLUDE_PATH="$HOMEBREW/include"
# export CPLUS_INCLUDE_PATH="$HOMEBREW/include"

## python modules
export PYTHONPATH="."

## python virtualenv
export VIRTUALENVWRAPPER_PYTHON='/usr/local/bin/python3'
export WORKON_HOME="$HOME/.envs"
export PROJECT_HOME="$HOME/Code/Python/Projects"
source /usr/local/bin/virtualenvwrapper.sh

## library for Intel MKL
# export MKL_ROOT='/opt/intel/mkl'
# export LD_LIBRARY_PATH="$MKL_ROOT/lib：$LD_LIBRARY_PATH"
# export DYLD_LIBRARY_PATH="$MKL_ROOT/lib:$DYLD_LIBRARY_PATH"

## tensorflow config
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export PKG_CONFIG_PATH='/usr/X11/lib/pkgconfig'
export NODE_PATH='/usr/local/lib/node_modules'
export RI='--format ansi -T'
