## homebrew init
export HOMEBREW_PREFIX='/opt/homebrew'

# homebrew config
export HOMEBREW_NO_INSTALL_CLEANUP=1
export LIBRARY_PATH="$LIBRARY_PATH:$HOMEBREW_PREFIX/lib"
export INCLUDE_PATH="$INCLUDE_PATH:$HOMEBREW_PREFIX/include"

## command line editors
export EDITOR="/usr/local/bin/mate -w"
export SVN_EDITOR="/usr/local/bin/mate -w"
export LESSEDIT="/usr/local/bin/mate -l %lm %f"

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
export VIRTUALENVWRAPPER_SCRIPT="$HOMEBREW_PREFIX/bin/virtualenvwrapper.sh"

## tensorflow
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export NODE_PATH="$HOMEBREW_PREFIX/lib/node_modules"
export PKG_CONFIG_PATH="/usr/X11/lib/pkgconfig"
export RI="--format ansi -T"

## universal aliases
alias egrep='ggrep -E'
