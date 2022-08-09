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

export GO_HOME='/opt/homebrew/bin/go'
export GOPATH="$HOME/Code/Go/Packages"
export GOBIN="$GOPATH/bin"

## java config
# ↓↓↓ 17.0.4 (arm64) "Oracle Corporation" - "GraalVM EE 22.2.0" ↓↓↓
export JAVA_HOME="`/usr/libexec/java_home`"
# ↓↓↓ 11.0.16 (arm64) "Homebrew" - "OpenJDK 11.0.16" ↓↓↓
# export JAVA_HOME="`/usr/libexec/java_home -v 11`"

# export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java11-22.2.0/Contents/Home"
export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java17-22.2.0/Contents/Home"

## rust config
export CARGO_HOME="$HOME/.cargo"

export NPM_LOCAL_BIN="./node_modules/.bin"
export DSCL_TOOLS="$HOME/Code/Repo/ds-cmdline/tools"
export USER_TOOLS="$NPM_LOCAL_BIN:$DSCL_TOOLS"

export PATH="$HOME/.local/bin:$GRAAL_HOME/bin:$CARGO_HOME/bin:$GO_HOME/libexec/bin:$GOBIN:$ANDROID/platform-tools:$ANDROID/tools:$USER_TOOLS:$PATH"

## setting for homebrew
export HOMEBREW_NO_INSTALL_CLEANUP=1

## for graalvm's llvm toolchain
export LLVM_TOOLCHAIN=$(lli --print-toolchain-path)

## python modules
export PYTHONPATH="."

## python virtualenv
export VIRTUALENVWRAPPER_PYTHON='/opt/homebrew/bin/python3'
export WORKON_HOME="$HOME/.envs"
export PROJECT_HOME="$HOME/Code/Python/Projects"
source /opt/homebrew/bin/virtualenvwrapper.sh

## library for Intel MKL
# export MKL_ROOT='/opt/intel/mkl'
# export LD_LIBRARY_PATH="$MKL_ROOT/lib：$LD_LIBRARY_PATH"
# export DYLD_LIBRARY_PATH="$MKL_ROOT/lib:$DYLD_LIBRARY_PATH"

## tensorflow config
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export PKG_CONFIG_PATH='/usr/X11/lib/pkgconfig'
export NODE_PATH='/opt/homebrew/lib/node_modules'
export RI='--format ansi -T'
