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
# ↓↓↓ 18.0.2 (arm64) "Homebrew" - "OpenJDK 18.0.2" ↓↓↓
# export JAVA_HOME="`/usr/libexec/java_home`"
# ↓↓↓ 17.0.4 (arm64) "Oracle Corporation" - "GraalVM EE 22.2.0" ↓↓↓
export JAVA_HOME="`/usr/libexec/java_home -v 17`"
# ↓↓↓ 11.0.16.1 (arm64) "Homebrew" - "OpenJDK 11.0.16.1" ↓↓↓
# export JAVA_HOME="`/usr/libexec/java_home -v 11`"

# export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java11-22.2.0/Contents/Home"
export GRAAL_HOME="$HOME/Code/GraalVM/VM/graalvm-ee-java17-22.2.0/Contents/Home"

## other path elements
export NPM_LOCAL_BIN="./node_modules/.bin"
export DSCL_TOOLS="$HOME/Code/Repo/ds-cmdline/tools"
export USER_TOOLS="$NPM_LOCAL_BIN:$DSCL_TOOLS"

export PATH="$USER_TOOLS:$HOME/.local/bin:$HOME/.emacs.d/bin:$HOME/.cabal/bin:$HOME/.ghcup/bin:$GRAAL_HOME/bin:$CARGO_HOME/bin:$GO_HOME/libexec/bin:$GOBIN:$ANDROID/platform-tools:$ANDROID/tools:$PATH"

## for graalvm's llvm toolchain
export LLVM_TOOLCHAIN=$(lli --print-toolchain-path)

## rust
source "$HOME/.cargo/env"

## python
# modules
export PYTHONPATH="."
# virtualenv
export VIRTUALENVWRAPPER_PYTHON='/opt/homebrew/bin/python'
export WORKON_HOME="$HOME/.envs"
export PROJECT_HOME="$HOME/Code/Python/Projects"
source /opt/homebrew/bin/virtualenvwrapper.sh

## rbenv init
# export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
# eval "$(rbenv init -)"

## tensorflow
export TF_CPP_MIN_LOG_LEVEL=3

## other config
export PKG_CONFIG_PATH='/usr/X11/lib/pkgconfig'
export NODE_PATH='/opt/homebrew/lib/node_modules'
export RI='--format ansi -T'

## iterm2 shell integration
# test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

## homebrew init
eval "$(/opt/homebrew/bin/brew shellenv)"
# homebrew config
export HOMEBREW_NO_INSTALL_CLEANUP=1
export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"
export INCLUDE_PATH="INCLUDE_PATH:$(brew --prefix)/include"
