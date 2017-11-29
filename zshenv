## environment

## locales hacking, used only on special issue
# export LC_ALL="C"
export LC_ALL="en_US.UTF-8"

## command line editors
export EDITOR="/usr/local/bin/mate -w"
export SVN_EDITOR="/usr/local/bin/mate -w"
export LESSEDIT="/usr/local/bin/mate -l %lm %f"
# export EDITOR="/usr/local/bin/subl -w"
# export SVN_EDITOR="/usr/local/bin/subl -w"
# export LESSEDIT="/usr/local/bin/subl %f"

## environment variables
export ANDROID="/Users/neo/Library/Android/sdk"
export GO_HOME="/usr/local/opt/go"
export CABAL_HOME="/Users/neo/.cabal"
export CARGO_HOME="/Users/neo/.cargo"
export STORM_HOME="/Users/neo/Code/Repo/storm"
export JAVA_HOME="`/usr/libexec/java_home -v 1.8`"

export PATH="$PATH:$ANDROID/platform-tools:$ANDROID/tools:$GO_HOME/libexec/bin:$CABAL_HOME/bin:$CARGO_HOME/bin"

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
# export TF_CPP_MIN_LOG_LEVEL=3

## other config
export PKG_CONFIG_PATH=/usr/X11/lib/pkgconfig
export NODE_PATH="/usr/local/lib/node_modules"
export RI="--format ansi -T"
