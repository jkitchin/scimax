#!/bin/bash
# Adapted from https://docs.travis-ci.com/user/multi-os/

if [[ $TRAVIS_OS_NAME == 'osx' ]]; then
    ./install-scimax-mac.sh

    # Install some custom requirements on OS X
    # e.g. brew install pyenv-virtualenv

else
    # Install some custom requirements on Linux
    curl -fsSkL https://gist.github.com/rejeep/ebcd57c3af83b049833b/raw > x.sh && source ./x.sh
    evm install $EVM_EMACS --use --skip
    cask
fi
