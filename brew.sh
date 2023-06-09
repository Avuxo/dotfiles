#!/usr/bin/env bash

# I always forget what I need to install via brew and then need to stumble through.
# this reminds me what I need

# install brew itself
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

taps=(
    "d12frosted/emacs-plus"
)

# formulae that don't require any extra setup steps
autoinstalls=(
    zsh
    git
    vim
    make
    tmux
    go
    wget
    emacs-plus@30
    emacsclient
)

# formulae that have manual setup steps but I don't want to forget
manuals=(
    zsh-autosuggestions
)

for i in "${taps[@]}"; do
    brew tap $i
done

for i in "${autoinstalls[@]}"; do
    brew install $i
done

for i in "${manuals[@]}"; do
    echo "Manually install $i"
done
