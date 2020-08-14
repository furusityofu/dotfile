#!/usr/bin/env bash
if [ "$(uname)" == 'Darwin' ]; then
    OS='mac'
    echo $OS
elif [ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]; then
    OS='linux'
    echo $OS
else
    echo "Your platform ($(uname -a)) is not supported."
    exit 1
fi
curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-$OS -o ~/.local/bin/rust-analyzer
chmod +x ~/.local/bin/rust-analyzer
