#!/bin/sh

cd `dirname $0`                 # ファイルのある場所をカレントディレクトリにする

ln -s $PWD/../.editorconfig $HOME/
ln -s $PWD/../.zshrc $HOME/
touch $HOME/.zshrc.local.zsh
ln -s $PWD/../.latexmkrc $HOME/
ln -s $PWD/../.gitconfig $HOME/
