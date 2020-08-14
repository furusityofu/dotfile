#!/bin/sh
cd `dirname $0`                 # ファイルのある場所をカレントディレクトリにする
ln -s $PWD/../.editorconfig $HOME/
ln -s $PWD/../.zshrc $HOME/
touch $HOME/.zshrc.local.zsh
ln -s $PWD/../.latexmkrc $HOME/
mkdir -p $HOME/.config/git/
ln -s $PWD/../.gitconfig $HOME/.config/git/config
touch $HOME/.config/git/config.local
sh ./init-emacs.sh
