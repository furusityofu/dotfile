#!/bin/sh
cd `dirname $0`                 # ファイルのある場所をカレントディレクトリにする
ln -s $PWD/../.editorconfig $HOME/
ln -s $PWD/../.zshrc $HOME/
ln -s $PWD/../.gnupg/gpg.conf $HOME/.gnupg/
ln -s $PWD/../.gnupg/gpg-agent.conf $HOME/.gnupg/
touch $HOME/.zshrc.local.zsh
mkdir -p $HOME/.config/
ln -s $PWD/../.config/git/ $HOME/.config/
ln -s $PWD/../.config/bat/ $HOME/.config/
ln -s $PWD/../.config/latexmk/ $HOME/.config/
sh ./init-emacs.sh
