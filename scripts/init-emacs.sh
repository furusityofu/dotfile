#!/bin/sh
cd `dirname $0`                 # ファイルのある場所をカレントディレクトリにする
ln -s $PWD/../.emacs.d/init.el $HOME/.emacs.d/
ln -s $PWD/../.emacs.d/conf $HOME/.emacs.d/
ln -s $PWD/../.emacs.d/snippets $HOME/.emacs.d/
ln -s $PWD/../.emacs.d/lisp $HOME/.emacs.d/
ln -s $PWD/../.emacs.d/straight $HOME/.emacs.d/
