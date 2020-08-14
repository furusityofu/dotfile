#!/bin/sh
cd `dirname $0`                 # ファイルのある場所をカレントディレクトリにする
EMACS_CONFIG_DIR=$HOME/.config/emacs/
ln -s $PWD/../.emacs.d/init.el $EMACS_CONFIG_DIR
ln -s $PWD/../.emacs.d/conf $EMACS_CONFIG_DIR
ln -s $PWD/../.emacs.d/snippets $EMACS_CONFIG_DIR
ln -s $PWD/../.emacs.d/lisp $EMACS_CONFIG_DIR
ln -s $PWD/../.emacs.d/straight $EMACS_CONFIG_DIR
