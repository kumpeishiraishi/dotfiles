#!/bin/sh

ln -s ~/dotfiles/.aspell.en.pws ~/.aspell.en.pws
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -s ~/dotfiles/.zshrc ~/.zshrc

#if [ $(uname)=='Darwin' ]
#then
#    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
#fi
