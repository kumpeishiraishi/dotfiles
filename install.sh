#!/bin/sh

mkdir ~/.emacs.d
ln -s ~/dotfiles/.aspell.en.pws ~/.aspell.en.pws
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.screenrc ~/.screenrc

if [ $OSTYPE=='linux-gnu' ]
then
    sudo apt update
    sudo apt install git gnome-sushi clangd cmigemo aspell nkf screen zsh cmake ripgrep aspell-fr
fi

if [ $(uname)=='Darwin' ]
then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    brew install aspell cmigemo emacs gcc git gnuplot imagemagick nkf pandoc python3 screen stunnel ripgrep zsh zsh-completions cmake llvm hugo
    brew cask install bibdesk dropbox emacs mactex qlmarkdown selfcontrol qlstephen
    pip3 install numpy scipy matplotlib pandas jupyterlab Cython pip ipython
fi
