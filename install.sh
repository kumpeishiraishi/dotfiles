#!/bin/sh

ln -s ~/dotfiles/.aspell.en.pws ~/.aspell.en.pws
ln -s ~/dotfiles/.bash_profile ~/.bash_profile
ln -s ~/dotfiles/.emacs.d/init.el ~/.emacs.d/init.el
ln -s ~/dotfiles/.zshenv ~/.zshenv
ln -s ~/dotfiles/.zshrc ~/.zshrc
ln -s ~/dotfiles/.screenrc ~/.screenrc

if [ $(uname)=='Darwin' ]
then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    brew install aspell cmigemo emacs gcc git gnuplot imagemagick nkf pandoc python3 screen stunnel ripgrep zsh zsh-completions cmake llvm hugo
    brew cask install bibdesk dropbox emacs mactex qlmarkdown selfcontrol qlstephen
fi

pip3 install numpy scipy matplotlib pandas jupyterlab Cython pip ipython

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
