export PATH=/usr/local/bin:/sbin:/usr/local/texlive/2015/bin/x86_64-darwin:/usr/sbin:$PATH
LC_ALL=en_US.UTF-8
LANG=en_US.UTF-8

alias pandoc_gh='pandoc --standalone --self-contained --highlight-style=pygments -t html5 --css=/Users/kumpeishiraishi/dotfiles/.pandoc/github.css'
alias pandoc_ghm='pandoc --standalone --self-contained --highlight-style=pygments -t html5 --css=/Users/kumpeishiraishi/dotfiles/.pandoc/github.css --mathjax=/Users/kumpeishiraishi/dotfiles/.pandoc/dynoload.js'
