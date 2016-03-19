PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH
LC_ALL=en_US.UTF-8
LANG=en_US.UTF-8

alias pandoc_gh='pandoc --standalone --self-contained --highlight-style=pygments -t html5 --css=/Users/kumpeishiraishi/.pandoc/github.css'
alias pandoc_ghm='pandoc --standalone --self-contained --highlight-style=pygments -t html5 --css=/Users/kumpeishiraishi/.pandoc/github.css --mathjax=/Users/kumpeishiraishi/.pandoc/dynoload.js'
