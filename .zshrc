# .zshrc
# Created: 2016-03-13
# mollifierによる「少し凝った zshrc」https://gist.github.com/mollifier/4979906を改変

# 環境変数
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export EDITOR=nano

# emacs風キーバインドに
bindkey -e

# ヒストリ設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# プロンプト表示
PROMPT="%F{83}[%n@%m]%f %F{207}%~%f %{${reset_color}%}
%# "
SPROMPT="%{${bg[red]}%}correct: %R -> %r ? [n,y,a,e] %{${reset_color}%}"

# 単語の区切り文字を指定する
autoload -Uz select-word-style
select-word-style default
# ここで指定した文字は単語区切りとみなされる
# / も区切りと扱うので、^W でディレクトリ１つ分を削除できる
zstyle ':zle:*' word-chars " /=;@:{},|"
zstyle ':zle:*' word-style unspecified

########################################
# 補完
# 補完機能を有効に、zsh-completionsも
fpath=(/usr/local/share/zsh-completions $fpath)
autoload -Uz compinit
compinit

# 補完で小文字でも大文字にマッチさせる
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ../ の後は今いるディレクトリを補完しない
zstyle ':completion:*' ignore-parents parent pwd ..

# ps コマンドのプロセス名補完
zstyle ':completion:*:processes' command 'ps x -o pid,s,args'


########################################
# vcs_info
autoload -Uz vcs_info
autoload -Uz add-zsh-hook

zstyle ':vcs_info:*' formats '%F{green}(%s)-[%b]%f'
zstyle ':vcs_info:*' actionformats '%F{red}(%s)-[%b|%a]%f'

function _update_vcs_info_msg() {
    LANG=en_US.UTF-8 vcs_info
    RPROMPT="${vcs_info_msg_0_}"
}
add-zsh-hook precmd _update_vcs_info_msg


########################################
# オプション
# 日本語ファイル名を表示可能にする
setopt print_eight_bit
# beep無効に
setopt no_beep
# フローコントロールを無効に
setopt no_flow_control
# Ctrl+Dでzshを終了しない
setopt ignore_eof
# '#' 以降をコメント扱い
setopt interactive_comments
# ディレクトリ名だけでcdする、cd後は自動でls
setopt auto_cd
function chpwd() { ls -F --color=auto }
# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups
# 同時に起動したzshの間でヒストリを共有する
setopt share_history
# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups
# スペースから始まるコマンド行はヒストリに残さない
setopt hist_ignore_space
# ヒストリに保存するときに余分なスペースを削除する
setopt hist_reduce_blanks
# 高機能なワイルドカード展開を使用する
setopt extended_glob
# コマンドミス時（表示はSPROMPT）
setopt correct
# 3秒以上の場合は実行時間報告
REPORTTIME=3


########################################
# キーバインド

# ^R で履歴検索をするときに * でワイルドカードを使用出来るようにする
bindkey '^R' history-incremental-pattern-search-backward


########################################
# エイリアス
alias la='ls -a'
alias ll='ls -l'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
alias pandoc_gh='pandoc --embed-resources --standalone --highlight-style=pygments -t html5 --css=$HOME/dotfiles/.pandoc/github.css'
alias pandoc_ghm='pandoc --embed-resources --standalone --highlight-style=pygments -t html5 --css=$HOME/dotfiles/.pandoc/github.css --mathjax=$HOME/dotfiles/.pandoc/dynoload.js'
alias ghc='stack ghc'
alias ghci='stack ghci'
alias runghc='stack runghc'
alias runhaskell='stack runhaskell'

# sudo の後のコマンドでエイリアスを有効にする
alias sudo='sudo '

# グローバルエイリアス
alias -g L='| less'
alias -g G='| grep'

# C で標準出力をクリップボードにコピーする
# mollifier delta blog : http://mollifier.hatenablog.com/entry/20100317/p1
if which pbcopy >/dev/null 2>&1 ; then
    # Mac
    alias -g C='| pbcopy'
elif which xsel >/dev/null 2>&1 ; then
    # Linux
    alias -g C='| xsel --input --clipboard'
elif which putclip >/dev/null 2>&1 ; then
    # Cygwin
    alias -g C='| putclip'
fi

########################################
# OS 別の設定
case ${OSTYPE} in
    darwin*)
        #Mac用の設定
        export CLICOLOR=1
        alias ls='ls -G -F'
        ;;
    linux*)
        #Linux用の設定
        alias ls='ls -F --color=auto'
	alias open='xdg-open'
        ;;
esac

path=(
    $HOME/.pyenv/bin
    $HOME/.local/texlive/2025/bin/x86_64-linux
    $HOME/.local/bin
    /opt/intel/oneapi/compiler/latest/bin
    $path
)
export CPATH=$HOME/.local/include:$CPATH
export LD_LIBRARY_PATH=$HOME/.local/lib:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/.local/lib:$LIBRARY_PATH
eval "$(pyenv init --path)"
