module purge rocks-openmpi
module load openmpi-2.0.1_gcc-5.1.0

shopt -s autocd

alias ll='ls -lha'
alias la='ls -a'
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

export LD_LIBRARY_PATH=$HOME/.local/lib:$HOME/.local/lib64:$LD_LIBRARY_PATH
export LIBRARY_PATH=$HOME/.local/lib:$HOME/.local/lib64:$LIBRARY_PATH
export CPATH=$HOME/.local/include:$CPATH

export PATH=$HOME/.local/bin:$HOME/.pyenv/bin:$PATH
eval "$(pyenv init --path)"

export PS1="\[\033[92m\][\u@\h] \[\033[95m\]\$PWD \[\033[m\]\n\$ "

# Eternal bash history.
# ---------------------
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
export HISTFILE=~/.bash_eternal_history
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
