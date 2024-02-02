plugins=(zsh-autosuggestions zsh-syntax-highlighting)

alias e='emacsclient -t'
alias ec="nohup emacsclient -c --no-wait > /dev/null 2>&1"
alias er='systemctl --user restart emacs'
alias ll='ls -hrtl'
alias la='ls -hrtla'
# colorscript --random

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/i010750@ims.co.at/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/i010750@ims.co.at/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/i010750@ims.co.at/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/i010750@ims.co.at/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

