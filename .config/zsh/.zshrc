export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="steeef"

plugins=(zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

#  alias clear-history="echo ' ' > $HISTFILE && history -c"
alias open="xdg-open"
alias full_upgrade="sudo pacman -Syu && echo '\n\n\n' && yay -Syu --devel && echo '\n\n\n' && yarn global upgrade && echo '\n\n\n' && composer global update"
alias bat="bat -p --theme gruvbox-dark"
alias gd="git diff --patch --stat"
alias gdf='git status -s | sed -E "/(^\?\?)|(^M )/d" | awk "{ print \$2 }" | fzf --preview "bat -p --color always -l diff <(git diff --patch --stat {})"'

alias e='emacsclient -t'
alias ec="nohup emacsclient -c --no-wait > /dev/null 2>&1"
alias er='systemctl --user restart emacs'

# Clear history on start and exit
#  clear-history 2> /dev/null
# trap 'clear-history 2> /dev/null' EXIT

 # ci", ci', ci`, di", etc
 autoload -U select-quoted
 zle -N select-quoted
 for m in visual viopp; do
   for c in {a,i}{\',\",\`}; do
     bindkey -M $m $c select-quoted
   done
 done

 # ci{, ci(, ci<, di{, etc
 autoload -U select-bracketed
 zle -N select-bracketed
 for m in visual viopp; do
   for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
     bindkey -M $m $c select-bracketed
   done
 done

# colorscript --random
