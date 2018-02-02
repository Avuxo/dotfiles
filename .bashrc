#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls="ls -FHG"
alias ec="emacsclient -n"
alias emacs="emacs -nw"
alias vim="emacs"
alias pid="ps ax | grep"

# custom prompt - blue = 14, green = 40, red = 9
PS1="\[$(tput setaf 14)\]┌[\u@\h] - \[$(tput setaf 40)\][\w]\n\[$(tput setaf 14)\]└\[$(tput setaf 9)\]λ \[$(tput sgr0)\]"


# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash ] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash ] && . /usr/local/lib/node_modules/serverless/node_modules/tabtab/.completions/sls.bash