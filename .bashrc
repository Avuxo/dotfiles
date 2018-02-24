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
alias freeciv="freeciv-gtk2"
alias midi="fluidsynth -i ~/.midi/soundfont.sf2"

# custom prompt - blue = 14, green = 40, red = 9
PS1="\[$(tput setaf 14)\]┌[\u@\h] - \[$(tput setaf 40)\][\w]\n\[$(tput setaf 14)\]└\[$(tput setaf 9)\]λ \[$(tput sgr0)\]"
