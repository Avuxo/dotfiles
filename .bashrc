#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

###############
## FUNCTIONS ##
###############

# download a song from youtube as an mp3
yt-song() {
    YDLCURDIR=`pwd`
    cd ~/Desktop # must CD to desktop because yt-dl doesn't have an output ability for whatever reason....
    youtube-dl -x --audio-format mp3 $1
    cd $YDLCURDIR
}


#############
## ALIASES ##
#############

# fix LS in zsh I think? can't remember why this is here anymore...
alias ls="ls -FHG"
# start an emacs client with $1 as a file
alias ec="emacsclient -n"
# start in no-window mode by default
alias emacs="emacs -nw"
# ancient joke
alias vim="emacs"
# get the pid of a given process
alias pid="ps ax | grep"
# play a midi song `midi t.mid`
alias midi="fluidsynth -i ~/.midi/soundfont.sf2"
# start python2 (macOS issue)
alias python2="/usr/bin/python"
# delete all the .DS_Store files in a given directory recursively (macOS)
alias dsclean="find . -name '.DS_Store' -type f -delete"
# start an http server at 127.0.0.1:8080 with ./index.html
alias localserver='python3 -m http.server 8080'
# custom prompt - blue = 14, green = 40, red = 9
PS1="\[$(tput setaf 14)\]┌[\u@\h] - \[$(tput setaf 40)\][\w]\n\[$(tput setaf 14)\]└\[$(tput setaf 9)\]λ \[$(tput sgr0)\]"
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
