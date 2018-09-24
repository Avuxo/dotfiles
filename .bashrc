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

# find files containing pattern $2 in path $1
fwith() {
    grep -rnw $1 -e $2
}

calc() {
    python -c "print $*"
}

# count lines of file
cl() {
    cat $1 | wc -l
}

#############
## ALIASES ##
#############

# fix LS in zsh I think? can't remember why this is here anymore...
alias ls="ls -FHG"
# start an emacs client with $1 as a file
alias ec="/usr/local/bin/emacsclient -n"
# start in no-window mode by default
alias emacs="emacs -nw"
# get the pid of a given process
alias pid="ps ax | grep"
# play a midi song `midi t.mid`
alias midi="fluidsynth -i ~/.midi/soundfont.sf2"
# delete all the .DS_Store files in a given directory recursively in the background (macOS)
alias dsclean="find . -name '.DS_Store' -type f -delete &"
# start an http server at 127.0.0.1:8080 with ./index.html
alias localserver='python3 -m http.server 8080'
# rot-13
alias rt="tr 'A-Za-z' 'N-ZA-Mn-za-m'"
# use pcregrep instead of crappy macgrep
alias grep="pcregrep"
alias ogrep="/usr/bin/grep"
# use gnu sed instead of awful macOS sed.
alias sed="gsed"
# clear ls = cls (not CLearScreen like on DOS)
alias cls="clear; ls"
# list directories
alias lsd="echo -ne '\033[0;34m' && find . -type d -maxdepth 1 -not -name '.' | sed -e 's/\.\///' | column && echo -ne '\033[0m'"
# node debug (for native modules)
alias nodb="lldb -- `which node`"
# force kill
alias fkill="kill -9"
# list emacs buffers
alias leb="emacsclient -e '(buffer-list)' | sed -E 's/>( )/\n/g' | tr -d \"#<buffer\" | sed -E 's/\( //g;s/>\)//g'"
# list npm scripts
alias scripts="cat package.json | sed -n '/\"scripts\"/,/},/ p'"
# ls -l
alias ll="ls -l"

# custom prompt - blue = 14, green = 40, red = 9
parse-current-branch(){
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

PS1="\[$(tput setaf 14)\]┌[\u@\h] - \[$(tput setaf 40)\][\w]\$(parse-current-branch)\n\[$(tput setaf 14)\]└\[$(tput setaf 9)\]λ \[$(tput sgr0)\]"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin:$HOME/scripts"
