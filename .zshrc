export PATH="$PATH:/usr/local/bin"

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

calc() {
    python -c "print $*"
}

ed() {
    command ed -p: "$@"
}

oi() {
    $2 | tail -n $1 | head -n1
}

##########
## TMUX ##
##########

dtm() {
    tmux start-server
    tmux new-session -d 'dev'
    tmux new-window -n 'dnm'
    tmux new-window -n 'scr'
    tmux new-window -n 'nde'
    tmux new-window -n 'rby'
    tmux new-window -n 'pyt'
    tmux new-window -n 'rnd'
    tmux attach
}

x() {
    python -c "print hex($1)"
}

ux() {
    python -c "print int(\"$1\", 0)"
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
# use pcregrep instead of crappy macgrep
alias grep="pcregrep"
alias ogrep="/usr/bin/grep"
# list directories
alias lsd="echo -ne '\033[0;34m' && find . -type d -maxdepth 1 -not -name '.' | sed -e 's/\.\///' | column && echo -ne '\033[0m'"
# node debug (for native modules)
alias nodb="lldb -- `which node`"
# list emacs buffers
alias leb="emacsclient -e '(buffer-list)' | sed -E 's/>( )/\n/g' | tr -d \"#<buffer\" | sed -E 's/\( //g;s/>\)//g'"
# ls -l
alias ll="ls -la"
# node module descriptions
alias nmd="for dir in node_modules/*; do echo "$dir:"; npm view `echo $dir | sed 's/node_modules\///'` | grep "description"; done"
# shorthand for curl: λ post '<json data>' <url>
alias post="curl -w '; RESP_CODE:%{response_code}' -X POST -H 'Content-Type: application/json' -d"
# tmux
alias tm="TERM=\"xterm-256color\" tmux -u"
# strip a given file of escape sequences
alias escs="sed -e 's/[\x01-\x1F\x7F]//g'"
# redis daemon shorthand (typing that long name is a pain in the ass and this fits more with other database daemons)
alias redisd="redis-server"
# tr delete shrothand
alias td="tr -d"
# node version
alias nv="node --version | td 'v'"
# pushd
alias pd="pushd"
# popd
alias dp="popd"
# clear docker containers
alias kw="docker system prune -a --volumes"

# single-char for most used commands
alias l="ls -l"
alias e="ec"
alias v="vim"


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin:$HOME/scripts"

parse-current-branch(){
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

autoload -U colors && colors
#PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "
setopt PROMPT_SUBST
# [~/workspace/project/lib]  (master) λ
PROMPT='%{$fg[cyan]%}[%~] %{$fg[green]%}$(parse-current-branch) %{$fg[red]%}λ%{$reset_color%} '
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin:/usr/local/bin"

export GOPATH=$HOME/go
