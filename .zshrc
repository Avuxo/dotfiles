export PATH="$PATH:/usr/local/bin"

calc() {
    python -c "print $*"
}

ed() { command ed -p: "$@" }

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
# get the pid of a given process
alias pid="ps ax | grep"
# delete all the .DS_Store files in a given directory recursively in the background (macOS)
alias dsclean="find . -name '.DS_Store' -type f -delete &"
# start an http server at 127.0.0.1:8080 with ./index.html
alias localserver='python3 -m http.server 8080'
# list directories
alias lsd="echo -ne '\033[0;34m' && find . -type d -maxdepth 1 -not -name '.' | sed -e 's/\.\///' | column && echo -ne '\033[0m'"
# list emacs buffers
alias leb="emacsclient -e '(buffer-list)' | sed -E 's/>( )/\n/g' | tr -d \"#<buffer\" | sed -E 's/\( //g;s/>\)//g'"
# list npm scripts
alias scripts="cat package.json | sed -n '/\"scripts\"/,/},/ p'"
# ls -l
alias ll="ls -la"
# shorthand for curl: λ post '<json data>' <url>
alias post="curl -w '; RESP_CODE:%{response_code}' -X POST -H 'Content-Type: application/json' -d"
# notification on macos
alias notf="osascript -e 'display notification \"SIKE\"'"
# strip a given file of escape sequences
alias escs="sed -e 's/[\x01-\x1F\x7F]//g'"
# redis daemon shorthand (typing that long name is a pain in the ass and this fits more with other database daemons)
alias redisd="redis-server"
# tr delete shrothand
alias td="tr -d"
# pushd
alias pd="pushd"
# popd
alias dp="popd"
alias kw="docker system prune -a --volumes"
# single-char for most used commands
alias l="ls -l"
alias e="ec"
alias v="vim"
alias py="python3"

parse-current-branch(){
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# history
HISTFILE=~/.zsh_history
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

autoload -U colors && colors
#PS1="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg[yellow]%}%~ %{$reset_color%}%% "
setopt PROMPT_SUBST
# [~/workspace/project/lib]  (master) λ
PROMPT='%{$fg[cyan]%}[%~] %{$fg[green]%}$(parse-current-branch) %{$fg[red]%}$%{$reset_color%} '

autoload -Uz compinit && compinit

export NVM_DIR="$HOME/.nvm"
  [ -s "/usr/local/opt/nvm/nvm.sh" ] && \. "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
  [ -s "/usr/local/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/usr/local/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
