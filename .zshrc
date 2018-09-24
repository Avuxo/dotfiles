parse-current-branch(){
    git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ (\1)/'
}

# set up prompt
autoload -U colors && colors
setopt PROMPT_SUBST
PROMPT='%{$fg[cyan]%}┌[%n@%M] - %{$fg[green]%}[%~] $(parse-current-branch)
%{$fg[cyan]%}└%{$fg[red]%}λ%{$reset_color%} '

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
