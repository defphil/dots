# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
# End of lines configured by zsh-newuser-install
#
#

# show completion menu on successive tab press	
setopt prompt_subst
setopt always_to_end
setopt auto_menu
setopt complete_in_word

zstyle ':completion:*' list-colors ''	
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'	

autoload -Uz compinit	
compinit	

 # do not autoselect first completion entry	
unsetopt menu_complete	
unsetopt flowcontrol	

if [[ "$CASE_SENSITIVE" = true ]]; then
	zstyle ':completion:*' matcher-list 'r:|=*' 'l:|=* r:|=*'
else
	if [[ "$HYPHEN_INSENSITIVE" = true ]]; then
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z-_}={A-Za-z_-}' 'r:|=*' 'l:|=* r:|=*'
	else
		zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
	fi
fi
unset CASE_SENSITIVE HYPHEN_INSENSITIVE

alias 'ls'='ls --color=auto'

git_prompt() {
  BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/*\(.*\)/\1/')

  if [ ! -z $BRANCH ]; then
    echo -n "%F{yellow}$BRANCH"

    if [ ! -z "$(git status --short)" ]; then
      echo " %F{red}✗"
    fi

  fi
}

PS1='%B%F{green}%~%b$(git_prompt) %F{244}%# %F{reset}'
