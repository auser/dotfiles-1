# Completions
# autoload -Uz compinit && compinit # TODO: ?
# # Matches case insensitive for lowercase
# zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
# compdef w=-redirect-,-default-,-default- # Gives w <tab> autocomplete
# #zstyle ':completion:*' insert-tab false

# Enable autocompletions
autoload -Uz compinit
typeset -i updated_at=$(date +'%j' -r ~/.zcompdump 2>/dev/null || stat -f '%Sm' -t '%j' ~/.zcompdump 2>/dev/null)
if [ $(date +'%j') != $updated_at ]; then
  compinit -i
else
  compinit -C -i
fi
zmodload -i zsh/complist

zstyle ':completion:*' menu select # select completions with arrow keys
zstyle ':completion:*' group-name '' # group results by category
zstyle ':completion:::::' completer _expand _complete _ignored _approximate # enable approximate matches for completion

IFS=$' \n\t'

# History
HISTSIZE=10000 # Lines of history to keep in memory for current session
HISTFILESIZE=10000 # Number of commands to save in the file
SAVEHIST=10000 # Number of history entries to save to disk
HISTFILE=~/.zsh_history # Where to save history to disk
HISTDUP=erase # Erase duplicates in the history file

# Options - `man zshoptions`
setopt hist_ignore_dups # Ignore duplicates
setopt hist_ignore_all_dups # remove older duplicate entries from history
setopt hist_reduce_blanks # remove superfluous blanks from history items
setopt append_history # Append history to the history file (no overwriting)
setopt share_history # Share history across terminals
setopt inc_append_history # Immediately append to the history file, not just when a term is killed
setopt extended_glob # Use extended globbing syntax
setopt auto_cd # Auto change to a dir without typing cd
setopt correct_all # autocorrect commands

setopt auto_list # automatically list choices on ambiguous completion
setopt auto_menu # automatically use menu completion
setopt always_to_end # move cursor to end if word had one match

#Colored Prompt
autoload -U promptinit
promptinit

#Colors
autoload -U colors && colors

#eval "$(git-hub alias -s)" # git -> hub. TODO: make it work with nix installed git-hub!
eval "$(direnv hook zsh)" # Direnv

export DIRENV_LOG_FORMAT= # Remove logs from direnv

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh # FZF fuzzy search

if [ $commands[kubectl] ]; then source <(kubectl completion zsh); fi
