export NIX_IGNORE_SYMLINK_STORE=1
source ~/.zsh_plugins.sh # Load zsh plugins

source ~/.dotfiles/zsh/env.zsh

source ~/.dotfiles/zsh/options.zsh
source ~/.dotfiles/zsh/functions/functions.zsh
source ~/.dotfiles/zsh/functions/fzf-functions.zsh
source ~/.dotfiles/zsh/functions/git-functions.zsh
source ~/.dotfiles/zsh/bindings.zsh
source ~/.dotfiles/zsh/alias.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ -n "$(/bin/ls ~/.zshrc.d)" ]; then
  for dotfile in ~/.zshrc.d/features/*.zsh
  do
    if [ -r "${dotfile}" ]; then
      source "${dotfile}"
    fi
  done
fi
