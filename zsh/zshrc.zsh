BASEDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

source ${BASEDIR}/.zsh_plugins.sh # Load zsh plugins

source ${BASEDIR}/zsh/env.zsh

#######################################################################
# history
#######################################################################

HISTFILE=~/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

#######################################################################
# key bindings
#######################################################################

bindkey -e

if [ -f ~/.zshkeys ]; then
    source ~/.zshkeys
fi


source ${BASEDIR}/zsh/options.zsh
source ${BASEDIR}/zsh/functions/functions.zsh
source ${BASEDIR}/zsh/functions/fzf-functions.zsh
source ${BASEDIR}/zsh/functions/git-functions.zsh
source ${BASEDIR}/zsh/bindings.zsh
source ${BASEDIR}/zsh/alias.zsh

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
