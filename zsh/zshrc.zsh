BASEDIR=$HOME/.dotfiles

source $HOME/.zsh_plugins.sh # Load zsh plugins

source ${BASEDIR}/zsh/env.zsh

#######################################################################
# history
#######################################################################

HISTFILE=${BASEDIR}/.zsh_history
HISTSIZE=1000
SAVEHIST=1000

#######################################################################
# key bindings
#######################################################################

bindkey -e

if [ -f ${BASEDIR}/.zshkeys ]; then
    source ${BASEDIR}/.zshkeys
fi


source ${BASEDIR}/zsh/options.zsh
source ${BASEDIR}/zsh/functions/functions.zsh
source ${BASEDIR}/zsh/functions/fzf-functions.zsh
source ${BASEDIR}/zsh/functions/git-functions.zsh
source ${BASEDIR}/zsh/bindings.zsh
source ${BASEDIR}/zsh/alias.zsh

for file in ${BASEDIR}/zsh/features/*.zsh; do
        source $file;
done

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh


