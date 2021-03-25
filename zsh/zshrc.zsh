# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

BASEDIR=$HOME/.dotfiles

if [ -f ${BASEDIR}/before.zsh ]; then
    source ${BASEDIR}/before.zsh
fi

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

source $HOME/.zsh_plugins.sh # Load zsh plugins

for file in ${BASEDIR}/zsh/features/*.zsh; do
        source $file;
done

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

if [ -f ${BASEDIR}/after.zsh ]; then
    source ${BASEDIR}/after.zsh
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
