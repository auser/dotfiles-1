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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/alerner/.asdf/installs/python/anaconda3-5.3.1/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/alerner/.asdf/installs/python/anaconda3-5.3.1/etc/profile.d/conda.sh" ]; then
        . "/Users/alerner/.asdf/installs/python/anaconda3-5.3.1/etc/profile.d/conda.sh"
    else
        export PATH="/Users/alerner/.asdf/installs/python/anaconda3-5.3.1/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

