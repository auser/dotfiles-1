# export WORKON_HOME=$HOME/.virtualenvs
# export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
# source /usr/local/bin/virtualenvwrapper.sh

if (command -v brew && brew list --formula | grep -c vim ) > /dev/null 2>&1; then
    # alias vim="$(brew --prefix vim)/bin/vim"
    alias vim=/usr/local/bin/vim
fi

