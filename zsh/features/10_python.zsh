if command -v pyenv 1>/dev/null 2>&1; then
    export PATH="~/.pyenv/bin:$PATH"
    eval "$(pyenv init -)"
fi

  if which pyenv-virtualenv-init > /dev/null; then
    eval "$(pyenv virtualenv-init -)";
  fi

