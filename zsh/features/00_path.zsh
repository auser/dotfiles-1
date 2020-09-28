
# path, the 0 in the filename causes this to load first

# Base PATH
PATH=/usr/local/sbin:/sbin:/usr/sbin:/bin:/usr/bin:./node_modules/bin:./node_modules/.bin:./bin

# Conditional PATH additions
for path_candidate in /opt/local/sbin \
  ~/.toolbox/bin \
  /Applications/Xcode.app/Contents/Developer/usr/bin \
  /opt/local/bin \
  /usr/local/share/npm/bin \
  ~/.cabal/bin \
  ~/.cargo/bin \
  ~/.rbenv/bin \
  ~/.node/bin \
  ~/.kiex/bin \
  ~/.asdf/bin \
  $HOME/go/bin \
  /usr/local/bin/ \
  $HOME/.dotfiles/bin \
  $HOME/bin \
  $HOME/.bin \
  /usr/local/bin \
  $HOME/src/gocode/bin \
  /Applications/Postgres.app/Contents/Versions/9.4/bin
do
  if [ -d ${path_candidate} ]; then
    export PATH=${PATH}:${path_candidate}
  fi
done

# In case a plugin adds a redundant path entry, remove duplicate entries
# from PATH
#
# This snippet is from Mislav Marohnić <mislav.marohnic@gmail.com>'s
# dotfiles repo at https://github.com/mislav/dotfiles

dedupe_path() {
  typeset -a paths result
  paths=($path)

  while [[ ${#paths} -gt 0 ]]; do
    p="${paths[1]}"
    shift paths
    [[ -z ${paths[(r)$p]} ]] && result+="$p"
  done

  export PATH=${(j+:+)result}
}

export GUILE_LOAD_PATH="/usr/local/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/usr/local/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/usr/local/lib/guile/3.0/extensions"

autoload bashcompinit
