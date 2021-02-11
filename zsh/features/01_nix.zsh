if [ -f /Users/alerner/.nix-profile/etc/profile.d/nix.sh ]; then
    . /Users/alerner/.nix-profile/etc/profile.d/nix.sh
fi

export NIX_PATH=darwin-config=$HOME/.nixpkgs/darwin-configuration.nix:$HOME/.nix-defexpr/channels:$NIX_PATH

#figlet roar | cowsay -n dragon | lolcat
