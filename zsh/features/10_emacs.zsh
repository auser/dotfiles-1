export PATH=/usr/local/opt/emacs-plus/bin:$PATH
alias emax='
export DISPLAY=:0.0
export LIBGL_ALWAYS_INDIRECT=1
setxkbmap -layout us
setsid emacs
exit
'
