export BASH_SILENCE_DEPRECATION_WARNING=1

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH"
export PATH="$PATH:/usr/local/sbin"
export PATH=/usr/local/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
# if [ -f '/Users/vegardok/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/vegardok/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
# if [ -f '/Users/vegardok/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/vegardok/google-cloud-sdk/completion.zsh.inc'; fi
