export BASH_SILENCE_DEPRECATION_WARNING=1

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH"
export PATH="$PATH:/usr/local/sbin"
export PATH=/usr/local/bin:$PATH

export PATH=$PATH:~/google-cloud-sdk/bin/

alias python=/usr/local/bin/python3

source "$HOME/.cargo/env"
