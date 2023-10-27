export BASH_SILENCE_DEPRECATION_WARNING=1

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion
nvm use lts/fermium

export PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH"
export PATH="$PATH:/usr/local/sbin"
export PATH=/usr/local/bin:$PATH

export PATH=$PATH:~/google-cloud-sdk/bin/

export PATH=$PATH:~/go/bin/

alias python=/usr/local/bin/python3
alias k=kubectl

export GOPATH=/Users/vegardok/repos/adventofcode/2022


# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/vegardok/Downloads/google-cloud-sdk/path.bash.inc' ]; then . '/Users/vegardok/Downloads/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/vegardok/Downloads/google-cloud-sdk/completion.bash.inc' ]; then . '/Users/vegardok/Downloads/google-cloud-sdk/completion.bash.inc'; fi
. "$HOME/.cargo/env"

export PATH="/Users/vegardok/Library/Application Support/edgedb/bin:$PATH"
