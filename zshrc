PROMPT='%1~ %# '

#export CPPFLAGS="-std=c++11"
PATH="$PATH:/opt/homebrew/bin/"
PATH="$(brew --prefix)/opt/coreutils/libexec/gnubin:$PATH"
PATH="$PATH:/usr/local/sbin"
export PATH=/usr/local/bin:$PATH

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/vegardok/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/vegardok/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/vegardok/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/vegardok/google-cloud-sdk/completion.zsh.inc'; fi


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion" # This loads nvm bash_completion

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

source $(brew --prefix nvm)/nvm.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/vegardok/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/vegardok/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/vegardok/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/vegardok/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

setopt localoptions rmstarsilent