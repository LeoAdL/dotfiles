# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt

source $(brew --prefix)/opt/antidote/share/antidote/antidote.zsh

antidote load
autoload -Uz compinit && compinit

# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'


export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--layout=reverse --height 70% --info=inline --border --margin=1 --padding=1 \
    --color=fg:#e5e9f0,bg:#3b4252,hl:#81a1c1 \
    --color=fg+:#e5e9f0,bg+:#3b4252,hl+:#81a1c1 \
    --color=info:#eacb8a,prompt:#bf6069,pointer:#b48dac \
    --color=marker:#a3be8b,spinner:#b48dac,header:#a3be8b"
export FZF_PREVIEW_ADVANCED=true
alias s="kitty +kitten ssh"
alias d="kitty +kitten diff"
alias gd="git difftool --no-symlinks --dir-diff"
# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/Caskroom/miniforge/base/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/Caskroom/miniforge/base/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/Caskroom/miniforge/base/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export PATH="/Applications/MacPorts/Emacs.app/Contents/MacOS:$PATH"
export EDITOR=nvim
export LANG="en_US.UTF-8"
export DBUS_SESSION_BUS_ADDRESS="unix:path=$DBUS_LAUNCHD_SESSION_BUS_SOCKET"
export BAT_THEME="Nord"
BLK="0B" CHR="0B" DIR="04" EXE="06" REG="00" HARDLINK="06" SYMLINK="06" MISSING="00" ORPHAN="09" FIFO="06" SOCK="0B" OTHER="06"
export NNN_FCOLORS="$BLK$CHR$DIR$EXE$REG$HARDLINK$SYMLINK$MISSING$ORPHAN$FIFO$SOCK$OTHER"
export NNN_FIFO=/tmp/nnn.fifo
export NNN_PLUG='f:fzcd;h:fzhist;o:fzopen;v:preview-tui'
if [ -d "/opt/homebrew/opt/ruby/bin" ]; then
  export PATH=/opt/homebrew/opt/ruby/bin:$PATH
  export PATH=`gem environment gemdir`/bin:$PATH
fi
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
