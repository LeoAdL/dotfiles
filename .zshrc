# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
bindkey -v
source $(brew --prefix)/opt/antidote/share/antidote/antidote.zsh
antidote load

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
alias imgcat="wezterm imgcat"
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
export VISUAL="nvim"
export EDITOR="nvim"
export LANG="en_US.UTF-8"
export BAT_THEME="Nord"
alias pinentry='pinentry-mac'
if [ -d "/opt/homebrew/opt/ruby/bin" ]; then
    export PATH=/opt/homebrew/opt/ruby/bin:$PATH
    export PATH=`gem environment gemdir`/bin:$PATH
fi
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"
export LESSOPEN="|$(brew --prefix)/bin/lesspipe.sh %s"
export PATH="/Users/leoap/.local/bin:$PATH"
export PATH="/opt/homebrew/opt/grep/libexec/gnubin:$PATH"
export PATH="/opt/homebrew/opt/openssl@3/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/openssl@3/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@3/include"
export PKG_CONFIG_PATH="/opt/homebrew/opt/openssl@3/lib/pkgconfig"
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"
export MPV_HOME="/Users/leoap/.config/mpv"
lfcd () {
    tmp="$(mktemp)"
    # `command` is needed in case `lfcd` is aliased to `lf`
    command lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp"
        if [ -d "$dir" ]; then
            if [ "$dir" != "$(pwd)" ]; then
                cd "$dir"
            fi
        fi
    fi
}
alias lf=lfcd
fb() {
    # save newline separated string into an array
    echo "$(buku -p -f 5 | column -ts$'\t' | fzf --multi)" > website
    arr=("${(f@)mapfile[website]}")
    # open each website
    for i in "${arr[@]}"; do
        index="$(echo "$i" | awk '{print $1}')"
        buku -p "$index"
        buku -o "$index"
    done
}
rga-fzf() {
    RG_PREFIX="rga --files-with-matches"
    local file
    file="$(
        FZF_DEFAULT_COMMAND="$RG_PREFIX '$1'" \
            fzf --sort --preview="[[ ! -z {} ]] && rga --pretty --context 5 {q} {}" \
            --phony -q "$1" \
            --bind "change:reload:$RG_PREFIX {q}" \
            --preview-window="70%:wrap"
    )" &&
    echo "opening $file" &&
    xdg-open "$file"
}
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
eval "$(zoxide init zsh)"
eval "$(starship init zsh)"
