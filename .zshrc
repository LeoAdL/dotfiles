# initialize plugins statically with ${ZDOTDIR:-~}/.zsh_plugins.txt
source $(brew --prefix)/opt/antidote/share/antidote/antidote.zsh
antidote load
# fzf Config
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden --follow --exclude .git'
export FZF_DEFAULT_OPTS="--layout=reverse --height 70% --info=inline --border --margin=1 --padding=1 \
--color fg:#D8DEE9,bg:#2E3440,hl:#A3BE8C,fg+:#D8DEE9,bg+:#434C5E,hl+:#A3BE8C \
--color pointer:#BF616A,info:#4C566A,spinner:#4C566A,header:#4C566A,prompt:#81A1C1,marker:#EBCB8B"
export FZF_PREVIEW_ADVANCED=true
# Preview file content using bat (https://github.com/sharkdp/bat)

# fzf shortcuts
export FZF_CTRL_T_COMMAND="fd --type f --strip-cwd-prefix --hidden --follow --exclude .git"
export FZF_CTRL_T_OPTS="
--preview 'bat --color=always --style=numbers {}'
--bind 'ctrl-/:change-preview-window(down|hidden|)'"
export FZF_CTRL_R_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_R_OPTS="
--preview 'echo {}' --preview-window up:3:hidden:wrap
--bind 'ctrl-/:toggle-preview'
--bind 'ctrl-y:execute-silent(echo -n {2..} | pbcopy)+abort'
--color header:italic
--header 'Press CTRL-Y to copy command into clipboard'"
export FZF_ALT_C_COMMAND="$FZF_CTRL_T_COMMAND"
export FZF_ALT_C_OPTS="--preview 'tree -C {}'"

# ZSH fzf-tab
# switch group using `,` and `.`
zstyle ':fzf-tab:*' switch-group ',' '.'

zstyle ':fzf-tab:complete:cd:*' fzf-preview 'lsd -1 --color=always $realpath' # remember to use single quote here!!!
zstyle ':fzf-tab:complete:*:*' fzf-preview 'bat --color=always --style=numbers --line-range=:500 ${(Q)realpath}'
# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
    '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'
# it is an example. you can change it
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
    'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
    'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
    'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
    'case "$group" in
	"commit tag") git show --color=always $word ;;
	*) git show --color=always $word | delta ;;
esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
    'case "$group" in
	"modified file") git diff $word | delta ;;
	"recent commit object name") git show --color=always $word | delta ;;
	*) git log --color=always $word ;;
esac'
zstyle ':fzf-tab:complete:brew-(install|uninstall|search|info):*-argument-rest' fzf-preview 'brew info $word'
zstyle ':fzf-tab:complete:tldr:argument-1' fzf-preview 'tldr --color always $word'
zstyle ':fzf-tab:complete:-command-:*' fzf-preview \
    ¦ '(out=$(tldr --color always "$word") 2>/dev/null && echo $out) || (out=$(MANWIDTH=$FZF_PREVIEW_COLUMNS man "$word") 2>/dev/null && echo $out) || (out=$(which "$word") && echo $out) || echo "${(P)word}"'
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
