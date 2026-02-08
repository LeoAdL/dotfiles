eval "$(/opt/homebrew/bin/brew shellenv)"

# export PATH="$PATH:/Library/TeX/texbin/"
alias python3="python"
alias rsync="/run/current-system/sw/bin/rsync"

# give a preview of commandline arguments when completing `kill`
# zstyle ':fzf-tab:complete:*:*' fzf-preview 'less ${(Q) realpath}'
# export LESSOPEN='|~/.lessfilter %s'
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
'[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap
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
zstyle ':fzf-tab:complete:brew-(install|uninstall|search|info):*-argument-rest' fzf-preview 'HOMEBREW_COLOR=1 brew info $word'
zstyle ':fzf-tab:complete:-command-:*' fzf-preview \
 '(out=$(tldr --color always "$word") 2>/dev/null && echo $out) || (out=$(MANWIDTH=$FZF_PREVIEW_COLUMNS man "$word") 2>/dev/null && echo $out) || (out=$(which "$word") && echo $out) || echo "${(P) word}"'

alias -- la='eza -a'
alias -- ll='eza -l'
alias -- lla='eza -la'
alias -- ls=eza
alias -- lt='eza --tree'

# History options should be set in .zshrc and after oh-my-zsh sourcing.
# See https://github.com/nix-community/home-manager/issues/177.
HISTSIZE="10000"
SAVEHIST="10000"

HISTFILE="/Users/leoap/.zsh_history"
mkdir -p "$(dirname "$HISTFILE")"

# Set shell options
set_opts=(
  HIST_FCNTL_LOCK HIST_IGNORE_DUPS HIST_IGNORE_SPACE SHARE_HISTORY
  NO_APPEND_HISTORY NO_EXTENDED_HISTORY NO_HIST_EXPIRE_DUPS_FIRST
  NO_HIST_FIND_NO_DUPS NO_HIST_IGNORE_ALL_DUPS NO_HIST_SAVE_NO_DUPS
)
for opt in "${set_opts[@]}"; do
  setopt "$opt"
done
unset opt set_opts

# ${ZDOTDIR:-~}/.zshrc

# Set the root name of the plugins files (.txt and .zsh) antidote will use.
zsh_plugins=${ZDOTDIR:-~}/.zsh_plugins

# Ensure the .zsh_plugins.txt file exists so you can add plugins.
[[ -f ${zsh_plugins}.txt ]] || touch ${zsh_plugins}.txt

# Lazy-load antidote from its functions directory.
fpath=($HOME/.antidote/functions   $fpath)
autoload -Uz antidote

# Generate a new static file whenever .zsh_plugins.txt is updated.
if [[ ! ${zsh_plugins}.zsh -nt ${zsh_plugins}.txt ]]; then
  antidote bundle <${zsh_plugins}.txt >|${zsh_plugins}.zsh
fi

# Source your static plugins file.
source ${zsh_plugins}.zsh
eval "$(starship init zsh)"
