# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load completions and git prompt
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    fi

    if [ -f "$HOME/Scripts/git-prompt.sh" ]; then
	. "$HOME/Scripts/git-prompt.sh"
    fi
fi

DISTRIBUTION_NAME=$(grep -E ^NAME /etc/os-release | sed -nr 's/^NAME="(.*)"/\1/p')
if [[ "$DISTRIBUTION_NAME" == *"Arch"* ]]; then
    . /usr/share/fzf/key-bindings.bash
    . /usr/share/fzf/completion.bash
elif [[ "$DISTRIBUTION_NAME" == *"Ubuntu"* ]]; then
    . /usr/share/doc/fzf/examples/key-bindings.bash
    . /usr/share/bash-completion/completions/fzf
fi

#GPG allow input of passphrase in tty
TTY=$(tty)
export GPG_TTY=$TTY

# Customize prompt
MAGENTA="\[$(tput setaf 5)\]"
RED="\[$(tput setaf 1)\]"
GREEN="\[$(tput setaf 2)\]"
BLUE="\[$(tput setaf 4)\]"
CYAN="\[$(tput setaf 6)\]"
RESET="\[$(tput sgr0)\]"
BOLD="\[$(tput bold)\]"
export GIT_PS1_SHOWDIRTYSTATE=1
export GIT_PS1_SHOWCOLORHINTS=1
export GIT_PS1_SHOWSTASHSTATE=1
export GIT_PS1_SHOWUNTRACKEDFILES=1
export GIT_PS1_SHOWUPSTREAM=("verbose" "name" "git")
export PROMPT_COMMAND='__git_ps1 "[${GREEN}${BOLD}\u${BLUE}@${RED}${BOLD}\h${RESET} ${CYAN}\w${RESET}]" "\$ " " (${MAGENTA}${BOLD}git:%s${RESET}) "'

# History settings
HISTCONTROL=ignoreboth
HISTSIZE=10000
HISTFILESIZE=5000
HISTFILE=~/.history
shopt -s "histappend" "checkwinsize" "extglob" "globstar"

# Enable forward search for interactive shell
[[ $- == *i* ]] && stty -ixon

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias diff='diff --color=auto'
if [ -x "$(command -v emacs)" ]; then
    alias e='emacs -nw'
    alias ec='emacsclient -t'
    export EDITOR='emacsclient -t'
    export VISUAL="$EDITOR"
    export COLORTERM='truecolor'
fi

# For Emacs Vterm
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    clear() {
	vterm_printf "51;Evterm-clear-scrollback"
	tput clear
    }
fi

if [[ "$INSIDE_EMACS" != 'vterm' ]]; then
    cd "$HOME" || return
fi

vterm_printf() {
    if [ -n "$TMUX" ] && { [ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ]; }; then
	# Tell tmux to pass the escape sequences through
	printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
	# GNU screen (screen, screen-257color, screen-256color-bce)
	printf "\eP\e]%s\007\e\\" "$1"
    else
	printf "\e]%s\e\\" "$1"
    fi
}

vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$HOSTNAME:$(pwd)"
}

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
	vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
	shift
    done

    vterm_printf "51;E$vterm_elisp"
}

export SSL_CERT_DIR="$HOME/.guix-home/profile/etc/ssl/certs/"
export SSL_CERT_FILE="$HOME/.guix-home/profile/etc/ssl/certs/ca-certificates.crt"
export GIT_SSL_CAINFO="$SSL_CERT_FILE"
export GUIX_LOCPATH="$HOME/.guix-home/profile/lib/locale/"
