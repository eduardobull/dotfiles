# Path to your oh-my-zsh installation.
# sh -c "$(wget https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh -O -)"
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in $HOME/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="robbyrussell"
ZSH_THEME="custom"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=14

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
HIST_STAMPS="yyyy-mm-dd"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in $HOME/.oh-my-zsh/plugins/*)
# Custom plugins may be added to $HOME/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
# Plugins:
#	zsh-completions: git clone https://github.com/zsh-users/zsh-completions ${ZSH_CUSTOM:=~/.oh-my-zsh/custom}/plugins/zsh-completions
#	zsh-autosuggestions: git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
plugins=(zsh-completions zsh-autosuggestions rsync colored-man-pages emacs tmux git rust cargo docker docker-compose)

# zsh-completions
autoload -U compinit && compinit

# zsh-autosuggestions
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=240"
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=200
ZSH_AUTOSUGGEST_USE_ASYNC=1

# User configuration
export PATH="$HOME/bin:$HOME/.local/bin:$PATH"

# Oh-My-Zsh
source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -x `which emacs` ]]; then
	export EDITOR='emacs'
elif [[ -x `which vim` ]]; then
	export EDITOR='vim'
elif [[ -x `which nano` ]]; then
	export EDITOR='nano'
fi

# Disable terminal flow control keystrokes <Ctrl-S> and <Ctrl-Q>
stty -ixon

# Share history only after shell exits
unsetopt INC_APPEND_HISTORY

# Disable auto 'cd'
unsetopt AUTO_CD

# Disable history expansion ("!")
setopt nobanghist

# Use Emacs bindings
bindkey -e

# Rehash on completion
# zstyle ":completion:*:commands" rehash 1

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="$HOME/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# alias tmux="tmux -2"
alias zstdtar="tar -I zstd -cvf"
alias ungzip="tar -xvzf"
alias untar="tar -zxvf"
alias un7z="7za x"

alias ls="ls -Fv --color --group-directories-first"
alias ll="ls -lhFv --color --group-directories-first"
alias la="ls -lhFvA --color --group-directories-first"
alias less="less -S"

alias gst="git status --short"
alias glog="git log --graph --oneline --all"
alias ga="git add -p"
alias gadd="git add -p"
alias gpush="git push"
alias gpull="git pull"

alias ..='cd ..'
alias emacs='emacs --no-window-system'
alias fd='fd --hidden --no-ignore'

if [[ -x `which aria2c` ]]; then
	alias download="aria2c --continue -x5"
elif [[ -x `which wget` ]]; then
	alias download="wget -c"
elif [[ -x `which curl` ]]; then
	alias download="curl -O"
fi

if [[ -x `which rg` ]]; then
	alias search="rg --smart-case --fixed-strings -uu"
elif [[ -x `which ag` ]]; then
	alias search="ag -C1 --literal"
fi

if [[ -x `which bat` ]]; then
	alias cat="bat -pp --theme=\"OneHalfDark\""
	alias bat="/usr/bin/bat --theme=\"OneHalfDark\""
fi

if [[ -x `which yay` ]]; then
	alias yayrm='yay -Rcns'
	alias yayorph='yay -Qtd'
fi

if [[ -x `which rsync` ]]; then
	alias copy='rsync -aP'
fi

if [[ -x `which exa` ]]; then
	export EXA_COLORS="ur=37:uw=37:ux=37:ue=37:gr=37:gw=37:gx=37:tr=37:tw=37:tx=37:su=37:sf=37:xa=37:sn=37:sb=37:df=37:ds=37:uu=37:un=37:gu=37:gn=37:da=37:in=37:bl=37:lp=37:cc=37"
	alias ls='exa -F --group-directories-first'
	alias ll='ls -lg --time-style=long-iso'
	alias la='ll -a'
	alias tree='exa --group-directories-first --ignore-glob=.git -T'
fi

if [[ -x `which dust` ]]; then
	alias dust='dust -rx'
fi

# Source local definitions
if [ -f $HOME/.localrc ]; then
	. $HOME/.localrc
fi

