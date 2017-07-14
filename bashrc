# If not running interactively, don't do anything. This is bad for launching remote stuff via ssh -f
[ -z "$PS1" ] && return

umask 002

alias m='less'
alias view='vim -R'
alias where='type'
alias cb='find . \( -type d -name .git -o -name node_modules \) -prune -o  -type f \( -name "*.pyc" -o -name "*.pyo" -o -name "*~" -o -name ".*~" -o -name ".#*" -o -name "*.class" -o -name "*.qc" -o -name "*.qp" \) -print0 | xargs -0 rm -f'

alias py='python'
alias cvsstat='cvs -n update -PA 2>&1 | fgrep -v "cvs update: Updating "'
#alias em="emacsclient -q -f ~/.emacs.d/server/server -n"  # See also emr function
alias em="emacsclient -q -n"  # See also emr function
alias pspy2='ps wwwaxu | egrep "^(USER|$USER.*[pP]ython)"'
#alias pspy='pgrep -u $USER python | xargs ps'
alias pspy='pgrep -u $USER [pP]ython | xargs ps -o pid,stat,time,%cpu,%mem,command -p | sed s,/System.*Python,Python,'

alias diff2diff='fgrep " differ" | sed -e "s/Files/diff -u/" -e "s/and //" -e "s/ differ//"'

alias gs='git status'
alias gd='git diff'
alias gdt='git difftool'
alias gp='git pull --ff-only'
alias ga='git add'
alias gc='git commit'
alias gco='git checkout'
alias gb='git branch'
#alias gl='git log --pretty="format:%h %aN %ad %s" --date=short'  # See also git help shortlog for .mailmap stuff
alias gl='git log --pretty="format:%C(auto)%h %s %C(blue)%aN %C(bold black)(%cr) %C(auto)%d"'
alias gll='git log --stat'

alias gdiff='git diff --no-index'

alias l='ls --color=auto -sF'
alias ls='ls --color=auto -sF'
alias la='ls --color=auto -sFa'
alias ll='ls --color=auto -sFl'
alias lla='ls --color=auto -sFla'
alias ltr='ls --color=auto -sFltr'

alias md='mkdir'
alias rd='rmdir'
alias pd='pushd'
alias od='popd'
alias d='dirs'

alias du='du -h'
alias df='df -h'

alias setxdisp='\ls -l /tmp/.X11-unix/ | fgrep $USER | sed "s/.*X/:/" > ~/.xdisplay'
alias xdisp='export DISPLAY=$(cat ~/.xdisplay)'
alias xok='printf "\n  X is working  \n\n" | xmessage -buttons Okay -default Okay -timeout 3 -file - 2>/dev/null'

function mcd {
    mkdir $1
    cd $1
}

function emr {
    # See also em alias above
    #emacsclient -q -f ~/.emacs.d/server/server --eval '(view-file "'$1'")' > /dev/null
    emacsclient -q -n --eval '(view-file "'$1'")' > /dev/null
}

function ff {
    find . \( -type f -o -type d \) -name "$1" -print
}

function ff0 {
    find . \( -type f -o -type d \) -name "$1" -print0
}

function fff {
    if [ x"$1" == x ]
    then find . -type f -print
    else find $1 -type f -print
    fi
}

function fin {
    if [ x"$1" == x ]
    then find . -type f -print
    else find . -type f -print0 | xargs -0 fgrep -- "$1" /dev/null
    fi
}

function fpy {
    if [ x"$1" == x ]
    #then find . -type f -name "*.py" -print
    then ack --python -f
    #else find . -type f -name "*.py" -print0 | xargs -0 fgrep -- "$1" /dev/null
    else ack --python "$@"
    fi
}

function fcc {
    if [ x"$1" == x ]
    then ack --cpp -f
    else ack --cpp -i "$@"
    fi
}

function fjs {
    if [ x"$1" == x ]
    #then find . \( -name "*.js" -o -name "*.json" \) -print
    then ack --js -f
    #else find . \( -name "*.js" -o -name "*.json" \) -print0 | xargs -0 fgrep -- "$1" /dev/null
    else ack --js "$@"
    fi
}

function fq {
    if [ x"$1" == x ]
    then find . \( -type d -name .git -o -name node_modules \) -prune -o  -type f -name "*.q" -print
    else find . \( -type d -name .git -o -name node_modules \) -prune -o  -type f -name "*.q" -print0 | xargs -0 fgrep -- "$1" /dev/null
    fi
}

function title {
    echo "$1" | awk '{ printf ("\033k%s\033\\", $NF) }'
}

function pipgrep {
    pip freeze | fgrep "==" | egrep "$1" | sed "s/==.*//"
}

# Adapted from https://github.com/jimeh/git-aware-prompt/blob/master/prompt.sh
find_git_branch() {
    # Based on: http://stackoverflow.com/a/13003854/170413
    local branch
    if branch=$(git rev-parse --abbrev-ref HEAD 2> /dev/null); then
        if [[ "$branch" == "HEAD" ]]; then
            branch='detached*'
        fi

        local status=$(git status --porcelain 2> /dev/null)
        if [[ "$status" != "" ]]; then
            git_info="($branch*) "
        else
            git_info="($branch) "
        fi
    else
        git_info=""
    fi
}

last_exit_code() {
    local EXIT="$?"
    if [ ${EXIT} != 0 ]; then
        exit_prompt="(${EXIT}) "
    else
        exit_prompt=""
    fi
}

# https://github.com/driv/upto/blob/master/upto.sh
function upto() {
    EXPRESSION="$1"
    if [ -z "$EXPRESSION" ]; then
	echo "A folder expression must be provided." >&2
	return 1
    fi
    CURRENT_FOLDER="$(pwd)"
    MATCHED_DIR=""
    MATCHING=true

    while [ "$MATCHING" = true ]; do
	if [[ "$CURRENT_FOLDER" =~ "$EXPRESSION" ]]; then
	    MATCHED_DIR="$CURRENT_FOLDER"
	    CURRENT_FOLDER="$(dirname $CURRENT_FOLDER)"
	else
	    MATCHING=false
	fi
    done
    if [ -n "$MATCHED_DIR" ]; then
	cd $MATCHED_DIR
    else
	echo "No Match." >&2
	return 1
    fi
}

alias compare='$(git rev-parse --show-toplevel)/quarkc/test/compare'

export HISTIGNORE='&:[ ]*:[bf]g:exit'  # Ignore duplicate entries and anything preceded by a space
export HISTTIMEFORMAT='%F %T '
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL=erasedups
export PROMPT_COMMAND='last_exit_code; find_git_branch; history -a'  # last_exit_code must be first!

export PATH=$HOME/bin:/usr/local/opt/coreutils/libexec/gnubin:$PATH

export TEMP=/var/tmp
export TMP=${TEMP}

export PYTHONSTARTUP=~/.pythonrc
export PAGER=less
export LESS="-FiMXR"  # quit if one screen, case-insensitive search, verbose prompt, don't clear screen, allow colors
export LESSOPEN="| src-hilite-lesspipe.sh %s"

# Virtualenv stuff
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Dropbox
source /usr/local/bin/virtualenvwrapper.sh

shopt -s cdspell checkhash checkwinsize cmdhist extglob histappend hostcomplete interactive_comments

myhost=${HOSTNAME%%.*}
if [ x$SUDO_USER == x ]; then
    export PS1='\[\e[32;1m\]${myhost}:\W ${git_info}\[\e[1;33m\]${exit_prompt}\[\e[32;1m\]\$ \[\e[0m\]'
else
    export PS1='\[\e[32;1m\]${USER}@${myhost}:\W \$ \[\e[0m\]'
fi


if [ "$TERM" == "screen" ] || [ -n "$STY" ] || [ -n "$TMUX" ]; then
    #export PS1="\[\ek${myhost}\e\\\\\]${PS1}"
    title "${myhost}"
fi

[ -f /usr/local/etc/bash_completion ] && . /usr/local/etc/bash_completion

source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.bash.inc'
source '/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/completion.bash.inc'

# added by travis gem
[ -f /Users/ark3/.travis/travis.sh ] && source /Users/ark3/.travis/travis.sh

export PATH=$(awk -F: '{for(i=1;i<=NF;i++){if(!a[$i]++)printf s$i;s=":"}}' <<< $PATH)  # Eliminate dups
export PATH=$(sed -e s/:\.:/:/g -e s/^\.:// -e s/:\.$// <<< $PATH)  # So . is not in my path
