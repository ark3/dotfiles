# Aliases and functions

case "$(uname -s)" in
    Darwin)
    alias ls='ls -sFG'
    ;;

    Linux)
    alias ls='ls -sF --color'
    ;;

    *)
    alias ls='ls -sF'
esac

# Enable aliases to be sudo’ed
# https://askubuntu.com/questions/22037/aliases-not-available-when-using-sudo/22043#22043
alias sudo='sudo '

alias c='cat'
alias m='less'
alias view='vim -R'
alias where='type'
alias cb='find . \( -type d -name .git -o -name node_modules \) -prune -o  -type f \( -name "*.pyc" -o -name "*.pyo" -o -name "*~" -o -name ".*~" -o -name ".#*" -o -name "*.class" -o -name "*.qc" -o -name "*.qp" \) -delete'

alias d='dirs'
alias md='mkdir'
alias rd='rmdir'
alias pd='pushd'
alias od='popd'

alias du='du -h'
alias df='df -h'
alias diff2diff='fgrep " differ" | sed -e "s/Files/diff -u/" -e "s/and //" -e "s/ differ//"'
alias em='emacsclient -q -n'
alias gdiff='git diff --no-index'
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias path='echo -e ${PATH//:/\\n}'
alias pspy='ps wwwaxu| egrep "^(USER|$USER.*[pP]ython)"'
alias py='python'

alias l='ls'
alias la='ls -a'
alias ll='ls -l'
alias lla='ls -la'
alias ltr='ls -ltr'

alias ga='git add'
alias gb='git branch'
alias gc='git commit'
alias gco='git checkout'
alias gd='git diff'
alias gdt='git difftool'
alias gl='git log --pretty="format:%C(auto)%h %s %C(blue)%aN %C(bold black)(%cr) %C(auto)%d"'
alias gll='git log --stat'
alias gp='git pull --ff-only'
alias gs='git status -sb'
alias gss='git status'
#alias gl='git log --pretty="format:%h %aN %ad %s" --date=short'
# See also git help shortlog for .mailmap stuff

# macOS has no md5sum or sha1sum; use fallbacks
command -v md5sum > /dev/null || alias md5sum="md5"
command -v sha1sum > /dev/null || alias sha1sum="shasum"

function mcd {
    mkdir -p -- "$1" &&
    cd -P -- "$1"
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
    else find "$1" -type f -print
    fi
}

function fpy {
    echo 'Use `rg -t py <pattern>` or `rg -t py --files` '
}

function fcc {
    echo 'Use `rg -t cpp <pattern>` or `rg -t cpp --files` '
}

function fjs {
    echo 'Use `rg -t js <pattern>` or `rg -t js --files` '
}

function fq {
    echo 'WTF? Use `rg -g \"*.q\" <pattern>` or `rg -g \"*.q\" --files`'
}

function title {
    echo "$1" | awk '{ printf ("\033k%s\033\\", $NF) }'
}

function pipgrep {
    pip freeze | fgrep "==" | egrep "$1" | sed "s/==.*//"
}

# https://github.com/driv/upto/blob/master/upto.sh
function upto {
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
