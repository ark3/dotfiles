#!/hint/bash

# Standard environment
if [ -z "$MY_BASH_ENV_IS_SET" ]; then
    [ -f "${HOME}/.profile" ] && . "${HOME}/.profile"
    MY_BASH_ENV_IS_SET=true
    [ -f "${HOME}/.local/bash" ] && . "${HOME}/.local/bash"
fi

# Interactive configuration
case $- in
*i*)
    HISTIGNORE='&:[ ]*:[bf]g:exit' # Ignore duplicate entries and anything preceded by a space
    HISTTIMEFORMAT='%F %T '        # ISO date and time
    HISTSIZE=-1                    # No limit
    HISTFILESIZE=1000000
    HISTCONTROL=erasedups:ignorespace

    command -v emacsclient >/dev/null && export EDITOR=emacsclient

    PRF="${HOME}/dotfiles/bash"
    [ -f "${PRF}/aliases" ] && . "${PRF}/aliases"
    [ -f "${PRF}/prompt" ] && . "${PRF}/prompt"
    [ -f "${PRF}/completion" ] && . "${PRF}/completion"
    unset PRF
    ;;
esac

# Stuff below here was added by an automation
