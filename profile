#!/hint/bash

umask 022

export TEMP=/var/tmp
export TMP=${TEMP}
export LESS="-FiMXR" # quit if one screen, case-insensitive search, verbose prompt, don't clear screen, allow colors
export PAGER=less
export SUDO_PROMPT="[sudo %u@%h] Password: "
export VIRTUAL_ENV_DISABLE_PROMPT=1 # so activate script doesn't modify prompt

# Set up for Nix
if [ -e "$HOME/.nix-profile/etc/profile.d/nix.sh" ]; then
    . "$HOME/.nix-profile/etc/profile.d/nix.sh"
fi

# Set up for Homebrew
[ -e brew ] && eval "$(brew shellenv)"
if [ -e "/home/linuxbrew/.linuxbrew/bin/brew" ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
fi

# Set up PATH
new_path=
build_new_path() {
    # Skip a relative pathname
    case "$1" in
    "/*") : ;;
    "*") return ;;
    esac

    # Skip empty or missing argument, non-directory
    [ -z "$1" ] && return
    [ ! -d "$1" ] && return

    # Skip if already present
    printf "%s" "$new_path" | tr ':' '\n' | grep -qx "$1" && return

    # Append to the new path, from https://unix.stackexchange.com/a/415028
    new_path=${new_path:+${new_path}:}$1
}

build_new_path "$HOME/bin"
build_new_path "$HOME/.local/bin"
build_new_path "$HOME/go/bin"
build_new_path "$HOME/.krew/bin"
build_new_path "$HOME/.cargo/bin"
build_new_path "/usr/local/go/bin"
build_new_path "/opt/X11/bin"
build_new_path "/snap/bin"

for path_comp in $(printf "%s" "$PATH" | tr ':' '\n'); do
    build_new_path "$path_comp"
done

PATH=$new_path

unset -v path_comp new_path
unset -f build_new_path
