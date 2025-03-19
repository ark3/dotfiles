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
paths_to_add=(
  "$HOME/bin"
  "$HOME/.local/bin"
  "$HOME/go/bin"
  "$HOME/.krew/bin"
  "$HOME/.cargo/bin"
  "/usr/local/go/bin"
  "/opt/X11/bin"
  "/snap/bin"
)

paths=()
for p in "${paths_to_add[@]}" $(echo $PATH | tr ':' ' '); do
  # Skip empty paths
  if [[ -z "$p" ]]; then
    continue
  # Skip relative paths (not starting with /)
  elif [[ "$p" != /* ]]; then
    continue
  # Skip non-directory paths
  elif [[ ! -d "$p" ]]; then
    continue
  # Add only if not already present (deduplicate)
  elif [[ ! " ${paths[*]} " =~ " $p " ]]; then
    paths+=("$p")
  fi
done

PATH=$(IFS=:; echo "${paths[*]}")
unset paths_to_add paths
