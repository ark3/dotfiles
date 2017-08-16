function fish_right_prompt --description 'Write out the right side of the prompt'
  #Save the return status of the previous command
  set stat $status

  if not set -q __fish_prompt_normal
    set -g __fish_prompt_normal (set_color normal)
  end

  if not set -q __fish_color_blue
    set -g __fish_color_blue (set_color -o blue)
  end

  if not set -q __fish_git_prompt_show_informative_status
    set -g __fish_git_prompt_show_informative_status 1
  end
  if not set -q __fish_git_prompt_hide_untrackedfiles
    set -g __fish_git_prompt_hide_untrackedfiles 1
  end

  if not set -q __fish_git_prompt_color_branch
    set -g __fish_git_prompt_color_branch magenta --bold
  end
  if not set -q __fish_git_prompt_showupstream
    set -g __fish_git_prompt_showupstream "informative"
  end
  if not set -q __fish_git_prompt_char_upstream_ahead
    set -g __fish_git_prompt_char_upstream_ahead "↑"
  end
  if not set -q __fish_git_prompt_char_upstream_behind
    set -g __fish_git_prompt_char_upstream_behind "↓"
  end
  if not set -q __fish_git_prompt_char_upstream_prefix
    set -g __fish_git_prompt_char_upstream_prefix ""
  end

  if not set -q __fish_git_prompt_char_stagedstate
    set -g __fish_git_prompt_char_stagedstate "●"
  end
  if not set -q __fish_git_prompt_char_dirtystate
    set -g __fish_git_prompt_char_dirtystate "✚"
  end
  if not set -q __fish_git_prompt_char_untrackedfiles
    set -g __fish_git_prompt_char_untrackedfiles "…"
  end
  if not set -q __fish_git_prompt_char_conflictedstate
    set -g __fish_git_prompt_char_conflictedstate "✖"
  end
  if not set -q __fish_git_prompt_char_cleanstate
    set -g __fish_git_prompt_char_cleanstate "✔"
  end

  if not set -q __fish_git_prompt_color_dirtystate
    set -g __fish_git_prompt_color_dirtystate blue
  end
  if not set -q __fish_git_prompt_color_stagedstate
    set -g __fish_git_prompt_color_stagedstate yellow
  end
  if not set -q __fish_git_prompt_color_invalidstate
    set -g __fish_git_prompt_color_invalidstate red
  end
  if not set -q __fish_git_prompt_color_untrackedfiles
    set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
  end
  if not set -q __fish_git_prompt_color_cleanstate
    set -g __fish_git_prompt_color_cleanstate green --bold
  end

  if not set -q __fish_prompt_hostname_short
    set -g __fish_prompt_hostname_color (set_color 666)
    switch (hostname)
      case 'timonium*'
        set -g __fish_prompt_hostname_short TMN
      case 'hexagon*'
        set -g __fish_prompt_hostname_short HEX
      case '*'
        set -g __fish_prompt_hostname_short (hostname)
    end
  end

  #Set the color for the status depending on the value
  set __fish_color_status (set_color -o green)
  if test $stat -gt 0
    set __fish_color_status (set_color -o red)
  end

  switch $USER

    case root toor

    case '*'

      if not set -q __fish_prompt_cwd
        set -g __fish_prompt_cwd (set_color $fish_color_cwd)
      end

      set __venv_info ""
      if set -q VIRTUAL_ENV
        set __venv_info " $__fish_prompt_normal""["(basename "$VIRTUAL_ENV")"]"
      end

      printf '%s%s%s%s %s%s %s%s %s%s' "$__fish_color_status" "$stat" (__fish_git_prompt) "$__venv_info" "$__fish_prompt_cwd" (prompt_pwd) "$__fish_prompt_hostname_color" "$__fish_prompt_hostname_short" "$__fish_prompt_normal" (date "+%H:%M:%S")

  end
end
