function ls --description 'alias ls=ls -sF +color'
  if not set -q __ls_color_flags
    switch (uname)
      case Linux
        set -g __ls_color_flags '--color=auto'
      case Darwin
        set -g __ls_color_flags '-G'
      case '*'
        set -g __ls_color_flags ''
    end
  end

	command ls -sF $__ls_color_flags $argv;
end
