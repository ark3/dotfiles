if not set -q abbrs_initialized
  set -U abbrs_initialized
  echo -n Setting abbreviations...

  abbr c 'cat'
  abbr m 'less'

  abbr l 'ls'
  abbr la 'ls -a'
  abbr ll 'ls -l'
  abbr lla 'ls -la'
  abbr ltr 'ls -ltr'

  abbr d 'dirs'
  abbr md 'mkdir'
  abbr rd 'rmdir'
  abbr pd 'pushd'
  abbr od 'popd'

  abbr du 'du -h'
  abbr df 'df -h'
  abbr em 'emacsclient -q -n'
  abbr py 'python'

  abbr g 'git'
  abbr ga 'git add'
  abbr gb 'git branch'
  abbr gbl 'git blame'
  abbr gc 'git commit'
  abbr gco 'git checkout'
  abbr gcp 'git cherry-pick'
  abbr gd 'git diff'
  abbr gf 'git fetch'
  abbr gll 'git log --stat'
  abbr gm 'git merge'
  abbr gp 'git pull --ff-only'
  abbr gr 'git remote'
  abbr gss 'git status'
  abbr gs 'git status -sb'

  echo 'Done'
end

set -x LESS "-FiMXR"
set -x LESSOPEN "| src-hilite-lesspipe.sh %s"
set -x PAGER less
set -x PROJECT_HOME $HOME/Dropbox
set -x PYTHONSTARTUP ~/.pythonrc
set -x SUDO_PROMPT "[sudo %u@%h] Password: "
set -x VIRTUALFISH_DEFAULT_PYTHON /usr/local/bin/python3

set -x PATH ~/bin ~/datawire/bin $PATH

eval (~/.virtualenvs/pytools/bin/python -m virtualfish compat_aliases projects)
