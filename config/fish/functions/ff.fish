function ff --description 'Find files and directories with a particular name'
  if test (count $argv) = 0
    find . -type f
  else
    find . \( -type f -o -type d \) -iname $argv
  end
end
