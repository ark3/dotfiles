function fin --description 'Find in files'
  if test (count $argv) = 0
    echo "Find what?"
    return 1
  else
    rg $argv
  end
end
