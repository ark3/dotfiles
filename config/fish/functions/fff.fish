function fff --description 'List files in directory'
  if test (count $argv) = 0
    find . -type f
  else
    find $argv -type f
  end
end
