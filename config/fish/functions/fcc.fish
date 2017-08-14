function fjs --description 'Find or find in JavaScript files'
  if test (count $argv) = 0
    rg -tcpp --files
  else
    rg -tcpp $argv
  end
end
