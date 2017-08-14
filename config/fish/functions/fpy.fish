function fpy --description 'Find or find in Python files'
  if test (count $argv) = 0
    rg -tpy --files
  else
    rg -tpy $argv
  end
end
