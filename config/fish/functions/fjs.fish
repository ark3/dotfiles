function fjs --description 'Find or find in JavaScript files'
	if test (count $argv) = 0
rg -tjs --files
else
rg -tjs $argv
end
end
