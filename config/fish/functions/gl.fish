# Defined in - @ line 0
function gl --description 'alias gl=git log --pretty="format:%C(auto)%h %s %C(blue)%aN %C(bold black)(%cr) %C(auto)%d"'
	git log --pretty="format:%C(auto)%h %s %C(blue)%aN %C(bold black)(%cr) %C(auto)%d" $argv;
end
