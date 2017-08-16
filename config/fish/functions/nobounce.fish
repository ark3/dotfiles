function nobounce --description 'Disable Dock icon bouncing'
	defaults write com.apple.dock no-bouncing -bool TRUE
killall Dock
end
