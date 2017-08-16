function rebounce --description 'Reenable Dock icon bouncing'
	defaults write com.apple.dock no-bouncing -bool FALSE
killall Dock
end
