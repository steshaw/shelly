tell application "System Events"
	set properties of security preferences to {require password to wake:true}
	activate application "ScreenSaverEngine"
end tell