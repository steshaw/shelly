tell application "System Events"
	set properties of security preferences to {require password to wake:false}
end tell