set inputVolume to input volume of (get volume settings)

log inputVolume

if inputVolume < 5 then
	set inputVolume to 100
	display notification "Volume set to 100" with title "✅ Microphone is on"
else
	set inputVolume to 0
	display notification "Volume set to 0" with title "❌ Microphone is off"
end if
set volume input volume inputVolume
