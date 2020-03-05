compton -b --config ~/.config/compton.conf &
setxkbmap -option ctrl:nocaps &
xrdb -m ~/.Xresources &

xset r rate 200 50 &


# Loop
while true
do
    # Set root title
    sh .xsetroot.sh

    # Check battery level
    BATT=$( acpi -b | sed 's/.*[charging|unknown], \([0-9]*\)%.*/\1/gi' )
    STATUS=$( acpi -b | sed 's/.*: \([a-zA-Z]*\),.*/\1/gi' )
    if ([ $BATT -le 5 ] && [ $STATUS == 'd!' ]); then
        # Beep
        echo -e "\007" >/dev/tty10 && sleep 0.2
        echo -e "\007" >/dev/tty10 && sleep 0.2
        echo -e "\007" >/dev/tty10 && sleep 0.2
        # Blink
        echo 'on' > /proc/acpi/ibm/light && sleep 1
        echo 'off' > /proc/acpi/ibm/light
    fi

    # Update every 10s
    sleep 10s
done &

exec dwm
