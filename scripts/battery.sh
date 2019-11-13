#!/bin/sh

status=$(</sys/class/power_supply/BAT0/status)
capacity=$(</sys/class/power_supply/BAT0/capacity)
energy_now=$(</sys/class/power_supply/BAT0/energy_now) # Remaining numerator
power_now=$(</sys/class/power_supply/BAT0/power_now) # Remaining denominator

echo "b;$status $capacity $energy_now $power_now" > /tmp/statuspipe.fifo
