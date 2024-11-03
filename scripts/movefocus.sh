#!/usr/bin/bash

direction=$1
res=$(hyprctl activewindow)
window_id=$(echo "$res" | grep -oP '(?<=Window )\w+')
grouped=$(echo "$res" | grep -oP '(?<=grouped: )\S+')

if [ "$grouped" = "0" ]; then
    echo "Not a group"
    command="hyprctl dispatch movefocus"
else
    IFS=',' read -r -a ids <<< "$grouped"

    index=-1
    for i in "${!ids[@]}"; do
        if [[ "${ids[i]}" == "$window_id" ]]; then
            index=$i
            break
        fi
    done

    length=${#ids[@]}
    if (( length == 1)); then
        echo "Alone"
        command="hyprctl dispatch movefocus"
    elif (( index + 1 == length )); then
        echo "All the way to the right"
        if [ "$direction" = "r" ]; then
            command="hyprctl dispatch movefocus"
        else
            command="hyprctl dispatch changegroupactive"
        fi
    elif (( index == 0 )); then
        if [ "$direction" = "l" ]; then
            command="hyprctl dispatch movefocus"
        else
            command="hyprctl dispatch changegroupactive"
        fi
    else
        echo "Yes"
        command="hyprctl dispatch changegroupactive"
    fi
fi

if [ "$command" = "hyprctl dispatch changegroupactive" ]; then
    if [ "$direction" = "l" ]; then
        direction="b"
    else
        direction="f"
    fi
fi

$command $direction
