#!/bin/bash
# Get the current date in ISO 8061-ish format YYYYMMDD
current_date=$(date +%Y%m%d)
filename=$(echo "$@" | tr ' ' '-')

#combine date & filename
filename="$current_date-$filename"

#create file & paste name to clipboard
touch $filename && echo "$filename" | xclip -selection clipboard

#some feedback
echo "$filename in clipboard"
