#!/bin/bash
# Get the current date in ISO 8061 format YYYY-MM-DD
current_date=$(date +%Y%m%d)

# Remove spaces and replace them with underscores
formatted_description=$(echo "$@" | tr ' ' '_' | tr a-z A-Z)

#combine date & formatted description
dirname="$current_date"_"$formatted_description"

#create directory & paste name to clipboard
mkdir $dirname && echo "$dirname" | xclip -selection clipboard

#some feedback
echo "$dirname in clipboard"
