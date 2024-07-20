#!/bin/bash
# Get the current date in ISO 8061 format YYYY-MM-DD
#current_date=$(date +%Y-%m-%d)
current_date=$(date -I)
# Read the description from the user input
read -p "enter directory description: " description

# Remove spaces and replace them with underscores
formatted_description=$(echo "$description" | tr ' ' '-')

# Create the directory with the formatted description
new_dir="$current_date"-"$formatted_description"
mkdir $new_dir && echo "Directory created: " $new_dir 
