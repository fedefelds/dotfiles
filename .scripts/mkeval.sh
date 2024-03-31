#!/bin/bash
# Get the current date in the format YYYY-MM-DD
current_date=$(date +%Y-%m-%d)

# Read the description from the user input
read -p "Enter tool & description (in11 mp8 dp): " description

# Remove spaces and replace them with underscores
formatted_description=$(echo "$description" | tr ' ' '_')

# Create the directory with the formatted description
mkdir "$current_date"_"$formatted_description"
echo "Directory created: $current_date"_"$formatted_description"
