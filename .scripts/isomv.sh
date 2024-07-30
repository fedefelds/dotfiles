#!/bin/bash
echo "Enter description:";
read desc;
echo "extension";
read ext;
new_name=$(echo $(date -I)-$(echo $desc | tr " " -).$ext | tr -d "\n")
mv $1 $new_name
echo $new_name | xclip -selection clipboard; echo "filename copied to clipboard!"
