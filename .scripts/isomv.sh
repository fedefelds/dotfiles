#!/bin/bash
echo "Enter description:";
read desc;
echo "extension";
read ext;
if [ -z "$extension" ]; then
#    echo "The variable 'extension' is empty."
    new_name=$(echo $(date +%Y%m%d)_$(echo $desc | tr " " _ | tr a-z A-Z) | tr -d "\n")
else
#    echo "The variable 'extension' is not empty."
    new_name=$(echo $(date +%Y%m%d)_$(echo $desc | tr " " _ | tr a-z A-Z).$ext | tr -d "\n")
fi


mv $1 $new_name
echo $new_name | xclip -selection clipboard; echo "filename copied to clipboard!"
