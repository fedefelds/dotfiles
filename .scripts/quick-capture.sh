#capture input from std in and transform it into an inbox-formated header
#format:
#  * this is a header
#    [2024-07-16 Tue 13:23]

while true; do
    read -p "quick capture (empty line to exit): " line
    if [ -z "$line" ]; then
        break
    fi
    printf "* $line \n[$(date '+%Y-%m-%d %a %H:%M')]\n" | tee -a ~/Documents/org/roam/inbox.org
    echo                                                                                           
done
