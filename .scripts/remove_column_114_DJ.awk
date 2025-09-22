#!/usr/bin/awk -f
# usage 
# awk -f remove_column_114.awk input.csv > output.csv
BEGIN {
    FS = OFS = ","  # Set input and output field separators to comma
}

{
    for (i = 1; i <= NF; i++) {
        if (i != 114) {
            printf "%s", $i;
            # Only print comma if not the last field and next field isn't the one we skipped
            if ((i < NF && i != 113) || (i == 113 && NF > 114)) {
                printf OFS;
            }
        }
    }
    printf "\n";
}

