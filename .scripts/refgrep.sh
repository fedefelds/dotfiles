#!/bin/bash
#grep --color=auto -riEn "$@" ~/Documents/org/*.org
grep --color=auto -riEn "$@" $(find ~/Documents/org -iname '*.org')
