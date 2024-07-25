#!/bin/bash
rsync -r ~/Documents/org/roam/pictures ~/Documents/org/roam/static/
emacs -Q --script ~/.scripts/build-site.el
