#!/bin/bash

hyprctl dispatch exec '[workspace 1] kitty'

#  hyprctl dispatch exec '[workspace specialworkspace silent] emacsclient --no-wait --create-frame --frame-parameters='\''(quote (name . "org-gtd"))'\'''
hyprctl dispatch exec "kitty '--title' 'agenda' emacsclient -t"
