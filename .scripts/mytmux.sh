#!/bin/bash

# Define the session name
session_name="tmux"

# Check if the session exists
if tmux has-session -t "$session_name" 2>/dev/null; then
    # If it exists, attach to it
    tmux attach-session -t "$session_name"
else
    # If it doesn't exist, create it
    tmux new-session -d -s "$session_name"
    tmux attach-session -t "$session_name"
fi
