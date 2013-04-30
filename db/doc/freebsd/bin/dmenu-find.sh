#!/bin/bash

# Locate a file and open it with the user's preferred application.

DMENU='dmenu.sh'
INPUT=$(xsel -o | $DMENU -p "file search:")

if [ "$INPUT" != '' ]; then
    RESULT=$(locate -e -i "$INPUT" | $DMENU -l 20 -p "search result:")
    xdg-open "$RESULT"
fi
