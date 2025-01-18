#!/bin/bash

# Directory to search (default to current directory if not specified)
SEARCH_DIR="${1:-.}"

# File size threshold (5MB)
SIZE_THRESHOLD="+5M"

# .gitignore file
GITIGNORE_FILE=".gitignore"

# Find files larger than the threshold, excluding the .git directory, and print base names
find "$SEARCH_DIR" -type f -size "$SIZE_THRESHOLD" -not -path "$SEARCH_DIR/.git/*" -exec basename {} \; | \
while read -r FILE; do
    # Check if the file is already in .gitignore
    if ! grep -qxF "$FILE" "$GITIGNORE_FILE"; then
        echo "$FILE" >> "$GITIGNORE_FILE"
    fi
done

echo "Large files have been added to $GITIGNORE_FILE."
