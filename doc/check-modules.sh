#!/bin/sh

generate () {
    file="$1"
    module="${file%.mdwn}"
    module='('"$(printf '%s' "$module" | tr / ' ')"')'
    printf '### `%s`\n\n' "$module"
    printf '@include scheme/%s@\n\n' "$file"
}

find ../scheme -name "*.mdwn" | sort | \
    while read file; do
        file="${file#../scheme/}"
        grep -F "$file" -q chip-remote.mdwn || generate "$file"
    done
