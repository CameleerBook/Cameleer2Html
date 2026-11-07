#!/bin/sh
set -eu

version=$(sed -n '1s/^# //p' "$1")

if [ -z "$version" ]; then
    echo "Could not extract version from CHANGELOG.md" >&2
    exit 1
fi

printf 'let version = "%s"\n' "$version"