#!/usr/bin/env bash

# Change directory to the source directory of this script. Taken from:
# https://stackoverflow.com/a/246128/3858681
pushd "$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )" > /dev/null

CMD=${1:-""}
DB=${DB:-"detsys.sqlite3"}

function display_help() {
    echo "$(basename $0) <command>"
    echo "<command> ::= init | drop_tables"
    exit 1
}

case "${CMD}" in
    init)        sqlite3 "${DB}" < init.sql ;;
    drop_tables) sqlite3 "${DB}" < drop_tables.sql ;;
    *)           display_help ;;
esac

popd > /dev/null
