#!/usr/bin/env bash

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
