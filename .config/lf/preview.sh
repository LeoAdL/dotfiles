#!/usr/bin/env bash
case "$1" in
    *.pdf) pdftotext "$1" -;;
    *.zip) zipinfo "$1";;
    *.tar.gz) tar -ztvf "$1";;
    *.tar.bz2) tar -jtvf "$1";;
    *.tar) tar -tvf "$1";;
    *) bat --color=always --style=plain --pager=never "$1" "$@";;
esac
