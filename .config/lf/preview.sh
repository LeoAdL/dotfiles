#!/usr/bin/env bash
case "$1" in
    *.pdf) pdftotext "$1" -;;
esac
