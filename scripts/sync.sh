#! /bin/bash
#
export RCLONE_PASSWORD_COMMAND="pass rclone"
export RCLONE_FAST_LIST=1
export RCLONE_TRANSFERS=16
export RCLONE_ORDER_BY="size,mixed,75"
rclone sync  --fast-list --transfers 16 --create-empty-src-dirs -P /$HOME/sync/core safe:
rclone sync  --fast-list --transfers 16 --create-empty-src-dirs -P /$HOME/phd_notes obsidian_crypt:
