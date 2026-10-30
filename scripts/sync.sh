#! /bin/bash
#
export RCLONE_PASSWORD_COMMAND="pass rclone"
export RCLONE_FAST_LIST=1
export RCLONE_TRANSFERS=16
export RCLONE_ORDER_BY="size,mixed,75"
rclone sync --create-empty-src-dirs -P "$HOME/sync/core" safe:
