#! /bin/bash
#
rclone bisync  --fast-list --transfers 32 --create-empty-src-dirs /$HOME/sync/core drive:core_backup
rclone bisync  --fast-list --transfers 32 --create-empty-src-dirs /$HOME/sync/core remote:core-backup-ladl
