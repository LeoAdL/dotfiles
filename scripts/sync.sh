#! /bin/bash

 rclone bisync  --transfers 32 --create-empty-src-dirs /$HOME/sync remote:core-backup-ladl
