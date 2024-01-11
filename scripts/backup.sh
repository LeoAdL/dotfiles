#! /bin/bash
#
export AWS_ACCESS_KEY_ID="$(pass AWS_ACCESS_KEY_ID)"
export AWS_SECRET_ACCESS_KEY="$(pass AWS_SECRET_ACCESS_KEY)"
export RESTIC_REPOSITORY="s3:s3.us-east-005.backblazeb2.com/restic-backup-ladl"
export RESTIC_PASSWORD="$(pass RESTIC_PASSWORD)"
restic backup $HOME/ --exclude="$HOME/Library/**" --exclude="$HOME/Downloads/**" --exclude="$HOME/Movies/movie/**" --exclude="$HOME/Movies/series/**" --exclude="$HOME/OneDrive/**" --exclude="$HOME/Music/**"
restic forget --keep-within 1d --keep-within-daily 30d --keep-within-weekly 1y --prune

