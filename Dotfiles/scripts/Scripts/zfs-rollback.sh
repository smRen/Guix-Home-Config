#!/usr/bin/env bash

set -euf -o pipefail

# Run these commands manually until checked if ok

# Zfs imports
zpool export -a
zpool import -N -R /mnt rpool -f
zpool import -N -R /mnt bpool -f

SNAPSHOTNAME='initial'

zfs list -t snapshot | grep $SNAPSHOTNAME | awk '{print "zfs rollback -r " $1}'

