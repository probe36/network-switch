#!/usr/bin/env python
DDIR="/var/spool"
FILE1="root"
FILE2="backup"

echo "216 Backup" | mailx -s -v "Backup log from 216" -a "$DDIR/$FILE1" skim@biddingo.com
