#Purpose: make dauily backup from DC to Office
#Revised: 20200420
#Version control
#!bin/bash

#Home dir
BACKUPHOME=/backup/ETNbackup

#Source dir
SDIR1=192.168.0.55:/home/ETN_Documents/SdDocument
SDIR2=192.168.0.55:/home/ETN_Documents/SdDocumentAddendum

#Destination dir
DDIR=$BACKUPHOME/ETN_Documents

#Excluded files
EXCLUDES=$BACKUPHOME/exclude_files.txt

#Create daily backup dir
DNAME=$(date +%A)
mkdir -p $BACKUPHOME/weekly/$DNAME

#Option
OPT="-avzhe ssh --bwlimit=1900 --progress --force --ignore-errors --delete-excluded --exclude-from=$EXCLUDES --delete --backup --backup-dir=$BACKUPHOME/weekly/$DNAME"

#Delete all last week's data
rm -rf $BACKUPHOME/weekly/$DNAME/*

#Start Syncing
rsync $OPT $SDIR1 $DDIR
rsync $OPT $SDIR2 $DDIR
rsync $Opt 
