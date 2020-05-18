#!/usr/bin/env bash

rsync -avzhe ssh --bwlimit=1900 --progress 192.168.0.55:/home/ETN_Documents/SdDocument /backup/ETNbackup/weekly/ETN_Documents

rsync -avzhe ssh --bwlimit=1900 -P 192.168.0.55:/home/ETN_Documents/SdDocumentAddendum /backup/ETNbackup/weekly/ETN_Documents

