
#!/bin/bash

rm -rf /backup/Oracle/daily/DB/redolog/*$(date --date='-4 days' +%Y%m%d)*
rm -rf /backup/Oracle/daily/exp_dump/*$(date --date='-4 days' +%Y-%m-%d)*
rm -rf /backup/Oracle/daily/DB/control/*$(date --date='-4 days' +%Y%m%d)*
rm -rf /backup/Oracle/daily/DB/dbf/*$(date --date='-4 days' +%Y%m%d)*

rsync -avzhe ssh --bwlimit=1900 --progress root@192.168.5.23:/backup/daily /backup/Oracle

