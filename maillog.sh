
#!/bin/bash

SDIR=/var/spool/mail
FILE1=root
FILE2=backup
EMAIL=logadmin@biddingo.com

echo "Root backup progress log" | mailx -v -s "Root log" -a "$SDIR/$FILE1" $EMAIL
echo "Backup backup progress log" | mailx -v -s "Backup log" -a "$SDIR/$FILE2" $EMAIL

cd $SDIR && rm -rf $FILE1 $FILE2




