:
DISK_NAME=$1
TEMP_FILE_NAME=$2
#echo "1= "$1"  2= " $2 "  0= " $0 "  arg" $# 

#check if standard error file exists and remove if so
rm -f errors*

#check number of parameters input - UNIX file name expansion adds parameters
if [ $# != 2 ]
  then echo "CHKSPACE: DISK name not valid" > errors
  exit 0
fi

#use "df" command to check disk space and pipe to nawk 
#if disk name was not a disk output error message to standard error file
#nawk will get the available disk space and put it in temp file
df -k "$DISK_NAME"  2>errors | awk ' {  
  if (NR == 2 && $4 != "") print $4 ; #if sys name is long avail on diff line 
  if (NR > 2 && $3 != "") print $3 ;}' > $TEMP_FILE_NAME

if [ -s errors ]
  then a=1
  else rm errors
fi
exit 0

