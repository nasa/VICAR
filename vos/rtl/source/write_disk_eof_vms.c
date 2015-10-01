#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <descrip.h>
#include <ssdef.h>
#include <iodef.h>

/* Updates the end-of-file in the disk header to be 'last_byte'. */

v2_write_disk_eof(unit, diskstate, last_byte, recsize)
int unit;
struct diskstate *diskstate;
V2_OFFSET last_byte;
int recsize;
{
   int i, j;
   char file_name[512];

   $DESCRIPTOR(file_desc,&file_name);
 
   union
   {
      int size;
      short int wsize[2];
   } u;

   /* Use an acp service to set the eof at the end of the file.	*/

   struct FIB
   {
      unsigned acctl : 24;
      unsigned wsize : 8;
      short int fid_num;
      short int fid_seq;
      unsigned fid_rvn : 8;
      unsigned fid_nmx : 8;
      
      short int did_num;
      short int did_seq;
      unsigned did_rvn : 8;
      unsigned did_nmx : 8;

      int wcc;
      short int nmctl;
      short int exctl;
      int exsz;
      int exvbn;
      unsigned alopts : 8;
      unsigned alalign : 8;
      short int alloc[5];
      short int verlimit;
      short int unused;
   } extend_fib;

   struct FAT
   {
      unsigned rtype : 8;
      unsigned rattrib : 8;
      short int rsize;
      short int hiblkh;
      short int hiblkl;
      short int efblkh;
      short int efblkl;
      short int ffbyte;
      unsigned bktsize : 8;
      unsigned vfcsize : 8;
      short int maxrec;
      short int defext;
      char future_use[32];
      short int unused;
      short int versions;
   } extend_fat;

   struct ATR
   {
      short int size;
      short int type;
      int addr;
   } extend_atr[3];
   
   memset(&extend_fib, 0, sizeof(extend_fib));
   memset(&extend_fat, 0, sizeof(extend_fat));
   memset(&extend_atr, 0, 3*sizeof(extend_atr));

   strcpy(file_name,CURRENT_S_VALUE(NAME));
   file_desc.dsc$w_length = strlen(CURRENT_S_VALUE(NAME));

   /* Set up the atr argument to pass a record attribute area. */
   extend_atr[0].size = 32;
   extend_atr[0].type = 4;
   extend_atr[0].addr = &extend_fat;

   j = CEIL(last_byte, diskstate->blocksize);
   u.size = j;
   extend_fat.ffbyte = last_byte - (long)diskstate->blocksize*(j-1);
   if (extend_fat.ffbyte == 0) ++u.size;
   extend_fat.efblkh = u.wsize[1];
   extend_fat.efblkl = u.wsize[0];
   extend_fat.rtype = 1;
   extend_fat.rsize = recsize;
   if (recsize > MAX_DISK_RECSIZE || recsize&1 != 0)
      extend_fat.rsize = 512; /* fix RMS problems with big & odd-length files */
   extend_fat.maxrec = extend_fat.rsize;

   i = sys$qiow(diskstate->io_event,diskstate->channel, /* Change file's eof */
		IO$_MODIFY,&diskstate->iosb,0,0,
		&extend_fib,0,0,0,&extend_atr,0);

   if (i != SUCCESS) return i;

   i = diskstate->iosb.status & 0x0000ffff;
   if (i != SS$_NORMAL) return i;

   if (recsize != 0)
      ((struct bufstate *)CURRENT_IP_VALUE(BUFSTATE))->eof_record =
							last_byte / recsize;
   return SUCCESS;
}
