#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <descrip.h>
#include <rms.h>
#include <dvidef.h>
#include <devdef.h>
#include <ssdef.h>

int v2_determine_device(unit, device)
int unit;
int *device;
{
   struct FAB fab;
   struct NAM nam;
   int status;
   char fname[MAX_STRING_SIZE];
   int retlen, devchar;
   struct {
      short int length;
      short int code;
      int *device_char;
      int *ret_len;
      int terminator;
   } itmlst;

   $DESCRIPTOR(filename,fname);

   /* If this is a new-style temporary file, it must be on disk.  Check	*/
   /* it here so sys$parse won't think it's an "illegal" name (since	*/
   /* '+' is in fact illegal in a VMS filename).			*/

   if (*(CURRENT_S_VALUE(NAME)) == '+') {
      *device = DEV_DISK;
      return SUCCESS;
   }

   itmlst.length = 4;
   itmlst.code = DVI$_DEVCHAR;
   itmlst.device_char = &devchar;
   itmlst.ret_len = &retlen;
   itmlst.terminator = 0;

   /* initialize the rms fab control block. */

   fab = cc$rms_fab;
   nam = cc$rms_nam;
   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);
   fab.fab$l_fna = CURRENT_S_VALUE(NAME);
   fab.fab$b_fns = strlen(CURRENT_S_VALUE(NAME));
   fab.fab$l_nam = &nam;           /* Link the NAM to the FAB */
   nam.nam$l_rsa = CURRENT_S_VALUE(NAME);
   nam.nam$b_rss = MAX_STRING_SIZE;
   nam.nam$l_esa = fname;
   nam.nam$b_ess = MAX_STRING_SIZE;
   nam.nam$b_nop = NAM$M_NOCONCEAL | NAM$V_SYNCHK;

   status = sys$parse(&fab);			/* get the full filename */
   if (status != RMS_SUCCESS)			/* including device name */
      return status;

   filename.dsc$w_length = nam.nam$b_esl;

   status = sys$getdvi(0,0,&filename,&itmlst,0,0,0,0);  /* get the device info*/
   if (status != SUCCESS)
      return status;

   *device = DEV_DISK;

   if (devchar & DEV$M_NET)
      *device = DEV_DECNET;
   if (devchar & DEV$M_SQD)
      *device = DEV_ANSI;

   if ((devchar & DEV$M_SWL) && !EQUAL(CURRENT_S_VALUE(OP),"READ"))
      return SS$_WRITLCK;         /* SW write lock and WRITE or UPDATE mode */

   return SUCCESS;
}
