#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#include <descrip.h>

/* This routine constructs the default file name suffix from the last	*/
/* two hex chars of the process id, which comes from a logical name	*/
/* defined outside of VICAR.						*/

void v2_get_def_filename()
{
   int i,size;
   static $DESCRIPTOR(log,PROCESS_ID_LOGICAL);
   static $DESCRIPTOR(out,"                 ");

   i = sys$trnlog(&log,&size,&out,0,0,0);
   if (i != SUCCESS)
      return;

   size = size & 0x0000ffff;
   strncat(default_file_name, out.dsc$a_pointer, size);

   return;
}

