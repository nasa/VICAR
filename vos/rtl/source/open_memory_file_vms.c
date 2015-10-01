#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <descrip.h>
#include <secdef.h>
#include <ssdef.h>

/* Return TRUE if unit is a memory file, FALSE otherwise.  If it is a memory */
/* file, then open it.							     */

int v2_open_memory_file(unit, bufstate)
int unit;
struct bufstate *bufstate;
{
   int lock,status,flags;
   struct dsc$descriptor_s section;
   char *s[2], *r[2], p[MAX_FILE_NAME_SIZE+1];

   strcpy(p, "V2$");
   strcat(p, CURRENT_S_VALUE(NAME));
   make_upper_case(p,p);

   section.dsc$w_length = strlen(p);
   section.dsc$b_class = DSC$K_CLASS_S;
   section.dsc$b_dtype = DSC$K_DTYPE_T;
   section.dsc$a_pointer = p;

   r[0] = lock;
   r[1] = lock;

   flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL;

   status = sys$mgblsc(r,s,0,flags,&section,0,0);
  
   if (status == SS$_NORMAL) {
      bufstate->devstate.device_type = DEV_MEMORY;
      bufstate->devstate.dev.memory.size = s[1] - s[0] + 1;
      bufstate->devstate.dev.memory.start = s[0];

      bufstate->buffer = s[0];
      bufstate->bufsize = s[1] - s[0] + 1;
      bufstate->blocksize = bufstate->bufsize;
      bufstate->blockno = 0;
      bufstate->nblocksinbuf = 1;
      bufstate->flags |= BUF_VALID;

      return TRUE;
   }
   return FALSE;
}
