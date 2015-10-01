#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine reads the size of the EOL labels from the beginning of the */
/* EOL area (at the end of the file) and puts it in EOL_SIZE.  This is so  */
/* that if the file is opened for UPDATE, the EOF position will be set     */
/* correctly.								   */

int v2_get_eol_size(int unit)
{
   struct bufstate *bufstate;
   char *value, *ip;
   int vallen;
   int rec,recsize,n_label_recs,i,eol_size,lblsize;
   int j,status;
   char *loc_buf;

#if RTL_USE_COMPRESSION
   /* If the file has eol and is compressed */
   if (CURRENT_I_VALUE(EOL) && COMPRESSED)
      return v2_compress_get_eol_size(unit);
#endif

   eol_size = 0;
   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   recsize = CURRENT_I_VALUE(RECSIZE);
   lblsize = CURRENT_I_VALUE(LBLSIZE);
   n_label_recs = lblsize/recsize;

   /* If EOL is set it means that extra labels exist at the end of the file.  */

   if (CURRENT_I_VALUE(EOL)==TRUE) {
      rec = n_label_recs + (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3)) +
				CURRENT_I_VALUE(NLB);

      j = LABEL_SIZE_ITEM;	/* Compute the # recs to read eol SYSTEM_KEY */
      j = CEIL(j, recsize);
      loc_buf = malloc(j*recsize);
      if (loc_buf==NULL)
	 return NO_MEMORY_FOR_LABEL_PROCESS;

      for (i=0; i<j; i++) {
	 status = v2_read_rec(bufstate, loc_buf+i*recsize, rec+i, 0, 0,
								&no_trans);
	 if (status != SUCCESS)
	    return status;
      }
      *(loc_buf+j*recsize-1) = '\0' ; /* Make 'loc_buf' a c string.     */

      if (v2_find_entry(loc_buf, SYSTEM_KEY, &value, &vallen, &ip) != 0)
	 eol_size = atoi(v2_string(value,vallen));
      else
	 eol_size = 0;
      CURRENT_I_VALUE(EOL_SIZE) = eol_size;
      free(loc_buf);
   }

   return SUCCESS;
}
