#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Before label processing by the xl routines can take place,	*/
/* the labels must be in memory. This routine reads the labels	*/
/* associated with 'unit' and returns a pointer to them in 'p'.	*/
/* The memory for the labels is allocated by this routine.	*/

int v2_read_in_labels(
   int unit,		/* in: unit number to use */
   char **p,		/* out: pointer to labels */
   int *size		/* out: size of allocated area */
)

{
   struct bufstate *bufstate;
   int rec,recsize,n_label_recs,i,eol_size,lblsize;
   int status,eol_label_recs;
   char *q,*lbl2;

   eol_size = 0;
   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   recsize = CURRENT_I_VALUE(RECSIZE);
   lblsize = CURRENT_I_VALUE(LBLSIZE);
   n_label_recs = lblsize/recsize;

/* If EOL is set it means that extra labels exist at the end of the file;  */
/* the size of this End of Line label must be determined so that space may */
/* be allocated for it and it may be read in.				   */

   v2_get_eol_size(unit);

   eol_size = CURRENT_I_VALUE(EOL_SIZE);

/* Allocate the space that will hold the labels; LABEL_BUFFER_EXTRA is added */
/* to allow for expansions of the system label.				     */

   *size = lblsize + eol_size + LABEL_BUFFER_EXTRA;
   *p = malloc(*size);
   if (*p == NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;
   memset(*p, 0, lblsize + eol_size + LABEL_BUFFER_EXTRA);

   q = *p;

   for (i=0; i<n_label_recs; i++,q+=recsize) {	/* read in the labels */
      status = v2_read_rec(bufstate, q, i, 0, 0, &no_trans);
      if (status != SUCCESS)
	 return status;
   }

   q = *p;
   q = q + strlen(q);
   lbl2 = q;			/* save beginning of EOL label */
   eol_label_recs = eol_size/recsize;

#ifdef RTL_USE_COMPRESSION
   if (COMPRESSED) {
       v2_compress_read_in_eol(unit, q, eol_size);
   }
   else
#endif
   {
      rec = (CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3)) +
	   			CURRENT_I_VALUE(NLB) + n_label_recs;

      for (i=0; i<eol_label_recs; i++,q+=recsize) {	/* read in EOL labels */
         status = v2_read_rec(bufstate, q, rec++, 0, 0, &no_trans);
         if (status != SUCCESS)
	    return status;
      }
   }

   /* Cut out 2nd SYSTEM_KEY */
#ifdef RTL_USE_COMPRESSION
   if (eol_label_recs > 0 || (COMPRESSED && CURRENT_I_VALUE(EOL)))
      v2_cut_label_string(lbl2, lbl2+LABEL_SIZE_ITEM, "");
#else
   if (eol_label_recs > 0)
      v2_cut_label_string(lbl2, lbl2+LABEL_SIZE_ITEM, "");
#endif

   return SUCCESS;
}
