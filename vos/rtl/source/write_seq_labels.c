#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Output labels on a sequential device may be modified before the first  */
/* write of data.  When the first data write is desired, any modified     */
/* labels, kept in LABELS, must be written first. This routine does that. */

int v2_write_seq_labels(int unit)
{
   int i, status;
   int recsize;
   int lblsize;
   int n_label_recs;
   char *labels, *p, *tape_labels;

   recsize = CURRENT_I_VALUE(RECSIZE);
   labels = (char *)CURRENT_IP_VALUE(LABELS);
   if (labels == NULL)
      return SUCCESS;

   lblsize = strlen(labels);

   n_label_recs = CEIL(lblsize, recsize);
   i = n_label_recs * recsize;

   v2_add_label_size_item(labels, i);
   CURRENT_I_VALUE(LBLSIZE) = i;

   tape_labels = malloc(i);	/* make sure end of last partial rec is 0's */
   if (tape_labels == NULL)
      return NO_MEMORY_FOR_LABEL_PROCESS;
   memset(tape_labels, 0, i);		/* zero the whole buffer */
   strcpy(tape_labels, labels);		/* copy the label string in */
   p = tape_labels;

   for (i=0; i < n_label_recs; i++) {
      status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE), p,
					i, 0, 0, &no_trans);
      if (status != SUCCESS) {
         free(tape_labels);
         return status;
      }
      p += recsize;
   }

   return SUCCESS;
}
