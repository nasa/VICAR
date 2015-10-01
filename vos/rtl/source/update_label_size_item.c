#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* When all the labels have been written an item of the system label,	*/
/* the LBLSIZE, indicating the size of the label, must be updated on	*/
/* the system label.  'lblsize' contains the size of the current label	*/
/* for 'unit'.								*/

int v2_update_label_size_item(int unit, int lblsize)
{
   int recsize, nrecs, label_size, left;
   int i, status;
   char *p;
   char buf[MAX_SIMPLE_LABEL_ITEM_SIZE+1];

/* Make the label size a multiple of the record size */

   recsize = CURRENT_I_VALUE(RECSIZE);
   nrecs = CEIL(lblsize, recsize);

   CURRENT_I_VALUE(LBLSIZE) = nrecs*recsize;
   label_size = nrecs*recsize;

   if (SEQ_DEVICE) {		/* use normal label modification procedure */
      status = v2_read_in_labels(unit, &p, &CURRENT_I_VALUE(LBLALLOC));
      if (status != SUCCESS)
         return status;
      CURRENT_S_VALUE(LABELS) = p;
      v2_add_label_size_item(p, label_size);
      CURRENT_I_VALUE(FLAGS) |= LABELS_MODIFIED;

      return SUCCESS;
   }

/* Label is written out to device */

   memset(buf, 0, MAX_SIMPLE_LABEL_ITEM_SIZE+1);
   v2_add_label_size_item(buf,label_size);

   nrecs = strlen(buf) / recsize;
   p = buf;

   for (i=0; i<nrecs; i++) {		/* write out any full records first */
     status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE), p, 
			i, 0, 0, &no_trans);
      p += recsize;
      if (status != SUCCESS)
         return status;
   }

   left = strlen(buf) % recsize;	/* write out remaining partial rec */
   if (left != 0) {
      status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE),p,
			 nrecs,0,left,&no_trans);
      if (status != SUCCESS)
         return status;
   }

   return SUCCESS;
}
