#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine will read the property labels from the primary input	*/
/* and transfer them to the current output file associated with 'unit'.	*/
/* 'lblsize' will be used to keep a running count of the label size;	*/
/* recbuf will act as the current output buffer.			*/

int v2_trans_prim_prop_labels(
   int unit,		/* In: unit number */
   int *lblsize,	/* In/Out: label size */
   char *recbuf		/* In: output buffer (see move_to_output_label()) */
)

{
   int status;
   char *loc, *endloc;
   char *p;
   char *value;
   int vallen;

/* The primary labels are stored in memory.  If they are not	*/
/* there, there must be some internal error.			*/

   p = PRIMARY_S_VALUE(LABELS);
   if (p == NULL)
      return INTERNAL_ERROR;

   if (v2_find_entry(p, PROPERTY_KEY, &value, &vallen, &loc) == NULL)
      return SUCCESS;			/* No property labels */

   v2_find_entry(p,TASK_KEY,&value,&vallen,&endloc); /* Find end of prop lbl */

   /* Move the property labels to the output label */

   status = v2_move_to_output_label(unit, loc, endloc - loc, recbuf);
   if (status != SUCCESS)
      return status;

   *lblsize += endloc - loc;		/* Keep the label size updated. */

   return SUCCESS;
}
