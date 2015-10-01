#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine will read the history labels from the primary input and	*/
/* transfer them to the current output file associated with 'unit'.	*/
/* 'lblsize' will be used to keep a running count of the label size;	*/
/* recbuf will act as the current output buffer.			*/

int v2_trans_prim_hist_labels(
   int unit,		/* In: unit number */
   int *lblsize,	/* In/Out: label size */
   char *recbuf		/* In: output buffer (see move_to_output_label()) */
)

{
   int status;
   char *loc;
   char *p;

/* The primary labels are stored in memory.  If they are not	*/
/* there, there must be some internal error.			*/

   p = PRIMARY_S_VALUE(LABELS);
   if (p == NULL)
      return INTERNAL_ERROR;

   if (!v2_find_first_hist_item(p,&loc))
      return SUCCESS;			/* No history labels */

   /* Move the history labels to the output label */

   status = v2_move_to_output_label(unit, loc, strlen(loc), recbuf);
   if (status != SUCCESS)
      return status;

   *lblsize += strlen(loc);		/* Keep the label size updated. */

   return SUCCESS;
}
