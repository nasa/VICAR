#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

int v2_trans_curr_hist_item(int unit, int *lblsize, char *recbuf)
{
   int status;
   char buf[MAX_STRING_SIZE+1];

   buf[0] = '\0';

   v2_build_history_label(buf);

/* At this point the current history label is in buf, and the	*/
/* 'move...' routine is moving it to the output file associated	*/
/* with unit.  'recbuf' is used as the buffer that will hold	*/
/* output label items until it is filled and written out to the	*/
/* output file, a function performed by	move_to_output_label().	*/

   status = v2_move_to_output_label(unit, buf, strlen(buf), recbuf);
   if (status != SUCCESS)
      return status;

   *lblsize += strlen(buf);		/* Keep the label size count current */

/* At this point, the last label item has been added to the output file; */
/* if recbuf is not null, then it has label items to be written out, so  */
/* 'write_rec' does this.						 */

   if (strlen(recbuf) > 0) {
      status = v2_write_rec((struct bufstate *) CURRENT_IP_VALUE(BUFSTATE),
			 recbuf, label_record++, 0, 0, &no_trans);
      if (status != SUCCESS)
         return status;
   }

   return SUCCESS;
}
