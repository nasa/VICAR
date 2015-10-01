#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#if VMS_OS
#include <ssdef.h>
#endif

/* This routine is called in the course of input file opening	*/
/* and initializes the unit table associated with the file with	*/
/* items in the file label.					*/

/* At this point, all items in the device state table devstate	*/
/* are valid, so we may use v2_read_blocks.  However, the buffer*/
/* state has not yet been fully initialized (since we don't	*/
/* know things like the record size yet!), so we can't use	*/
/* read_buf.  Bufstate->blocksize is valid.  If the valid bit	*/
/* is set, then we can read directly from bufstate->buffer.	*/
/* This is only true for memory and array files.		*/

/* This is where dual-format PDS/VICAR labels are handled as	*/
/* well.  If the PDS key is found at the beginning of the label,*/
/* we interpret just enough to figure out where the VICAR file	*/
/* starts, then set the file offset to there and try again.	*/

#define AMOUNT_TO_READ	512	/* must be enough to get entire system label */

int v2_initialize_from_label(int unit)
{
   struct bufstate *bufstate;
   struct devstate *devstate;
   int blocksize;
   V2_OFFSET nblocks;
   int tempsize;
   char *temp;
   int i, status;
   int vallen;
   char *value, *ip;
   int essential[N_UNIT_TABLE_ENTRIES];
   int repeat_read, found_pds;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   devstate = &bufstate->devstate;
   blocksize = bufstate->blocksize;

   memset(essential, FALSE, sizeof(essential));

   essential[FORMAT] = TRUE;
   essential[BUFSIZE] = TRUE;
   essential[RECSIZE] = TRUE;
   essential[NL] = TRUE;
   essential[NS] = TRUE;
   essential[NB] = TRUE;
   essential[LBLSIZE] = TRUE;

   repeat_read = TRUE;
   found_pds = FALSE;

   while (repeat_read) {		/* Repeat read if PDS label found */
      repeat_read = FALSE;

      if (bufstate->flags & BUF_VALID) {	/* Get label from buffer */
         tempsize = MIN(AMOUNT_TO_READ, bufstate->bufsize);
         temp = malloc(tempsize + 1);		/* +1 for null terminator */
         if (temp == NULL)
	    return INSUFFICIENT_MEMORY;
         v2_move(temp, bufstate->buffer, tempsize);
      }
      else {					/* Read label off device */
         nblocks = 1;
         if (blocksize >= AMOUNT_TO_READ)
	    tempsize = blocksize;
         else if (blocksize == 0)		/* shouldn't happen */
	    tempsize = AMOUNT_TO_READ;
         else {
	    tempsize = CEIL(AMOUNT_TO_READ, blocksize) * blocksize;
	    nblocks = tempsize / blocksize;
         }
         temp=malloc(tempsize + bufstate->buf_extra + 1); /* +1 for null term */
         if (temp == NULL)
	    return INSUFFICIENT_MEMORY;

         status = v2_read_blocks(devstate, temp, (V2_OFFSET)0, nblocks, FALSE);
#if VMS_OS
         if (status != SUCCESS && status!=SS$_DATAOVERUN && status!=END_OF_FILE)
#else
         if (status != SUCCESS && status != END_OF_FILE)
#endif
         {
            free(temp);
	    return status;
         }
      }
      *(temp+tempsize) = '\0';		/* Make sure it is a c string. */

      /* Check for PDS label */

      if (!found_pds) {			/* don't do it twice! */
         if ((strncmp(temp, "PDS_VERSION_ID", 14) == 0) ||
	     (strncmp(temp, "ODL_VERSION_ID", 14) == 0)) {

            V2_OFFSET pds_offset = v2_find_pds_offset(temp);

            if (pds_offset != 0) {		/* found a valid label */
               status = v2_set_file_offset(unit, pds_offset);
	       if (status == SUCCESS) {

	          /* Everything worked.  Set the flags so we go back and   */
	          /* read the VICAR header again, now that we know where   */
	          /* it is.  Note that if anything fails, we simply do	   */
	          /* nothing, and let the VICAR parser below report errors */
	          /* as it normally would.				   */

	          free(temp);
	          found_pds = TRUE;
	          repeat_read = TRUE;
	       }
	    }
	 }
      }
   }					/* end while (repeat_read) */

/* Loop thru the unit table: for every item that is a system entry, retrieve */
/* its value from the system label and enter it into the current value table.*/
/* HOST, INTFMT, and REALFMT are treated specially because their defaults    */
/* are different for input and output files.  Input default==VAX always,     */
/* output default==native format.  BHOST, BINTFMT, and BREALFMT are also     */
/* different for input vs. output, but the output is more complex so the     */
/* input default is in the table and no special handling is needed here.     */

   for (i=0; i<N_UNIT_TABLE_ENTRIES; i++) {
      if (unit_table[i].mode == SYSTEM) {	/* extract value from label */
	 if (!v2_find_entry(temp, unit_table[i].name, &value, &vallen, &ip)) {
	    if (essential[i]) {
               free(temp);
	       return FAILURE;
            }
	    if (i == HOST)	/* special defaults for non-existent labels */
	       v2_add_lbl_item_value_tbl(unit,i,DEF_HOST_LABEL);
	    if (i == INTFMT)
	       v2_add_lbl_item_value_tbl(unit,i,DEF_INTFMT_LABEL);
	    if (i == REALFMT)
	       v2_add_lbl_item_value_tbl(unit,i,DEF_REALFMT_LABEL);
	 }
	 else			/* add 'value' to current value table */
	    v2_add_lbl_item_value_tbl(unit,i,v2_dequoted(value,vallen));
      }
   }

   free(temp);
   return SUCCESS;
}
