#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine moves a buffer of label information in 'buf' of size	*/
/* 'size' into the output file label associated with 'unit'.  If the	*/
/* device is sequential, the label is kept in memory to be written out	*/
/* later.  Otherwise, the label information will be stored in 'recbuf'	*/
/* which has been allocated to be the length of an image record for	*/
/* 'unit'.  When 'recbuf' becomes filled, this routine will physically	*/
/* write it to the file associated with 'unit'.				*/

int v2_move_to_output_label(int unit, char *buf, int size, char *recbuf)
{
   int rem_rec, max_rec;	/* Remaining and max size in recbuf */
   int size_left;		/* Amount of input data left */
   int len;
   int status;
   int offset = 0;

   size_left = size;

   max_rec = CURRENT_I_VALUE(RECSIZE);
   rem_rec = max_rec - strlen(recbuf);	/* Empty space left in recbuf */

   while (size_left > 0) {		/* Keep going until all data gone */

      /* Copy as much as we can to 'recbuf' */

      len = MIN(size_left,rem_rec);
      strncat(recbuf, buf+offset, len);
      /* Note:  Strncat, unlike strncpy, always appends a NULL, which may */
      /* come after the length.  The buffer is allocated one bigger in    */
      /* create_output_label to compensate for this.  There is no         */
      /* guaranteed null terminator in the label.			  */

      size_left -= len;
      rem_rec -= len;
      offset += len;

      if (rem_rec == 0) {		/* If recbuf is full write it out  */

         status = v2_write_rec((struct bufstate *)CURRENT_IP_VALUE(BUFSTATE), 
			    recbuf, label_record++, 0, 0, &no_trans);
         if (status != SUCCESS)
            return status;

         memset(recbuf, '\0', max_rec);		/* Zero out recbuf */

         rem_rec = max_rec;
      }
   }

   return SUCCESS;
}
