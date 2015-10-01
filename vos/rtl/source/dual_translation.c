#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

static void *tbuffer = NULL;
static int tbufsize = 0;

/************************************************************************/
/* Handle a dual translation by passing the data through both functions	*/
/* using a temporary buffer.						*/
/************************************************************************/

int v2_dual_translation(void *from, void *to, int len, struct trans *trans)
{
   int status;

   if (len * trans->midpixsize > tbufsize) {	/* need a bigger temp buffer */
      if (tbuffer != NULL)
         free(tbuffer);
      tbufsize = len * trans->midpixsize;
      tbuffer = malloc(tbufsize);
      if (tbuffer == NULL) {
         tbufsize = 0;
         return INSUFFICIENT_MEMORY;
      }
   }
   status = (*trans->transfn1)(from,tbuffer,len,trans);
   if (status != SUCCESS)
      return status;
   status = (*trans->transfn2)(tbuffer,to,len,trans);
   if (status != SUCCESS)
      return status;

   return SUCCESS;
}

/************************************************************************/
/* Handle possibly non-aligned input translations.  They must be type	*/
/* only - no host conversions.  If the buffer actually _is_ aligned, we	*/
/* simply call the type translation routine.				*/
/* This is in the same file as dual_translation in order to take	*/
/* advantage of the same static temp buffer...				*/
/************************************************************************/

int v2_align_in_translation(void *from, void *to, int len, struct trans *trans)
{
   int buf_len;

   if ((((long)from) % trans->spixsize) == 0) {		/* we're aligned */
      return (*trans->transfn2)(from, to, len, trans);
   }

   buf_len = len * trans->spixsize;

   if (buf_len > tbufsize) {	/* need a bigger temp buffer */
      if (tbuffer != NULL)
         free(tbuffer);
      tbufsize = buf_len;
      tbuffer = malloc(tbufsize);
      if (tbuffer == NULL) {
         tbufsize = 0;
         return INSUFFICIENT_MEMORY;
      }
   }
   memcpy(tbuffer, from, buf_len);

   return (*trans->transfn2)(tbuffer, to, len, trans);
}

/************************************************************************/
/* Handle possibly non-aligned output translations.  They must be type	*/
/* only - no host conversions.  If the buffer actually _is_ aligned, we	*/
/* simply call the type translation routine.				*/
/* This is in the same file as dual_translation in order to take	*/
/* advantage of the same static temp buffer...				*/
/************************************************************************/

int v2_align_out_translation(void *from, void *to, int len, struct trans *trans)
{
   int status;
   int buf_len;

   if ((((long)to) % trans->dpixsize) == 0) {		/* we're aligned */
      return (*trans->transfn1)(from, to, len, trans);
   }

   buf_len = len * trans->dpixsize;

   if (buf_len > tbufsize) {	/* need a bigger temp buffer */
      if (tbuffer != NULL)
         free(tbuffer);
      tbufsize = buf_len;
      tbuffer = malloc(tbufsize);
      if (tbuffer == NULL) {
         tbufsize = 0;
         return INSUFFICIENT_MEMORY;
      }
   }

   status = (*trans->transfn1)(from, tbuffer, len, trans);
   memcpy(to, tbuffer, buf_len);

   return status;
}

