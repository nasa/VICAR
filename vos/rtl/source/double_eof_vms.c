#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <iodef.h>

/* This routine is called at the close of an ouput tape file; it writes two */
/* end of file marks and then backs up over the last.			    */

int double_eof(tapestate)
struct tapestate *tapestate;
{
   int code;

   code = sys$qiow(tapestate->io_event, tapestate->channel,  /* Write an EOF. */
		   IO$_WRITEOF, &tapestate->iosb,
		   NULL, NULL, NULL,
		   NULL, NULL, NULL,
		   NULL, NULL);

   if (code != SUCCESS)
      return code;
   if (tapestate->iosb.status != SUCCESS)
      return tapestate->iosb.status;

   code = sys$qiow(tapestate->io_event, tapestate->channel,  /* Write an EOF. */
		   IO$_WRITEOF, &tapestate->iosb,
		   NULL, NULL, NULL,
		   NULL, NULL, NULL,
		   NULL, NULL);

   if (code != SUCCESS)
      return code;
   if (tapestate->iosb.status != SUCCESS)
      return tapestate->iosb.status;

   /* Backspace over the file marks. */

   code = sys$qiow(tapestate->io_event, tapestate->channel,
		   IO$_SKIPFILE, &tapestate->iosb,
		   NULL, NULL, -2,
		   NULL, NULL, NULL,
		   NULL, NULL);

   if (code != SUCCESS)
      return code;
   if (tapestate->iosb.status != SUCCESS)
      return tapestate->iosb.status;

   return SUCCESS;
}

#else

int double_eof(tapestate)
{
   return NO_TAPE_SUPPORT;
}

#endif

