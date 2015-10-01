#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* This routine is called at the close of an ouput tape file; it writes	*/
/* two end of file marks and then backs up over the last.		*/

int v2_double_eof(tapestate)
struct tapestate *tapestate;
{
   int code;
   struct mtop op;

   op.mt_op = MTWEOF;
   op.mt_count = 2;

   code = ioctl(tapestate->channel, MTIOCTOP, &op);

   if (code == -1)
      return errno;

   op.mt_op = MTBSF;
   op.mt_count = 2;

   code = ioctl(tapestate->channel, MTIOCTOP, &op);

   if (code == -1)
      return errno;

   return SUCCESS;
}

#else

int v2_double_eof(struct tapestate * UNUSED(tapestate))
{
   return NO_TAPE_SUPPORT;
}

#endif

