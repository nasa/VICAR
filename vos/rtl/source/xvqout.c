#include "xvmaininc.h"
#include "ftnbridge.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"

#if VMS_OS
#pragma nostandard		/* turn off portability check on PUBLICREF */
#endif

PUBLICREF int in_vicar;

#if VMS_OS
#pragma standard
#endif

/************************************************************************/
/* xvqout, zvq_out - Send a filled parameter block back to TAE, or to	*/
/* Shell-VICAR, for processing of output variables.			*/
/************************************************************************/

int FTN_NAME2(xvqout, XVQOUT) (void *parblk)
{
   return zvq_out(parblk);
}

int zvq_out(void *parblk)
{
   int status = 0;

#if RTL_USE_TAE && RTL_USE_SHELL_VIC
   if (in_vicar)
      status = q_out(parblk);		/* TAE version */
   else
      status = zzq_out(parblk);		/* Shell-VICAR version */
#else
#if RTL_USE_TAE
   status = q_out(parblk);
#else
#if RTL_USE_SHELL_VIC
   status = zzq_out(parblk);
#else
   status = 0;		/* neither? */
#endif	/* shvic only */
#endif	/* tae only */
#endif	/* both */

   return status;

}

