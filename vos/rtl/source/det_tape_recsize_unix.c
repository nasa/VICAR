#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

int v2_det_tape_recsize(int unit)
{
   struct bufstate *bufstate;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (CURRENT_I_VALUE(RECSIZE) == 0)
      CURRENT_I_VALUE(RECSIZE) = bufstate->blocksize;

   return SUCCESS;
}

#else

int v2_det_tape_recsize(int UNUSED(unit))
{
   return NO_TAPE_SUPPORT;
}

#endif

