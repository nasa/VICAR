#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

#if RTL_USE_TAPE

#include <dvidef.h>

/* Get the record size of the tape requested during mount via a getdvi call */

int v2_det_tape_recsize(unit)
int unit;
{
   struct bufstate *bufstate;
   struct tapestate *tapestate;
   int status;
   int rec;
   short dummy;

   struct {
      short length;
      short code;
      int *addr;
      short *ret_adr;
      int zero;
   } item = {4,DVI$_RECSIZ,&rec,&dummy,0};

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   tapestate = &bufstate->devstate.dev.tape;

   if (CURRENT_I_VALUE(RECSIZE) == 0) {

      status = sys$getdviw(0,tapestate->channel,0,&item,0,0,0,0);
      if (status != SUCCESS)
	 return status;
   
      if (rec == 0)
	 rec = bufstate->blocksize;

      CURRENT_I_VALUE(RECSIZE) = rec;
   }

   return SUCCESS;
}

#else

int v2_det_tape_recsize()
{
   return NO_TAPE_SUPPORT;
}

#endif

