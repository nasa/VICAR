/*  TEST PGM FOR HSTGNB */
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzhstgnb)(nsamp,pixlin,hist)
/* ...ARGUMENT DECLARATIONS  */

   int *nsamp;
   void *pixlin,*hist;

{
   zhstgnb(*nsamp,pixlin,hist);
   return;
}

