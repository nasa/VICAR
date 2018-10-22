/*  This is the PHIST C Subroutine for the phist.f program  */
/*  which produces a histogram.                             */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zphist(freq,ns,ilow,ihigh,ispike,imode)

int *freq,ns,ilow,ihigh,ispike,imode;

{
     FTN_NAME(phist)(freq,&ns,&ilow,&ihigh,&ispike,&imode);
}
