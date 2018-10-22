#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TTIECHV.F */
/************************************************************************/
void FTN_NAME(tztiechv) (indr,ityp,pbuf,npnt,convp)
int *indr;     /* indicator on return--0 if OK (output) */
int *ityp;     /* 0 = no distortion correction
                 1 = perform distortion correction (input) */
void *pbuf;    /* tiepoint parameter buffer   
                 in order line,sample,lat,long,... (input) */
int *npnt;   /* number of points (input) */
void *convp;   /*  GEOMA parameters to be used by TRITRA 
               for geometric correction of image space
               tiepoints (input) */
{
      ztiechv(*indr,*ityp,pbuf,*npnt,convp);
}
