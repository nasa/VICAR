#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzmomati)(m,oal,oas,ssl,sss,scale,fl,sclo,scla,angn,range,a,rs)
int *m;
double *oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range;
void *a, *rs;
{
/*  ============================================  */
  if (*m) {
    zvmessage(" Calling zmomati ...","");
    zmomati(*oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range,a,rs);
  }
  else {
    zvmessage(" Calling momati_c directly ...","");
    momati_c(*oal,*oas,*ssl,*sss,*scale,*fl,*sclo,*scla,*angn,*range,a,rs);
  }
}
