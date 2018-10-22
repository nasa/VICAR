#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  Unit test C-bridge for TGETPLACON.F */
/************************************************************************/
void FTN_NAME(tzgetplacon)(planet,id,data,ind)
char planet[12];
void *data;
int *id,*ind;
{
/*  ============================================  */
      zgetplacon(planet,*id,data,ind);
}
