#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TABLE77V2.F */
/************************************************************************/

void FTN_NAME(tzable77v2)(ind,unit,arra) 
  int   *ind;     /* returned status (output) */
  int   *unit;    /* VICAR unit # (input) */
  void  *arra;   /* Array containing extracted data (output) */

{
       zable77v2(ind,*unit,arra);
}

