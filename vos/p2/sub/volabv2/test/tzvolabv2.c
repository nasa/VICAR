#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVOLABV2.F */
/************************************************************************/

void FTN_NAME(tzvolabv2)(ind,unit,lbuff) 
  int   *ind;     /* returned status (output) */
  int   *unit;    /* the unit of the file to be read (input) */
  void  *lbuff;   /* a 40 longword workspace buffer  (input) */

{
       zvolabv2(ind,*unit,lbuff);
}

