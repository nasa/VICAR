#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of VOLABV2                                         */
/************************************************************************/

void zvolabv2(ind,unit,lbuff) 
  int    *ind;     /* returned status (output) */
  int    unit;    /* the unit of the file to be read (input) */
  void  *lbuff;   /* a 40 longword workspace buffer  (input) */


{
FTN_NAME(volabv2)(ind,&unit,lbuff); 
}

