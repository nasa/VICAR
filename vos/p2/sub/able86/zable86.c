#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version ZABLE86  (See Fortran Source ABLE86)             */
/************************************************************************/

void  zable86 (ind,unit,buf)  
int   *ind;          /* ind = outputted status indicator   */
int   unit;          /* VICAR  unit #      */
int   *buf;          /* data buffer        */
{
FTN_NAME2(able86, ABLE86) (ind, &unit, buf) ;
}
