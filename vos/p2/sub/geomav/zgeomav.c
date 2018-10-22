#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version  ZGeomav  */
/*  (See Fortran Source code of GEOMAV)                                 */
/************************************************************************/


void  zgeomav (con, n, ir)  
float   con[] ;      /*  CONV[2216], output  */
int     n ;          /*  camera SN #,  input  */
float   ir[]  ;      /*  ISRES[404], Input    */
{
FTN_NAME2(geomav, GEOMAV) (&con[0], &n, &ir[0]) ;
}
