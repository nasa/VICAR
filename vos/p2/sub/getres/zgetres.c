#include  "xvmaininc.h"  
#include  "ftnbridge.h"

/************************************************************************/
/*  C-Callable Version  ZGetres                                         */
/*  (See Fortran Source code of GETRES)                                 */
/************************************************************************/


void  zgetres (l, c)  
float   l[404];      /* location,  output  */
int     c;             /* camera #,  input  */
{
FTN_NAME2(getres, GETRES) (&l[0], &c) ;
}
