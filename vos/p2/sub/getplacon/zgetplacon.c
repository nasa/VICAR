#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void  zgetplacon(planet,id,data,ind)
char   planet[12];
int    id,*ind;
void   *data;
{
FTN_NAME2(getplacon, GETPLACON) (planet,&id,data,ind);
}
