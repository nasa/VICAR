#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of searc_distor */
/************************************************************************/

void zsearc_distor(int unit, int *ind)
{
FTN_NAME2_(searc_distor, SEARC_DISTOR) (&unit,ind);
}
