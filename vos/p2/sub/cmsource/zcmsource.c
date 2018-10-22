#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcmsource - determine the source of the C-matrix...*/
/************************************************************************/


void zcmsource(sedr,isource)
void *sedr;			/* buffer of SEDR/SPICE data from GETSPICE*/
int  *isource;			/* integer code of C-matrix source      */

{
FTN_NAME2(cmsource, CMSOURCE) ( sedr, isource);
}
