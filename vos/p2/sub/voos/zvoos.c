#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of VOOS                                         */
/************************************************************************/

void zvoos(icam,oloc)
 int    icam;  /* Input VO camera serial number. */
 void  *oloc;  /* Output object space reseau locations stored
		  as (line,sample) pairs. */

{
FTN_NAME(voos)(&icam,oloc);
}
