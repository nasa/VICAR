#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVOOS.F */
/************************************************************************/

void FTN_NAME(tzvoos)(icam,oloc)
 int   *icam;  /* Input VO camera serial number. */
 void  *oloc;  /* Output object space reseau locations stored
		  as (line,sample) pairs. */

{
       zvoos(*icam,oloc);
}
