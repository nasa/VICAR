#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of GEOMVO                                         */
/************************************************************************/

void zgeomvo(conv,icam,res)
	int icam;	/* Viking Orbiter camera serial number (input) */
	void *res;      /* image-space reseau locations (input) */
	void *conv;     /* GEOMA parameters (output) */

{
FTN_NAME2(geomvo, GEOMVO) (conv,&icam,res);
}

