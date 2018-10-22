#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TGEOMVO.F */
/************************************************************************/

void FTN_NAME(tzgeomvo)(conv,icam,res)
	int *icam;	/* Viking Orbiter camera serial number (input) */
	void *res;      /* image-space reseau locations (input) */
	void *conv;     /* GEOMA parameters (output) */
{
       zgeomvo(conv,*icam,res);
}

