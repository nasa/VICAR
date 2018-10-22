#ifndef MP_PCONSTANTS_H
#define MP_PCONSTANTS_H

        /*
         *      MP_PCONSTANTS.H
         *
	 *  This file contains the prototype for mpPConstants(), which was
	 * removed from mp_routines.h so that mp_routines  code could be
	 * moved to p1. Because it's used by SPICE code, mpPConstants() was
	 * left in p2 to avoid having any SPICE dependencies in p1.
         *
         * Revision History:
	 *
         * October 5, 1998     	msm     mp_pconstant.h created
         */

#include "mp_routines.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _NO_PROTO
int mpPConstants();
#else	/* _NO_PROTO */
int mpPConstants( MP, char *, char * );
#endif	/* _NO_PROTO */

#ifdef __cplusplus
}	/* end extern "C" */
#endif

#endif

