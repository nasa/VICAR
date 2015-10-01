/************************************************************************/
/* Get the project name for an image.  See the .c/.hlp files for usage.	*/
/************************************************************************/

#ifndef _GETPROJ_H
#define _GETPROJ_H

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _NO_PROTO
void zgetproj();
#else
void zgetproj(int unit,
	     char *project,
	     int *camera,
	     int *fds,
	     int *ind);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _GETPROJ_H */

