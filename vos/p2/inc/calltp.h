/*
 CallTp.h: Contains subroutine that starts-up interactive
 tiepoint collection program and gets tiepoints when that
 program exits.  The tiepoints are transformed back and forth
 through a temporary file set in the user's environment.
*/
#ifndef CALLTP_H
#define CALLTP_H

#ifdef __cplusplus
extern "C" {
#endif

typedef double * TPOINT;

#ifndef _NO_PROTO

double **display_points(const char *file1, const char *file2,
			double array[][4], int *npoints, 
			char *pathname, int *tp_status);

#ifdef __VMS
#ifndef VMS_STRDUP_SIM_DEFINED
#define VMS_STRDUP_SIM_DEFINED
#include <string.h>
// VMS (at least Alpha OpenVMS 6.1) does not have strdup, so we create one here
// This is cribbed from Young's release notes
char *strdup(const char *str);
#endif
#endif

#else 

double **display_points();

#ifdef __VMS
#ifndef VMS_STRDUP_SIM_DEFINED
#define VMS_STRDUP_SIM_DEFINED
#include <string.h>
// VMS (at least Alpha OpenVMS 6.1) does not have strdup, so we create one here
// This is cribbed from Young's release notes
char *strdup();
#endif
#endif

#endif

#ifdef __cplusplus
}
#endif

#endif
