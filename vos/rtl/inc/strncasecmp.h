/************************************************************************/
/* Simulation for strncasecmp() on VMS.					*/
/************************************************************************/

#ifndef STRNCASECMP_H
#define STRNCASECMP_H

#include "xvmaininc.h"

#if VMS_OS

#include <ctype.h>

	/* Make sure not to do it twice */

#ifndef VMS_STRNCASECMP_SIM_DEFINED
#define VMS_STRNCASECMP_SIM_DEFINED

static int strncasecmp(char *s1, char *s2, int n)
{
   int i;
   char a, b;

   for (i=0; i<n && *s1 && *s2; i++) {
      char a = (isalpha(*s1) ? tolower(*s1) : *s1);
      char b = (isalpha(*s2) ? tolower(*s2) : *s2);
      if (a < b) return -1;
      if (a > b) return 1;
      s1++; s2++;
   }
   if (i==n) return 0;
   if (*s1 && !*s2) return 1;	/* ran out of s2 first */
   if (*s2 && !*s1) return -1;	/* ran out of s1 first */
   return 0;
}

#endif		/* !defined */
#endif		/* VMS */

#endif		/* STRNCASECMP_H */

