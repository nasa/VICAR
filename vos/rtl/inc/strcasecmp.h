/************************************************************************/
/* Simulation for strcasecmp() on VMS.					*/
/************************************************************************/

#ifndef STRCASECMP_H
#define STRCASECMP_H

#include "xvmaininc.h"

#if VMS_OS

#include <ctype.h>

	/* Make sure not to do it twice */

#ifndef VMS_STRCASECMP_SIM_DEFINED
#define VMS_STRCASECMP_SIM_DEFINED

static int strcasecmp(char *s1, char *s2)
{
   int i;
   char a, b;

   while (*s1 && *s2) {
      a = (isalpha(*s1) ? tolower(*s1) : *s1);
      b = (isalpha(*s2) ? tolower(*s2) : *s2);
      if (a < b) return -1;
      if (a > b) return 1;
      s1++; s2++;
   }
   if (*s1 && !*s2) return 1;   /* ran out of s2 first */
   if (*s2 && !*s1) return -1;  /* ran out of s1 first */
   return 0;
}

#endif		/* !defined */
#endif		/* VMS */

#endif		/* STRCASECMP_H */

