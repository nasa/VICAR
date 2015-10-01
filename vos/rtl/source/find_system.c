#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Determine the starting and stopping point for the system label */

int v2_find_system(
   int unit,			/* in: unit number */
   char **start,		/* out: starting point of system label */
   char **stop			/* out: ending point of system label */
)

{
   char *pstart, *pstop;
   char *p, *value;
   int vallen;

   p = CURRENT_S_VALUE(LABELS);

   pstop = v2_find_entry(p, SYSTEM_KEY, &value, &vallen, &pstart);
   if (pstop == NULL)
      return NO_SYSTEM_LABEL;
   *start = pstart;

   pstop = v2_find_entry(p, PROPERTY_KEY, &value, &vallen, &pstart);
   if (pstop != NULL) {	/* find end of system (==start of property) label */
      *stop = pstart;
   }
   else {		/* No PROPERTY label, look for start of history label */
      pstop = v2_find_entry(p, TASK_KEY, &value, &vallen, &pstart);
      *stop = pstart;
   }

   return SUCCESS;
}
