#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Determine the starting and stopping point for the property label	*/
/* determined by 'prop' and 'instance'.  If instance is not found, the	*/
/* last valid instance is returned in *inst_out.  Thus, passing		*/
/* instance=0 allows you to simply count how many instances there are.	*/

int v2_find_property(
   int unit,			/* in: unit number */
   char *prop,			/* in: property name */
   int instance,		/* in: instance # of property */
   char **start,		/* out: pointer to start of property */
   char **stop,			/* out: pointer to (just past) end of prop */
   int *inst_out		/* out: last valid inst # for this name iff */
				/*      instance not found.  May be NULL. */
)

{
   char *p;
   char *pstart, *pstop;
   char *value;
   int vallen;
   int found;
   int label_instance;

   p = CURRENT_S_VALUE(LABELS);

   found = FALSE;
   label_instance = 1;

   *start = NULL;

   while (!found) {
      pstop = v2_find_entry(p, PROPERTY_KEY, &value, &vallen, &pstart);
      if (pstop == NULL)
         break;			/* only way out of loop if strlen(task)==0 */
      if (strcmp(v2_dequoted(value,vallen), prop) == 0) {
         if (instance == label_instance) {	/* found property & instance! */
            *start = pstart;
            found = TRUE;
         }
         label_instance++;
      }
      p = pstop;		/* look for next one */
   }

   if (inst_out)
      *inst_out = label_instance-1;

   if (*start == NULL)
      return NO_SUCH_PROPERTY;

   /* Find end of property (==beginning of next, or of history if no more) */

   pstop = v2_find_entry(p, PROPERTY_KEY, &value, &vallen, &pstart);
   if (pstop == NULL) {		/* No more properties, look for start of hist */
      pstop = v2_find_entry(p, TASK_KEY, &value, &vallen, &pstart);
   }
   *stop = pstart;

   return SUCCESS;
}
