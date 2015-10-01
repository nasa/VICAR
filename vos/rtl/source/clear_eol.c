#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* When labels are modified in update mode they may shrink; as a result	*/
/* they may not need an EOL label area.  This routine sets the EOL	*/
/* keyword in the first label area to 0.  'P' points at the label.	*/

void v2_clear_eol(char *p) 
{
   char *place;
   char *value;
   int vallen;

   if (v2_find_entry(p, "EOL", &value, &vallen, &place) != NULL)
      *value = '0';

   return;
}
