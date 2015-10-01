#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Appends the standard history label for this task to 'ps'.	*/
/* 'ps' is assumed to be big enough to hold the label.		*/

void v2_build_history_label(char *ps)
{
   int i;

   for (i=0; i<N_HISTORY_ITEMS; i++) {
      strcat(ps, history[i].name);
      strcat(ps, "='");
      strcat(ps, history[i].value);
      strcat(ps, "'  ");
   }

   return;
}
