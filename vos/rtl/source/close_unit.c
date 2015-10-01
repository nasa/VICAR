#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Close_unit resets everything in the CURRENT_TABLE to their default values */
/* (and frees the memory that may have been allocated) except for NAME,      */
/* OPEN_ACT, OPEN_MESS, IO_ACT, and IO_MESS.  NAME must be preserved since   */
/* it is set by xvunit and one xvunit call is advertised to serve multiple   */
/* xvopens and xvcloses.  OPEN_ACT and IO_ACT (and _MESS) must be retained   */
/* as well since error processing may follow this point if an error occurs   */
/* or if this routine were called when an attempted open failed.             */
/* See also final_cleanup().						     */

void v2_close_unit(int unit)
{
   int i;

   if (CURRENT_IP_VALUE(BUFSTATE) != NULL)
       free(CURRENT_IP_VALUE(BUFSTATE));

   for (i=0; i<N_UNIT_TABLE_ENTRIES; i++) {
      if ((i != NAME) && (i != OPEN_ACT) && (i != OPEN_MES) &&
			 (i != IO_ACT) && (i != IO_MESS)) {
         if (unit_table[i].type == STRING || unit_table[i].type == MESSAGE) {
            if (CURRENT_S_VALUE(i) != default_table[i].pvalue) {
               free(CURRENT_S_VALUE(i));
            }
         }
         CURRENT_VALUE(i) = default_table[i].value;
      }
   }

   for (i=0; i<N_LABEL_TABLE_ENTRIES; i++) {
      if (label_options[i].type == STRING || label_options[i].type == MESSAGE) {
         if (LABEL_S_VALUE(i) != label_default_table[i].pvalue) {
            free(LABEL_S_VALUE(i));
         }
      }
      LABEL_VALUE(i) = label_default_table[i].value;
   }

   return;
}
