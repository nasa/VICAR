#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Deallocates all strings that have been allocated to a unit, and	*/
/* resets all values in the CURRENT_TABLE to their default values.	*/

void v2_deassign_value_block(int unit) 
{
   int i;

   for (i=0; i<N_UNIT_TABLE_ENTRIES; i++) {
      if (unit_table[i].type == STRING || unit_table[i].type == MESSAGE) {
         if (current_table[unit][i].pvalue != default_table[i].pvalue) {
            free(current_table[unit][i].pvalue);
         }
         current_table[unit][i].pvalue = default_table[i].pvalue;
      }
   }

   for (i=0; i<N_LABEL_TABLE_ENTRIES; i++) {
      if (label_options[i].type == STRING || unit_table[i].type == MESSAGE) {
         if (label_table[unit][i].pvalue != label_default_table[i].pvalue) {
            free(label_table[unit][i].pvalue);
            label_table[unit][i].pvalue = label_default_table[i].pvalue;
         }
      }
   }
}
