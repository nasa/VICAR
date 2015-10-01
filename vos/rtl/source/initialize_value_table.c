#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Certain items in the unit table need to be initialized with their	*/
/* defaults per entry to any of the xv... routines.  A flag exists in	*/
/* unit_table which indicates whether initialization is to take place.	*/
/* For flagged entries, the current value in the corresponding value	*/
/* table is updated with the default.					*/

void v2_initialize_value_table(
   struct UNIT_TABLE  unit_table[],
   int		   n_unit_table_entries,
   VALUE_TABLE current_table[],
   VALUE_TABLE default_table[]
)

{
  int i;

   for (i=0; i<n_unit_table_entries; i++) {
      if (unit_table[i].init == YES) {
         if (unit_table[i].type == STRING || unit_table[i].type == MESSAGE) {
            if (current_table[i].pvalue != default_table[i].pvalue) {
               free(current_table[i].pvalue);
            }
         }
         current_table[i].value = default_table[i].value;
      }
      else
         return;    /* efficiency trick:  all YES's must be at front of table */
   }
   return;
}
