#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Allocate a new unit, return it's number, and activate the unit */

int v2_activate_a_unit(int *unit, int instance, char *name)
{
   int i;

   for (i=0; i<N_ACTIVE_UNITS_ALLOWED; i++) {
      if ((EQUAL(active_units[i].name,name)) &&
          (active_units[i].instance == instance) &&
          (active_units[i].unit == V2_ACTIVE)) {     /* unit already active */

         *unit = i;

         if (!(current_table[*unit][FLAGS].ivalue & OPEN))
	    v2_deassign_value_block(*unit);	/* deallocate all the memory */
         return SUCCESS;
      }
   }

   for (i=0; i<N_ACTIVE_UNITS_ALLOWED; i++) {
      if (active_units[i].unit == V2_INACTIVE) {

         *unit = i;

         if (current_table[i] == NULL) {
            current_table[i] = (VALUE_TABLE *)malloc(
				sizeof(VALUE_TABLE)*N_UNIT_TABLE_ENTRIES);
            label_table[i] = (VALUE_TABLE *)malloc(
				sizeof(VALUE_TABLE)*N_LABEL_TABLE_ENTRIES);
            if (current_table[i] == NULL || label_table[i] == NULL) {
               if (current_table[i] != NULL)
                  free(current_table[i]);
               continue;	/* maybe another empty one is already alloc'd */
            }
         }
         strcpy(active_units[*unit].name,name);
         active_units[*unit].instance = instance;
         active_units[*unit].unit = V2_ACTIVE;
         v2_full_init_value_tbl(*unit);
         return SUCCESS;
      }
   }

   return NO_FREE_UNITS;
}

