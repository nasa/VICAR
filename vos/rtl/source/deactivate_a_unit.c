#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Marks a unit as inactive and deletes all memory allocated to it.	*/
/* The current_table/label_table entries themselves are not deallocated	*/
/* (they can be re-used for the next unit).				*/

int v2_deactivate_a_unit(int unit)
{

   if (active_units[unit].unit == V2_INACTIVE)
      return SUCCESS;

   v2_deassign_value_block(unit);
   active_units[unit].unit = V2_INACTIVE;

   return SUCCESS;
}
