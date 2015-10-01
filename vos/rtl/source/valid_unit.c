#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Returns SUCCESS if the given unit number is legal and active */

int v2_valid_unit(int unit)
{
   if (unit >= N_ACTIVE_UNITS_ALLOWED || unit < 0)
      return NO_SUCH_UNIT;

   if (active_units[unit].unit == V2_INACTIVE)
      return NO_SUCH_UNIT;

   return SUCCESS;
}
