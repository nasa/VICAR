#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>

/* Delete the file */

int delete_file(unit)
int unit;
{
   struct bufstate *bufstate;
   struct FAB fab;
   int status;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   if (bufstate->devstate.device_type != DEV_DISK &&
	bufstate->devstate.device_type != DEV_ARRAY)
      return SUCCESS;

   fab= cc$rms_fab;

   fab.fab$l_fna = CURRENT_S_VALUE(NAME);
   fab.fab$b_fns = strlen(CURRENT_S_VALUE(NAME));
   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);

   status = sys$erase(&fab);
   if (status != RMS_SUCCESS)
      return status;

   return SUCCESS;
}
