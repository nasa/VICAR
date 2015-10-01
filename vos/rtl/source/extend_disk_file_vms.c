#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include <rms.h>

/* Increases the allocation size of the file associated with 'diskstate'. */

int v2_extend_disk_file(diskstate, amount)
struct diskstate *diskstate;
V2_OFFSET amount;
{
   struct FAB fab;
   int status;

   fab = cc$rms_fab;
   fab.fab$l_fna = diskstate->name;
   fab.fab$b_fns = strlen(diskstate->name);

   sys$dassgn(diskstate->channel);

   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
   fab.fab$b_org = FAB$C_REL;

   status = sys$open(&fab);
   if (status != RMS_SUCCESS)
      return status;

   fab.fab$l_alq = amount;

   status = sys$extend(&fab);
   if (status != RMS_SUCCESS)
      return status;

   sys$close(&fab);

   fab.fab$l_dna = default_file_name;
   fab.fab$b_dns = strlen(default_file_name);
   fab.fab$l_fna = diskstate->name;
   fab.fab$b_fns = strlen(diskstate->name);
   fab.fab$b_fac = FAB$M_BIO | FAB$M_GET | FAB$M_PUT;
   fab.fab$b_org = FAB$C_SEQ;
   fab.fab$l_fop = FAB$M_UFO;

   status = sys$open(&fab);
   if (status != RMS_SUCCESS)
      return status;

   diskstate->channel = fab.fab$l_stv;
   diskstate->allocation = fab.fab$l_alq;

   return SUCCESS;
}
