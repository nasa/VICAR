#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"

int v2_open_input_file(unit)
int unit;
{
   int i;
   int h1, h2;
   int def_fil,virgin_file,file;
   int once_opened,current_file,u_fil;
   struct bufstate *bufstate;
   int status;
   int mem;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

#if RTL_USE_TAPE

/* Determine whether unit is a tape by searching the TAE-supplied table	*/
/* 'i_tape'; 'h1' and 'h2' return table index and desired tape file	*/

   status = i_analyze(CURRENT_S_VALUE(NAME), i_tape,i_count, &h1,&h2);

   if ((status==I_TAPE) && (h1>=0)) {      /* The unit is a tape. */
      status = v2_open_tape(unit, bufstate, h1, h2);
      return status;
   }

#endif

   mem = v2_open_memory_file(unit, bufstate);  /* check if unit is a memory file */
   if (mem)
      return SUCCESS;

   status = v2_determine_device(unit, &bufstate->devstate.device_type);
   if (status != SUCCESS)
      return status;

   switch (bufstate->devstate.device_type) {
      case DEV_DISK:
	 status = v2_open_disk_input(unit, bufstate);
	 break;

      case DEV_ANSI:
	 status = v2_open_ansi_input(unit, bufstate);
	 break;

      default:
	 status = NOT_IMPLEMENTED;
   }
   if (status != SUCCESS)
      return status;

   if (CURRENT_PP_VALUE(ADDRESS) != NULL) {	/* make it an array file */
      status = v2_map_disk_file(unit);
      return status;
   }

   return SUCCESS;
}
