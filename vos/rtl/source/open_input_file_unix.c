#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

int v2_open_input_file(int unit)
{
   struct bufstate *bufstate;
   int status;
#if RTL_USE_TAPE
   int h1, h2;
#endif

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

#if RTL_USE_TAPE
/* Determine whether unit is a tape by searching the TAE-supplied table	*/
/* 'i_tape'; 'h1' and 'h2' return table index and desired tape file	*/

   status = v2_i_analyze(CURRENT_S_VALUE(NAME), i_tape,i_count, &h1,&h2);

   if ((status==I_TAPE) && (h1>=0)) {		/* The unit is a tape. */
      status = v2_open_tape(unit, bufstate, h1, h2);
      v2_set_file_offset(unit, (V2_OFFSET)0);
      return status;
   }
#endif

#if 0
   mem = v2_open_memory_file(unit, bufstate);
   if (mem) {
      v2_set_file_offset(unit, (V2_OFFSET)0);
      return SUCCESS;
   }

   status = v2_determine_device(unit, &bufstate->devstate.device_type);
#endif

   bufstate->devstate.device_type = DEV_DISK;
   v2_set_file_offset(unit, (V2_OFFSET)0);

   switch (bufstate->devstate.device_type) {
      case DEV_DISK:
         status = v2_open_disk_input(unit, bufstate);
         break;

#if 0
      case DEV_ANSI:
         status = v2_open_ansi_input(unit, bufstate);
         break;
#endif

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
