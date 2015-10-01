#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/*                                                                      */
/*                          UNIX VERSION                                */
/*                                                                      */
/* This routine constructs the default file name suffix from the last	*/
/* two hex chars of the process id, which comes from a environmental    */
/* variable defined outside of VICAR.					*/

void v2_get_def_filename(void)
{

   int id;              /* parent process of VICAR */
   char buffer[80];
   char *ptr;

   ptr = getenv("PROCESS_ID_LOGICAL");

   if (ptr != NULL) {
      strcpy(buffer, ptr);
      id = atoi(buffer);
      sprintf(buffer, "%2X", id % 0x100);
      strcat(default_file_name, buffer);
   }

   return;
}

/* Written by Mark Mann ASU 10/3/89 */
