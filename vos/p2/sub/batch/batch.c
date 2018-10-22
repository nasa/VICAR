#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
#include <zvproto.h>
int zbatch(void);

/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/

int FTN_NAME2(batch, BATCH) (void)
{
  return zbatch();
}

/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/

int zbatch(void)
{
   char runtype[32];
   int  count, batch;

   memset (runtype, 0, 32);

   zvp("$RUNTYPE", runtype, &count);

   if (strcmp(runtype,"INTERACTIVE") == 0)
      batch = 0;
   else if (strcmp(runtype,"BATCH") == 0)
      batch = 1;
   else if (strcmp(runtype,"ASYNC") == 0)
      batch = 2;
   else
      batch = -1;

   if (count != 1)
      batch = -1;

   return batch;
}
