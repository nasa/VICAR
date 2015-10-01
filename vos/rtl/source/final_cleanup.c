#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Called from xvclose to make the final cleanup of tables associated with */
/* 'unit'.  In particular, the OPEN_ACT, IO_ACT, OPEN_MES, and IO_MESS     */
/* strings are cleaned up here, after being preserved by close_unit.	   */
/* Note:  This routine assumes that close_unit has been called first!	   */

void v2_final_cleanup(int unit)
{

   if (CURRENT_S_VALUE(OPEN_ACT) != default_table[OPEN_ACT].pvalue)
      free(CURRENT_S_VALUE(OPEN_ACT));
   CURRENT_S_VALUE(OPEN_ACT) = default_table[OPEN_ACT].pvalue;

   if (CURRENT_S_VALUE(OPEN_MES) != default_table[OPEN_MES].pvalue)
      free(CURRENT_S_VALUE(OPEN_MES));
   CURRENT_S_VALUE(OPEN_MES) = default_table[OPEN_MES].pvalue;

   if (CURRENT_S_VALUE(IO_ACT) != default_table[IO_ACT].pvalue)
      free(CURRENT_S_VALUE(IO_ACT));
   CURRENT_S_VALUE(IO_ACT) = default_table[IO_ACT].pvalue;

   if (CURRENT_S_VALUE(IO_MESS) != default_table[IO_MESS].pvalue)
      free(CURRENT_S_VALUE(IO_MESS));
   CURRENT_S_VALUE(IO_MESS) = default_table[IO_MESS].pvalue;

   return;
}
