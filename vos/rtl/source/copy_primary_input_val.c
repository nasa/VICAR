#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* Copy various values from the primary input's value table to	*/
/* the file's value table.					*/

void v2_copy_primary_input_val(int unit)
{
   int i, count;
   static int to_item[] =   { U_NL, U_NS, U_NB, U_N1, U_N2, U_N3, U_N4, U_DIM };
   static int from_item[] = {   NL,   NS,   NB,   N1,   N2,   N3,   N4,   DIM };

   for (i=0; i < N_UNIT_TABLE_ENTRIES; i++) {
      if (i == NAME)     continue;	/* Copy pri. inp values to curr table */
      if (i == EOL)      continue;
      if (i == OPEN_ACT) continue;
      if (i == IO_ACT)   continue;
      if (i == NLB)      continue;
      if (i == NBB)      continue;
      if (i == U_NBB)    continue;
      if (i == U_NLB)    continue;
      if (i == ERR_ACT)  continue;
      if (i == ADDRESS)  continue;
      if (i == METHOD)   continue;
      if (i == LBLSIZE)  continue;
      if (i == BUFSTATE) continue;
      if (i == HOST)     continue;
      if (i == INTFMT)   continue;
      if (i == REALFMT)  continue;
      if (i == CONVERT)  continue;
      if ((unit_table[i].mode != LOCAL) &&
           (CURRENT_VALUE(i) == default_table[i].value))
         v2_copy_value_block_item(primary_input_unit, unit, i);
   }

   /* The special items NL,... are used to determine the size	*/
   /* of the output file by setting the U_NL,... form to the	*/
   /* NL,... form.  This is simply saying that the size of the	*/
   /* output file will be that of the input file if no later	*/
   /* overides occur. The U_... form in the value table holds	*/
   /* the desired sizes for 'unit'.				*/

   count = sizeof(from_item) / sizeof(int);

   for (i=0; i<count; i++)
      if (CURRENT_VALUE(to_item[i]) == default_table[to_item[i]].value)
         CURRENT_VALUE(to_item[i]) = CURRENT_VALUE(from_item[i]);

   return;
}
