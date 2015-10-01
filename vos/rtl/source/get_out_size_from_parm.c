#include "xvmaininc.h"
#include "defines.h"
#include "zvproto.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Get the size of an output file from SIZE, NL, NS, and NB parameters. */
/* If all four items of SIZE are present, then the nl and ns from SIZE	*/
/* will override the nl and ns from the NL and NS params (assuming	*/
/* they are non-0 of course).  If less than 4 SIZE items are present,	*/
/* SIZE is ignored.							*/

void v2_get_out_size_from_parm(int unit) 
{
   int i, status, x, count;
   int size[4];
   static int from_item[] = {   NL,   NS,   NB,   N1,   N2,   N3,   N4,   DIM };
   static int to_item[]   = { U_NL, U_NS, U_NB, U_N1, U_N2, U_N3, U_N4, U_DIM };

   status = v2_get_one_int_parm("NL", 0, &x);		/* Get NL param */
   if (status == SUCCESS && x != 0)
      CURRENT_I_VALUE(NL) = x;

   status = v2_get_one_int_parm("NS", 0, &x);		/* Get NS param */
   if (status == SUCCESS && x != 0)
      CURRENT_I_VALUE(NS) = x;

   status = v2_get_one_int_parm("NB", 0, &x);		/* Get NB param */
   if (status == SUCCESS && x != 0)
      CURRENT_I_VALUE(NB) = x;

   for (i=0; i<4; i++) {		/* Get the SIZE parameter */
      status = v2_get_one_int_parm("SIZE", i, &size[i]);
      if (status != SUCCESS)
         break;			/* item not present */
   }

   if (status == SUCCESS) {		/* all four SIZE values were present */
      if (size[2] != 0)
         CURRENT_I_VALUE(NL) = size[2];
      if (size[3] != 0)
         CURRENT_I_VALUE(NS) = size[3];
   }

   /* Copy NL, NS to their U forms */

   count = sizeof(from_item) / sizeof(int);

   for (i=0; i < count; i++) 
      if (CURRENT_I_VALUE(to_item[i]) == default_table[to_item[i]].ivalue)
         CURRENT_I_VALUE(to_item[i]) = CURRENT_I_VALUE(from_item[i]);

   return;
}
