#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* This routine will convert, if necessary, the data in 'value' and	*/
/* add it to the value table associated with 'unit' in the entry	*/
/* indicated by 'kopt'. This routine is called during input file label	*/
/* processing.  Errors are ignored so we get as much of the label as	*/
/* possible.								*/

void v2_add_lbl_item_value_tbl(
   int unit,			/* In: unit number */
   int kopt,			/* In: index into value table */
   char *value			/* In: pointer to the value */
)

{

   if (unit_table[kopt].type == INTEGER)
      CURRENT_I_VALUE(kopt) = atoi(value);

   else				/* Better be type==STRING!! */
       v2_add_str_current_table(value, kopt, current_table[unit],
						default_table);
}
