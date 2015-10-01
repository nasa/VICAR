#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"

/* This routine takes a pointer to a string, 'a', and puts the string	*/
/* in the value table pointed at by 'currval_table', at the position	*/
/* indicated by 'kopt'.  Memory must be allocated to the string.  The	*/
/* string and must be a C string already (no Fortran strings here).	*/
/* This routine is identical to add_str_current_table, except that	*/
/* this routine does not convert to upper case.				*/

int v2_add_msg_current_table(
   char *a,			/* string to put in the table */
   int kopt,			/* index into currval_table to use */
   VALUE_TABLE currval_table[],	/* value table vector for this unit only */
   VALUE_TABLE def_table[]	/* default value table */
)

{
   if (currval_table[kopt].pvalue != def_table[kopt].pvalue)
      free(currval_table[kopt].pvalue);

   /* If the string is empty, and the default is null_str, just set the	*/
   /* table to the default to avoid allocating memory.  Can't just go	*/
   /* by the default string being empty, and can't compare the strings	*/
   /* for equality, as that would mess up the xveaction defaults.	*/

   if ((strlen(a) == 0) && (def_table[kopt].pvalue == &null_str))
      currval_table[kopt].pvalue = def_table[kopt].pvalue;

   else {

      /* Allocate memory for the string, and copy it in */

      currval_table[kopt].pvalue = malloc(strlen(a)+1);

      if (currval_table[kopt].pvalue == NULL) {
         currval_table[kopt].pvalue = def_table[kopt].pvalue;
         return FAILURE;
      }
      strcpy(currval_table[kopt].pvalue, a);
   }

   return SUCCESS;
}
