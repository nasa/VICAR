#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvunit(
   int unit,
   char *name,
   int instance			/* still 1-based */
)

{
int status;
char *filename;

filename = "";

/* Get the filename either through the parameters, or via the U_NAME optional */

if (EQUAL(name,"INP") || EQUAL(name,"OUT")) {	/* Get filename from params */

   status = v2_get_one_string_parm(name, instance-1, &filename);
   if (status == PARAM_NOT_FOUND)
      return BAD_FILE_PARAM_NAME;
   if (status != SUCCESS)
      return status;
}
else {						/* internal file name	*/
   if (strlen(CURRENT_S_VALUE(U_NAME)) == 0)
      /* make_file_name(...) */;
   else
      filename = CURRENT_S_VALUE(U_NAME);
}

/* call routine to add the file name to the current unit table. */

status = v2_add_msg_current_table(filename, NAME,
				      current_table[unit], default_table);
if (status != SUCCESS)
   return UNABLE_TO_STORE_OPTIONAL;

return SUCCESS;

}
