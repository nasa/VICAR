#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

/*$$*/  /* That symbol marks things in the template which	*/
	/* change for different routines			*/

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

int zvwrit(int unit, void *buffer, ...)			/*$$*/
{
   va_list params;
   int nargs;
   int status;

   va_start(params, buffer);		/*$$*/

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 2; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xvwrit(unit);		/*$$*/
   if (status != SUCCESS) {
      v2_error_handler(unit, status);
      va_end(params);
      return status;
   }

   status = v2_process_optionals_c(unit, unit_table, N_UNIT_TABLE_ENTRIES,
			     current_table[unit], default_table,
			     nargs, &params);
   if (status != SUCCESS) {
      v2_error_handler(unit, status);
      va_end(params);
      return status;
   }

/* Call the common routine */

   status = c_xvwrit(unit, buffer);	/*$$*/
   va_end(params);

   if (status != SUCCESS)
      v2_error_handler(unit, status);

   return status;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvwrit,XVWRIT) (int *unitp,char *buffer, int *status, ...) /*$$*/
{
   va_list params, str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 3; /*$$*/	/* argument number of last const arg */
   int strno = 0; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   va_start(params, status);

/* Figure out the number of arguments */

   v2_get_nopts(&nopts, &nargs, argno, &params);

/* Get constant strings (params points correctly) */		/*$$*/

   va_copy(str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

/* Do the common preprocessing */

   *status = p_xvwrit(unit);		/*$$*/
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Process the optional arguments */

   *status = v2_process_optionals_for(unit, unit_table, N_UNIT_TABLE_ENTRIES,
				current_table[unit], default_table,
				nopts, &params, (char *) &unitp,		/*$$*/
				nargs, argno, strno, &str_params, &which);
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Call the common routine */

   *status = c_xvwrit(unit, buffer);	/*$$*/
   va_end(params);

   if (*status != SUCCESS)
      v2_error_handler(unit, *status);

   return;

}
