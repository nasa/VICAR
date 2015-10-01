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

int zvunit(int *unit, char *c_name, int instance, ...)		/*$$*/
{
   va_list params;
   int nargs;
   int status;

   char name[PARAM_NAME_LENGTH+1];

   va_start(params, instance);		/*$$*/

   v2_make_upper_case_max(name, c_name, PARAM_NAME_LENGTH);

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 3; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xvunit(unit, name, instance);	/*$$*/
   if (status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, status);
      va_end(params);
      return status;
   }

   status = v2_process_optionals_c(*unit, unit_table, N_UNIT_TABLE_ENTRIES,
			     current_table[*unit], default_table,
			     nargs, &params);
   if (status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, status);
      va_end(params);
      return status;
   }

/* Call the common routine */

   status = c_xvunit(*unit, name, instance);	/*$$*/
   va_end(params);

   if (status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, status);
   }

   return status;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xvunit, XVUNIT) (int *unit, char *for_name, int *instancep,
					int *status, ...)	/*$$*/
{
   va_list params, str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 4; /*$$*/	/* argument number of last const arg */
   int strno = 1; /*$$*/	/* string number of last const arg */
   int which = 0;
   int instance = *instancep;

   char name[PARAM_NAME_LENGTH+1];

   va_start(params, status);		/*$$*/

/* Figure out the number of arguments */

   v2_get_nopts(&nopts, &nargs, argno, &params);

/* Get constant strings (params points correctly) */

   va_copy(str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

   v2_sfor2c(name, PARAM_NAME_LENGTH, for_name, &unit, nargs, 2, 1,
			&str_params, &which);
   v2_make_upper_case(name, name);

/* Do the common preprocessing */

   *status = p_xvunit(unit, name, instance);	/*$$*/
   if (*status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, *status);
      va_end(params);
      return;
   }

/* Process the optional arguments */

   *status = v2_process_optionals_for(*unit, unit_table, N_UNIT_TABLE_ENTRIES,
				current_table[*unit], default_table,
				nopts, &params, (char *) &unit,		/*$$*/
				nargs, argno, strno, &str_params, &which);
   if (*status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, *status);
      va_end(params);
      return;
   }

/* Call the common routine */

   *status = c_xvunit(*unit, name, instance);	/*$$*/
   va_end(params);

   if (*status != SUCCESS) {
      v2_deactivate_a_unit(*unit);
      v2_error_handler(NO_UNIT, *status);
   }

   return;

}
