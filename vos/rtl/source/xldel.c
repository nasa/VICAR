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

int zldel(int unit, char *type, char *key, ...)			/*$$*/
{
   va_list params;
   int nargs;
   int status;

   char c_type[MAX_SHORT_STRING_SIZE+1];
   char c_key[MAX_LABEL_KEY_SIZE+1];

   va_start(params, key);

/* Get the constant parameters */	/*$$*/

   v2_make_upper_case_max(c_type, type, MAX_SHORT_STRING_SIZE);
   v2_make_upper_case_max(c_key, key, MAX_LABEL_KEY_SIZE);

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 3; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xldel(unit);			/*$$*/
   if (status != SUCCESS) {
      v2_error_handler(unit, status);
      va_end(params);
      return status;
   }

   status = v2_process_optionals_c(unit, label_options, N_LABEL_TABLE_ENTRIES,
			     label_table[unit], label_default_table,
			     nargs, &params);
   if (status != SUCCESS) {
      v2_error_handler(unit, status);
      va_end(params);
      return status;
   }

/* Call the common routine */

   status = c_xldel(unit, c_type, c_key);		/*$$*/
   va_end(params);
   if (status != SUCCESS) {
      v2_error_handler(unit, status);
      return status;
   }

   return SUCCESS;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xldel, XLDEL) (int *unitp, char *for_type, char *for_key,
				int *status, ...)	/*$$*/
{
   va_list params, str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 4; /*$$*/	/* argument number of last const arg */
   int strno = 2; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

/* Constant parameters */		/*$$*/

   char c_type[MAX_SHORT_STRING_SIZE+1];
   char c_key[MAX_LABEL_KEY_SIZE+1];

   va_start(params, status);

/* Figure out the number of arguments if necessary */

   v2_get_nopts(&nopts, &nargs, argno, &params);

/* Get constant strings (params points correctly) */

   va_copy(str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

   v2_sfor2c(c_type, MAX_SHORT_STRING_SIZE, for_type, &unitp, nargs, 2, 1,
				&str_params, &which);
   v2_make_upper_case(c_type, c_type);
   v2_sfor2c(c_key, MAX_LABEL_KEY_SIZE, for_key, &unitp, nargs, 3, 2,
				&str_params, &which);
   v2_make_upper_case(c_key, c_key);

/* Do the common preprocessing */

   *status = p_xldel(unit);		/*$$*/
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Process the optional arguments */

   *status = v2_process_optionals_for(unit, label_options,N_LABEL_TABLE_ENTRIES,
				label_table[unit], label_default_table,
				nopts, &params, (char *) &unitp,		/*$$*/
				nargs, argno, strno, &str_params, &which);
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      va_end(params);
      return;
   }

/* Call the common routine */

   *status = c_xldel(unit, c_type, c_key);		/*$$*/
   va_end(params);
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      return;
   }

   *status = SUCCESS;
   return;

}
