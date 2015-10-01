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

int zlninfo(int unit, char *key, char *format, int *maxlength,
			int *nelement, ...)			/*$$*/
{
   va_list params;
   int nargs;
   int status;

   va_start(params, nelement);

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 5; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xlninfo(unit);		/*$$*/
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

   status = c_xlninfo(unit, key, format, maxlength, nelement);	/*$$*/
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

void FTN_NAME2(xlninfo, XLNINFO) (int *unitp, char *for_key, char *for_format,
		int *maxlength, int *nelement, int *status, ...)	/*$$*/
{
   va_list params, str_params, save_str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 6; /*$$*/	/* argument number of last const arg */
   int strno = 2; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   char c_key[MAX_LABEL_KEY_SIZE+1];
   char c_format[MAX_SHORT_STRING_SIZE+1];

   va_start(params, status);		/*$$*/

/* Figure out the number of arguments */

   v2_get_nopts(&nopts, &nargs, argno, &params);

/* Get constant strings (params points correctly) */		/*$$*/

   va_copy(str_params, params);
   va_copy(save_str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

/* Do the common preprocessing */

   *status = p_xlninfo(unit);		/*$$*/
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

   *status = c_xlninfo(unit, c_key, c_format, maxlength, nelement);	/*$$*/
   va_end(params);
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      return;
   }

   which = 0;			/* restart str lens */
   v2_sc2for(c_key, 0, for_key, &unitp, nargs, 2, 1, &save_str_params, &which);
   v2_sc2for(c_format, 0, for_format, &unitp, nargs, 3, 2,
						&save_str_params, &which);

   *status = SUCCESS;
   return;

}
