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

int zlpinfo(int unit, char *props, int *nprop, ...)		/*$$*/
{
   va_list params;
   int nargs;
   int status;

   va_start(params, nprop);		/*$$*/

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 3; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xlpinfo(unit);		/*$$*/
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

   status = c_xlpinfo(unit, props, nprop, LABEL_I_VALUE(ULEN));	/*$$*/
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

void FTN_NAME2(xlpinfo, XLPINFO) (int *unitp, char *for_props, int *nprop,
			int *status, ...)			/*$$*/
{
   va_list params, str_params, save_str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 4; /*$$*/	/* argument number of last const arg */
   int strno = 1; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   int len, in_nprop, dumlen;
   char *c_props;

   va_start(params, status);

/* Figure out the number of arguments */

   v2_get_nopts(&nopts, &nargs, argno, &params);

/* Get constant strings (params points correctly */

   va_copy(str_params, params);
   va_copy(save_str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

/* Do the common preprocessing */

   *status = p_xlpinfo(unit);		/*$$*/
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

   which = 0;				/* reset str lens */
   va_copy(str_params, save_str_params);
   len = v2_sfor2len(for_props, &unitp, nargs, 2, 1, &str_params, &which);
   if (len == 0)
      len = LABEL_I_VALUE(ULEN);
   if (len == 0) {		/* If no length, then error */
      *status = IMPROPER_LENGTH;
      v2_error_handler(unit, *status);
      return;
   }
   else {
      len++;			/* add one for null terminator */
      c_props = malloc(len * *nprop);	/* allocate space for results */
   }
   if (c_props == NULL) {
      *status = NO_MEMORY_FOR_LABEL_PROCESS;
      v2_error_handler(unit, *status);
      return;
   }

   in_nprop = *nprop;

   *status = c_xlpinfo(unit, c_props, nprop, len);	/*$$*/
   if (*status != SUCCESS) {
      free(c_props);
      va_end(params);
      v2_error_handler(unit, *status);
      return;
   }

/* Pack into the Fortran array */

   which = 0;				/* reset str lens */
   va_copy(str_params, save_str_params);
   dumlen = 0;
   v2_sc2for_array(c_props, len, in_nprop, for_props, &dumlen,
		&unitp, nargs, 2, 1, &str_params, &which);

   free(c_props);
   va_end(params);

   *status = SUCCESS;
   return;

}
