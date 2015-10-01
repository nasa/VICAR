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

int zlhinfo(int unit, char *tasks, int *instances, int *nhist, ...)	/*$$*/
{
   va_list params;
   int nargs;
   int status;

   va_start(params, nhist);		/*$$*/

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 4; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xlhinfo(unit);		/*$$*/
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

   status = c_xlhinfo(unit, tasks, instances, nhist, LABEL_I_VALUE(ULEN));/*$$*/
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

void FTN_NAME2(xlhinfo, XLHINFO) (int *unitp, char *for_tasks, int *instances,
			int *nhist, int *status, ...)		/*$$*/
{
   va_list params, str_params, save_str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 5; /*$$*/	/* argument number of last const arg */
   int strno = 1; /*$$*/	/* string number of last const arg */
   int which = 0;
   int unit = *unitp;

   int len, in_nhist, dumlen;
   char *c_tasks;

   va_start(params, status);

/* Figure out the number of arguments */

   v2_get_nopts(&nopts, &nargs, argno, &params);

   va_copy(str_params, params);
   va_copy(save_str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

/* Do the common preprocessing */

   *status = p_xlhinfo(unit);		/*$$*/
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

   va_copy(str_params, save_str_params);		/* reset str lens */
   which = 0;
   len = v2_sfor2len(for_tasks, &unitp, nargs, 2, 1, &str_params, &which);
   if (len == 0)
      len = LABEL_I_VALUE(ULEN);
   if (len == 0)		/* If no length, use 8 for old compatibility */
      c_tasks = malloc(8 * *nhist);
   else {
      len++;			/* add one for null terminator */
      c_tasks = malloc(len * *nhist);	/* allocate space for results */
   }
   if (c_tasks == NULL) {
      *status = NO_MEMORY_FOR_LABEL_PROCESS;
      va_end(params);
      v2_error_handler(unit, *status);
      return;
   }

   in_nhist = *nhist;

   *status = c_xlhinfo(unit, c_tasks, instances, nhist, len);	/*$$*/
   if (*status != SUCCESS) {
      free(c_tasks);
      va_end(params);
      v2_error_handler(unit, *status);
      return;
   }

/* Pack into the Fortran array */

   if (len == 0)		/* byte array, for old VMS compatibility */
      v2_move(v2_sfor2ptr(for_tasks), c_tasks, 8 * in_nhist);
   else {
      dumlen = 0;
      va_copy(str_params, save_str_params);		/* reset str lens */
      which = 0;
      v2_sc2for_array(c_tasks, len, in_nhist, for_tasks, &dumlen,
		   &unitp, nargs, 2, 1, &str_params, &which);
   }

   free(c_tasks);
   va_end(params);

   *status = SUCCESS;
   return;

}
