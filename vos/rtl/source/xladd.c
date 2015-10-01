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

int zladd(int unit, char *c_type, char *c_key, void *value, ...)	/*$$*/
{
   va_list params;
   int nargs;
   int status;

   char type[MAX_SHORT_STRING_SIZE+1];
   char key[MAX_LABEL_KEY_SIZE+1];

   va_start(params, value);		/*$$*/

/* Get the constant parameters */	/*$$*/

   v2_make_upper_case_max(type, c_type, MAX_SHORT_STRING_SIZE);
   v2_make_upper_case_max(key, c_key, MAX_LABEL_KEY_SIZE);

#if NARGS_AVAIL_OS
   va_count(nargs);
   nargs -= 4; /*$$*/	/* Adjust by the number of constant parameters */
#else
   nargs = -1;
#endif

/* Do the common preprocessing */

   status = p_xladd(unit);		/*$$*/
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

   status = c_xladd(unit, type, key, value, LABEL_I_VALUE(ULEN));	/*$$*/
   va_end(params);

   if (status != SUCCESS)
      v2_error_handler(unit, status);

   return status;

}

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xladd, XLADD) (int *unitp, char *for_type, char *for_key,
				char *value, int *status, ...)	/*$$*/
{
   va_list params, str_params, save_str_params;
   int nargs;			/* total number of arguments */
   int nopts;			/* number of opt args (nargs - constant args) */
   int argno = 5; /*$$*/	/* argument number of last const arg */
   int strno = 2; /*$$*/	/* 3 if string val) string # of last const arg*/
   int which = 0;
   int unit = *unitp;

/* Constant parameters */		/*$$*/

   char type[MAX_SHORT_STRING_SIZE+1];
   char key[MAX_LABEL_KEY_SIZE+1];

   int len, nelements;

   va_start(params, status);		/*$$*/

   if (v2_valid_unit(unit) != SUCCESS) {
      va_end(params);
      *status = NO_SUCH_UNIT;
      v2_error_handler(unit, NO_SUCH_UNIT);
      return;
   }

/* Figure out the number of arguments */

/* Preset the FORMAT optional so we know the data type of "value".  While */
/* we're at it, preset ERR_ACT and ERR_MESS params.			  */
/* Also, fill in nopts if we don't know yet...takes place of get_nopts(). */

   current_access = A;			/* Set up the current access */
   current_call = LADD;

   *status = v2_format_preset_for(unit, &nopts, &nargs, argno, &params);
   if (EQUAL(LABEL_S_VALUE(LFORMAT),"STRING"))
      strno++;			/* format is string, so there's one more */
   if (*status != SUCCESS) {
      v2_error_handler(unit, *status);
      return;
   }

   /* Get constant strings (params points correctly) */

   va_copy(str_params, params);
   va_copy(save_str_params, params);
   va_start(params, status);		/*$$*/ /* restart for optionals */

   v2_sfor2c(type, MAX_SHORT_STRING_SIZE, for_type, &unitp, nargs, 2, 1,
			&str_params, &which);
   v2_make_upper_case(type, type);
   v2_sfor2c(key, MAX_LABEL_KEY_SIZE, for_key, &unitp, nargs, 3, 2,
			&str_params, &which);
   v2_make_upper_case(key, key);

/* Do the common preprocessing */

   *status = p_xladd(unit);		/*$$*/
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

/* Unpack the Fortran string array */

   len = LABEL_I_VALUE(ULEN);

   if (EQUAL(LABEL_S_VALUE(LFORMAT),"STRING")) {
      nelements = LABEL_I_VALUE(NELEMENTS);
      if (nelements == 0)
         nelements = 1;
       if (nelements == -1) {	/* what does "all elements" mean for add? */
         *status = IMPROPER_ELEMENT_NUMBER;
         v2_error_handler(unit, *status);
         return;
      }

      which = 0;	/* reset to start of str len list (save_str_params) */
      *status = v2_sfor2c_array((char **)&value, &len, nelements, value, &unitp,
				nargs, 4, 3, &save_str_params, &which);
      if (*status != SUCCESS) {
         v2_error_handler(unit, *status);
         return;
      }
   }

/* Call the common routine */

   *status = c_xladd(unit, type, key, value, len);	/*$$*/
   va_end(params);

   if (EQUAL(LABEL_S_VALUE(LFORMAT),"STRING")) {
      free(value);
   }

   if (*status != SUCCESS)
      v2_error_handler(unit, *status);

   return;

}
