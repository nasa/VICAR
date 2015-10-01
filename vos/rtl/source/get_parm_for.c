#include "xvmaininc.h"
#if RTL_USE_TAE
#include "taeconf.inp"
#include "symtab.inc"
#include "parblk.inc"
#endif
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"

#if RTL_USE_TAE

/* Gets parameters for the Fortran-callable routines xvparm and xvp.	*/
/* See the comments for xvparm for more explanation of the parameters.	*/

void v2_get_parm_for(
   struct PARBLK *parblock,	/* in: parblock to use			*/
   char *name,			/* in: name of parm to get		*/
   int *count,			/* out: # of items in parameter		*/
   int *def,			/* out: 1 if defaulted, 0 if entered	*/
   int maxcnt,			/* in: max # of items to return		*/
   char *value,			/* out: returned value			*/
   char **argptr,		/* in: argument pointer for sc2for	*/
   int nargs,			/* in: # of args for sc2for		*/
   int argno,			/* in: argument # for value for sc2for	*/
   int strno,			/* in: string # for value for sc2for	*/
   va_list *params,		/* in/out: ptr to string lengths	*/
   int *which,			/* in/out: "which" value for sfor2c	*/
   int double_flag		/* in: TRUE for double, FALSE for float	*/
)

{
   int i, length;
   struct VARIABLE *v;		/* ptr to VARIABLE struct for this parm	*/
   int *int_ptr;
   float *float_ptr;
   double *double_ptr;
   char *c_value;
   va_list save_params;
   int save_which;

   v = p_fvar(parblock, name);	/* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return;
   }

   if (maxcnt > 0)
      *count = MIN(v->v_count, maxcnt);
   else
      *count = v->v_count;

   *def = v->v_default;

   if (*count <= 0)
      return;			/* null parameter - no need to store */

   switch (v->v_type) {			/* store value according to type */

      case V_INTEGER:
         int_ptr = (int *)value;
         for (i = 0; i < *count; i++)
            *int_ptr++ = IVAL(*v, i);
         break;

      case V_REAL:
         if (double_flag) {		/* Use double precision */
            double_ptr = (double *)value;
            for (i = 0; i < *count; i++)
               *double_ptr++ = RVAL(*v, i);
         }
         else {				/* Use single precision */
            float_ptr = (float *)value;
            for (i = 0; i < *count; i++)
               *float_ptr++ = RVAL(*v, i);
         }
         break;

      case V_STRING:
         va_copy(save_params, *params);
         save_which = *which;
         length = v2_sfor2len(value, argptr, nargs, argno, strno,
						&save_params, &save_which);

         if (*count == 1) {		/* single value, do it the easy way */
            v2_sc2for(SVAL(*v,0), length, value, argptr, nargs, argno, strno,
						params, which);

         } else {
            if (length == 0) { /* Probably VMS pass by ref so pack for XVSPTR */
               v2_pack_xvsptr(v2_sfor2ptr(value), v, *count);	/* grr */
            }
            else {			/* Normal Fortran string array */
               c_value = (char *)malloc(*count * (length+1));
               if (c_value == NULL) {
                  *count = 0;
                  v2_error_handler(NO_UNIT, INSUFFICIENT_MEMORY);
                  return;
               }

               for (i = 0; i < *count; i++) {
                  strncpy(c_value + i*(length+1), SVAL(*v,i), length);
                  *(c_value + i*(length+1) + length) = '\0';   /* ensure term */
               }

               v2_sc2for_array(c_value, length+1, *count, value, &length,
			 argptr, nargs, argno, strno, params, which);
               free(c_value);
            }
         }

         break;

      default:
         *count = 0;
         v2_error_handler(NO_UNIT, PARBLK_ERROR);
         return;
   }

   return;
}

#else

void v2_get_parm_for(struct PARBLK * UNUSED(parblock),
		  char *UNUSED(name),int *count, int *UNUSED(def), 
		  int UNUSED(maxcnt),
		  char *UNUSED(value), char **UNUSED(argptr), 
		  int UNUSED(nargs), int UNUSED(argno),
		  int UNUSED(strno), va_list *UNUSED(params), 
		  int *UNUSED(which), int UNUSED(double_flag))
{
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return;
}

#endif

