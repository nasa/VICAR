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

#if RTL_USE_TAE

/* Gets parameters for the C-callable routines zvparm and zvp.		*/
/* See the comments for zvparm for more explanation of the parameters.	*/

int v2_get_parm_c(
   struct PARBLK *parblock,	/* in: parblock to use			*/
   char *name,			/* in: name of parm to get		*/
   char *value,			/* out: returned value			*/
   int *count,			/* out: # of items in parameter		*/
   int *def,			/* out: 1 if defaulted, 0 if entered	*/
   int maxcnt,			/* in: max # of items to return		*/
   int length,			/* in: length of each string in array	*/
   int double_flag		/* in: TRUE for double, FALSE for float	*/
)

{
   int i;
   struct VARIABLE *v;		/* ptr to VARIABLE struct for this parm	*/
   int *int_ptr;
   float *float_ptr;
   double *double_ptr;

   v = p_fvar(parblock, name);	/* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   if (maxcnt > 0)
      *count = MIN(v->v_count, maxcnt);
   else
      *count = v->v_count;

   *def = v->v_default;

   if (*count <= 0)
      return SUCCESS;			/* null parameter - no need to store */

   switch (v->v_type) {			/* store value according to type */

      case V_INTEGER:
         int_ptr = (int *)value;
         for (i = 0; i < *count; i++)
            *int_ptr++ = IVAL(*v, i);
         break;

      case V_REAL:
         if (double_flag) {		/* double precision */
            double_ptr = (double *)value;
            for (i = 0; i < *count; i++)
               *double_ptr++ = RVAL(*v, i);
         }
         else {				/* single precision */
            float_ptr = (float *)value;
            for (i = 0; i < *count; i++)
               *float_ptr++ = RVAL(*v, i);
         }
         break;

      case V_STRING:
         if (*count == 1) {	/* single value, do it the easy way */
            if (length == 0)
               strcpy(value, SVAL(*v,0));	/* no max length */
            else {
               strncpy(value, SVAL(*v,0), length);
               *(value+length-1) = '\0';	/* make sure of a terminator */
            }
         }
         else {			/* multiple values */
            if (length == 0)	/* No length, so pack for XVSPTR (grrr) */
               v2_pack_xvsptr(value, v, *count);
            else {			/* Normal C 2D array of chars */
               for (i = 0; i < *count; i++) {
                  strncpy(value + i*length, SVAL(*v,i), length);
                  *(value + i*length + length-1) = '\0'; /* ensure terminator */
               }
            }
         }

         break;

      default:
         *count = 0;
         v2_error_handler(NO_UNIT, PARBLK_ERROR);
         return PARBLK_ERROR;
   }

   return SUCCESS;
}

#else

int v2_get_parm_c(struct PARBLK * UNUSED(parblock), char *UNUSED(name), 
	       char *UNUSED(value), int *count, 
	       int *UNUSED(def), int UNUSED(maxcnt), 
	       int UNUSED(length), int UNUSED(double_flag))
{
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

