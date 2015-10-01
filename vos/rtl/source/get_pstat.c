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

/* Gets status info from a given parameter. */

int v2_get_pstat(
   struct PARBLK *parblock,	/* in: parblock to use			*/
   char *key,			/* in: name of parm to get		*/
   int *count,			/* out: # of items in parameter		*/
   int *def,			/* out: 1 if defaulted, 0 if entered	*/
   int *maxlen,			/* out: max length of any item		*/
   char *type			/* out: type of the parameter		*/
)

{
   int i;
   struct VARIABLE *v;		/* ptr to VARIABLE struct for this parm	*/

   v = p_fvar(parblock, key);	/* p_fvar() ignores case */

   if (v == NULL) {
      *count = 0;
      v2_error_handler(NO_UNIT, PARAM_NOT_FOUND);
      return PARAM_NOT_FOUND;
   }

   *count = v->v_count;
   *def = v->v_default;

   switch (v->v_type) {
      case V_REAL:
         *maxlen = sizeof(float);
         strcpy(type, "REAL");
         break;
      case V_INTEGER:
         *maxlen = sizeof(int);
         strcpy(type, "INT");
         break;
      case V_STRING:
         strcpy(type, "STRING");
         *maxlen = 0;
         for (i = 0; i < v->v_count; i++) {
            if (strlen(SVAL(*v,i)) > *maxlen)
               *maxlen = strlen(SVAL(*v,i));
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

int v2_get_pstat(struct PARBLK * UNUSED(parblock), char *UNUSED(key),
	      int *count, 
	      int *UNUSED(def), 
	      int *UNUSED(maxlen), char *UNUSED(type))
{
   *count = 0;
   v2_error_handler(NO_UNIT, NO_TAE_SUPPORT);
   return NO_TAE_SUPPORT;
}

#endif

