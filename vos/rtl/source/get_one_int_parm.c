#include "xvmaininc.h"

#if RTL_USE_TAE

#include "taeconf.inp"
#include "symtab.inc"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Returns a single value from a multivalued integer parameter	*/
/* If an error occurs, *value is guaranteed to be unchanged	*/

int v2_get_one_int_parm(char *name, int number, int *value)
{
   struct VARIABLE *v;
   /* The cast is ok. The only difference between PARBLK and		*/
   /* LARGE_PARBLK is the size of the pool, which isn't used by p_fvar.	*/
   v = p_fvar((struct PARBLK*) &parb, name);
   if (v == NULL)
      return PARAM_NOT_FOUND;
   if (v->v_type != V_INTEGER)
      return PARAM_NOT_FOUND;
   if (number >= v->v_count || number < 0)
      return ILLEGAL_INSTANCE;

   *value = IVAL(*v, number);

   return SUCCESS;
}

#else

#include "errdefs.h"
int v2_get_one_int_parm()
{
   return NO_TAE_SUPPORT;
}

#endif

