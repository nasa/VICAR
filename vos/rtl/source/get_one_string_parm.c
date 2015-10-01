#include "xvmaininc.h"

#if RTL_USE_TAE

#include "taeconf.inp"
#include "symtab.inc"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Returns a pointer to a single value from a multivalued string	*/
/* parameter.  The resultant string should not be modified (i.e., copy	*/
/* it first) since it actually points into the parblock.		*/

int v2_get_one_string_parm(char *name, int number, char **value)
{
   struct VARIABLE *v;

   // The cast is ok. The only difference between PARBLK and
   // LARGE_PARBLK is the size of the pool, which isn't used by p_fvar.
   v = p_fvar((struct PARBLK*) &parb, name);
   if (v == NULL)
      return PARAM_NOT_FOUND;
   if (v->v_type != V_STRING)
      return PARAM_NOT_FOUND;
   if (number >= v->v_count || number < 0)
      return ILLEGAL_INSTANCE;

   *value = SVAL(*v, number);

   return SUCCESS;
}

#else

#include "errdefs.h"
int v2_get_one_string_parm()
{
   return NO_TAE_SUPPORT;
}

#endif

