#include "xvmaininc.h"

#if RTL_USE_TAE

#include "taeconf.inp"
#include "symtab.inc"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Packs up a group of strings from a parameter into the format that	*/
/* XVSPTR wants.  This format should NOT be used by any new code; it is	*/
/* intended only for compatibility with old stuff.  There is *no*	*/
/* bounds checking on the output string.				*/

void v2_pack_xvsptr(char* out, struct VARIABLE *v, int count)
{
   int i;

   for (i=0; i<count; i++) {
      strcpy(out, SVAL(*v,i));
      out += strlen(SVAL(*v,i)) + 1;	/* +1 for null terminator */
   }
   return;
}

#else

void v2_pack_xvsptr(char * UNUSED(out), struct VARIABLE * UNUSED(v),
				int UNUSED(count))
{
   return;
}

#endif

