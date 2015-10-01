#include "xvmaininc.h"
#include "rtlintproto.h"

#if RTL_USE_TAE

#include "taeconf.inp"
#include "parblk.inc"

/* Looks for a keyword value in the given vblock.  Returns	*/
/* TRUE if the given keyword was found.				*/

int v2_test_keyword (
   struct PARBLK *parblock,	/* in: parameter block to be searched	*/
   char *value			/* in: keyword value to be searched for */
)

{
   struct VARIABLE *v;		/* current place in table	*/
   int i;			/* loop variable		*/

   for (v = parblock->symtab.link; v != NULL; v = v->v_link) {
      if ((v->v_type == V_STRING) && v->v_keyword) {
	 for (i = 0; i < v->v_count; i++) {
	    if (s_equal(value, SVAL(*v, i)))
	       return TRUE;
	 }
      }
   }
   return FALSE;  /* Not found */
}

#else

int v2_test_keyword(struct PARBLK * UNUSED(parblock), char *UNUSED(value))
{
   return 0;		/* FALSE */
}

#endif

