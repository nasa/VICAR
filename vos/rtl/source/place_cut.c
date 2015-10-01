#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* Return a pointer to the last label item before p+size (i.e., the	*/
/* first one to go in the EOL labels).					*/

char *v2_place_cut(
   char *p,		/* pointer to the label */
   int size		/* offset to cut before */
)
{
   int len;
   char *last_cut;
   char *sk, *ek, *sv, *ev;
   char *end;

   len = strlen(p);
   if (len < size)
      return p + len;

   last_cut = p;
   end = p + size;

   while ((p <= end) && v2_parse_label(p, end-p+1, &sk, &ek, &sv, &ev)) {
      last_cut = sk;
      p = ev + 1;
   }
   return last_cut;
}
