#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"

/* Will return a pointer to a string equal to 's' with a null	*/
/* at 'len' chars from the beginning.  The returned string is	*/
/* statically allocated, so it better be either copied or not	*/
/* needed any more before string() is called again.		*/

char *v2_string(char *s, int len)
{
   static char a[MAX_STRING_SIZE+1];

   strncpy(a,s,len);
   a[len] = '\0';
   return a;
}
