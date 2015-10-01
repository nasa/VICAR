#include "xvmaininc.h"
#include "rtlintproto.h"

/* Returns TRUE if 'sub' is a substring of 'string'.	*/

#define TRUE	1
#define FALSE	0

int v2_substr(
   char *string,		/* long string */
   char *sub			/* substring */
)

{
   char *p;

   if (strlen(sub) > strlen(string))
      return FALSE;

   if (strlen(sub)==0)
      return FALSE;

   p = string;

   while((p=strchr(p,*sub))) {
      if (strncmp(p,sub,strlen(sub))==0)
         return TRUE;
      else
         p++;
   }

   return FALSE;
}
