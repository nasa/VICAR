#include <ctype.h>

/* Copies a string from "in" to "out", converting it to upper case on the way */

void make_upper_case(out,in)
char *in, *out;
{
   int i,l;

   l = strlen(in);
   if (l==0) {
      *out='\0';
      return;
   }

   for (i=0; i<l; i++)
      out[i] = islower(in[i]) ? toupper(in[i]) : in[i];
   out[l] = '\0';

   return;
}
