#include <ctype.h>
#include <string.h>
#include "taeintproto.h"
/* Copies a string from "in" to "out", converting it to upper case on the way */

void make_upper_case(char *out,char *in)
{
   size_t i,l;

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
