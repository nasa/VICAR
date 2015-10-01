#include "rtlintproto.h"

/****************************************************************/
/* Moves a chunk of "len" bytes from address "from" to "to".	*/
/* Overlapping moves are handled correctly, unlike memcpy.	*/
/* The "to" address is not returned (also unlike memcpy), since	*/
/* it's not needed.						*/
/****************************************************************/

void v2_move(char *to, char *from, int len)
{
   int i;

   if (from == to  ||  len < 1) {
      /* do nothing */
   }
   else if (from < to && from+len >= to) {
      for (i=len-1; i>=0; i--) {
         to[i] = from[i];
      }
   }
   else {
      memcpy(to, from, len);
   }

   return;
}

