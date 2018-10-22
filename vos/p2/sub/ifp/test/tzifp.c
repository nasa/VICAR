#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdio.h>
void FTN_NAME(tzifp)()
{
   float         x[2];
   unsigned char bbuf[10][10];
   unsigned char bx;
   short    int  hbuf[5][5];
   short    int  hx;
   short    int  i, j;
   char  buffer [80];
   char *ptr;

   memset (buffer, ' ', sizeof (buffer));
   buffer [sizeof(buffer)-1] = '\0';

   zvmessage ("byte input buffer:", 0);
   for (i = 0; i < 10; i++) {
      for (j = 0; j < 10; j++) {
          bbuf[j][i] = 5 * (i+j+1);      /* construct a ramp */
         (void)sprintf (buffer+5+j*4, "%4d", bbuf [j][i]);
      }
      zvmessage (buffer,0);
   }

   x[0] = 5.1;
   x[1]	= 4.3;

   (void)sprintf (buffer, "At X/Y=     %1.4E %1.4E", x[0], x[1]);
   zvmessage (buffer, 0);

   bx = zifp( x[0], x[1], 10, 10, bbuf, 0, 0);
   (void)sprintf (buffer, "IFP =      %d\n", bx); 
   zvmessage (buffer, 0);

   memset (buffer, ' ', sizeof (buffer));
   buffer [sizeof(buffer)-1] = '\0';

   zvmessage ("halfword input buffer:", 0);
   for (i = 0; i < 5; i++) {
      for (j = 0; j < 5; j++) {
         hbuf[j][i] = 100 * (i+j+1);    /* construct a ramp */
         (void)sprintf (buffer+5+j*6, "%6d", hbuf [j][i]);
      }
      zvmessage (buffer,0);
   }

   x[0] = 2.1;
   x[1] = 3.3;

   (void)sprintf (buffer, "At X/Y=     %1.4E %1.4E", x[0], x[1]);
   zvmessage (buffer, 0);

   hx = zifp ( x[0], x[1], 5, 5, hbuf, 0, 1);
   (void)sprintf (buffer, "IFP =       %d\n", hx);
   zvmessage (buffer, 0);

   return;
}
