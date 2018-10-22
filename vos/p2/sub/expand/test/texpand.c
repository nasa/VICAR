#include "vicmain_c"
#include "ftnbridge.h"

/* Program to test subroutine expand().                                       */
main44()
{
   unsigned char buf[200], msg[50];
   int nsi=100, inc=2, i, j;

   zvmessage("\nTesting EXPAND subroutine C interface\n", "");
   sprintf(msg, "nsi = %d, inc = %d\n", nsi, inc);
   zvmessage(msg, "");
   for (i = 0; i < nsi; i++)
      buf[i] = i;
   zvmessage("Original buffer 1:\n", "");
   print_buffer(buf, nsi);
   zexpand(buf, nsi, inc);
   zvmessage("Expanded buffer 1:\n", "");
   print_buffer(buf, 2*nsi);

   buf[0] = 0x00;
   buf[1] = 0xff;
   buf[2] = 0x00;
   for (i = 3; i < 8; i++)
      buf[i] = 0xff;
   for (i = 8; i < 13; i++)
      buf[i] = 0x00;
   for (i = 13; i < 50; i++)
      buf[i] = 0xff;
   for (i = 50; i < nsi; i++)
      buf[i] = 0x00;

   zvmessage("Original buffer 2:\n", "");
   print_buffer(buf, nsi);
   zexpand(buf, nsi, inc);
   zvmessage("Expanded buffer 2:\n", "");
   print_buffer(buf, 2*nsi);

   FTN_NAME(texpandf)();
   exit(0);
}

print_buffer(buf, nsi)
unsigned char buf[];
int nsi;
{
   int i, j;
   char msg[81], temp[10];

   for (i = 0; i < nsi; ) {
      msg[0] = 0;
      for (j = 0; j < 18 && i < nsi; j++, i++) {
         sprintf(temp, " %3d", (int) buf[i]);
         strcat(msg, temp);
      }
      zvmessage(msg, "");
   }
   sprintf(msg, "\n");
   zvmessage(msg, "");
}
