#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
   int size, line, inchr, test, dn;
   char *inbuf = {"ABCD2468[\\]^abcd"};
   unsigned char outbuf[72];

   zvmessage("\nTesting TEXT subroutine C interface.\n", "");

   inchr = 4;
   for (test = 0; test < 4; test++) {
      for (size = 1; size <= 3; size++) {
         dn = size * 35;
         for (line = 0; line < 7; line++) {
            ztext(inbuf+(inchr*test), inchr, line, outbuf, size*6, dn);
            print_buffer(outbuf, inchr*size*6);
         }
         zvmessage("", "");
      }
   }

   FTN_NAME(ttextf)();
   exit(0);   
}


print_buffer(buf, nsi)
unsigned char buf[];
int nsi;
{
   int i;
   unsigned char msg[81];

   for (i = 0; i < nsi; i++) {
      if (buf[i] == 0)
         msg[i] = ' ';
      else
         msg[i] = buf[i];
   }
   msg[nsi] = 0;
   zvmessage(msg, "");
}
