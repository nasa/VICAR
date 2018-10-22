#include "vicmain_c"
#include "ftnbridge.h"

/*  Program to test the GETHOST subroutine.  */

main44()
{
   char hostname[41], msg[81];
   int  length, status;

   length = 40;
   zvmessage("Testing GETHOST subroutine C interface", "");

   status = zgethost(hostname, length);
   if (status) {
      sprintf(msg, "   Full Host Name = %s", hostname);
      zvmessage(msg, "");
   }
   else
      zvmessage("   GETHOST cannot determine host name.", "");

   length = 4;
   status = zgethost(hostname, length);
   if (status) {
      sprintf(msg, "   Truncated Host Name = %s", hostname);
      zvmessage(msg, "");
   }
   else
      zvmessage("   GETHOST cannot determine host name.", "");

   FTN_NAME(tgethostf)();
   exit(0);
}
