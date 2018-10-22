/*  This is a test program for the FORTRAN Callable portion of the
    gtprcs subroutine.                                               */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     char b[8], msg[8];

     zvmessage(" "," ");
     zvmessage("Test the C Interface"," ");
     zvmessage(" "," ");

     zgtprcs(b);
     sprintf(msg,"%s",b);
     zvmessage(msg," ");
     zvmessage(" "," ");

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN Interface"," ");
     zvmessage(" "," ");

     FTN_NAME(tgtprcs)();

}
