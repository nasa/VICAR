/*  This is a program that test the C Callable Subroutine for */
/*  twopow.                                                   */


#include "vicmain_c"
#include "ftnbridge.h"

main44()
{
     short int n, pow, result;
     char msg1[40], msg2[40];

     zvmessage("Test the C interface"," ");
     zvmessage(" "," ");

     
     for (n = -16; n < 10; )
     {
          ztwopow(&result,n,&pow);

          if(result == 1)
          {
               sprintf(msg1, "number = %d", n);
               sprintf(msg2, "  power of 2 = %d", pow);

               zvmessage(msg1," ");
               zvmessage(msg2," ");
          }

          else
          {
               sprintf(msg1, "number = %d", n);
               sprintf(msg2, "  next power of 2 = %d", pow);

               zvmessage(msg1," ");
               zvmessage(msg2," ");
          }

     n=n+5;
     }

     zvmessage(" "," ");
     zvmessage("Test the FORTRAN interface"," ");
     zvmessage(" "," ");

     FTN_NAME(ttwopow)();
}
