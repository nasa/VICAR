/* tzbswap is the C program that test the bswap subroutine.  */

#include "vicmain_c"
#include "ftnbridge.h"

main44()
{

  unsigned char in[10];
  int i;
  char msg[132];

  for (i=0; i<10; i++)
    {
      in[i] = i - 2;
    }

  zvmessage("nbyte_pairs 5","");
  sprintf(msg,"input in decimal %u %u %u %u %u %u %u %u %u %u",in[0],in[1],in[2],in[3],in[4],in[5],in[6],in[7],in[8],in[9]);
  zvmessage(msg,"");
  zbswap(in,5);
  sprintf(msg,"output in decimal %u %u %u %u %u %u %u %u %u %u",in[0],in[1],in[2],in[3],in[4],in[5],in[6],in[7],in[8],in[9]);
  zvmessage(msg,"");

  zvmessage("***********Testing the FORTRAN interface....******","");

  FTN_NAME(tbswap)();

}
  
