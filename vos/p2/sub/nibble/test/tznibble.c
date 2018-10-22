#include "xvmaininc.h"
#include "ftnbridge.h"

/* "C" test routine to test the nibble subroutine.  This routine builds
 *  an array of nibbles and then calls the znibble subroutine to parse
 *  the array of nibbles and build an array of bytes, each containing
 *  a single nibble. 
 */ 



void FTN_NAME(tznibble)() 

{
  char in[10], ot[10];
  int i, num;

/*  ==================================================================  */

      zvmessage("Test the C interface","");

      for (i=0; i<10; i++)
      {
         in[i] = -2 + i;
         ot[i] = 0;
      }
        num =9; 
	zprnt(4,1,&num,"nbytes_out");
	zprnt(1,5,in,"in in dec");
	znibble(in,ot,9);
	zprnt(1,9,ot,"ot in dec");
}
