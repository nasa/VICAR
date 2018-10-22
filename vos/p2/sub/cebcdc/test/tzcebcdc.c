#include "xvmaininc.h"
#include "ftnbridge.h"

/* "C" test routine to test the cebcdc subroutine.  This routine builds
 *  an array of EBCDIC characters and then calls the zcebcdc subroutine
 *  to parse the array of characters and replace them with ASCII
 *  characters. 
 */ 



void FTN_NAME(tzcebcdc)() 

{
  unsigned char buf[256] = {0xE3,0xC5,0xE2,0xE3,0x7A,0xE2,0xE3,
                            0xD9,0xC9,0xD5,0xC7};
  int nchar;

/*  ==================================================================  */

      zvmessage("Test the C interface","");
        nchar = 11; 
	zprnt(0,nchar,buf,"start =  ");
	zcebcdc(buf,nchar);
	zprnt(0,nchar,buf,"result =  ");
}
