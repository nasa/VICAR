/*---------------------------  shfv     ------------------------
 * SHFV  (SHiFt Vector)
 *
 * Performs an arithmetic shift on each element of an array
 *
 * dcode   - Data type code. (See below.)
 * n       - number of data elements to transfer
 * shift   - number of bits to shift left(pos.) or right(neg.)
 * buf     - input vector of original (source) data;
 *           On output contains shifted bits.
 * inc     - source vector index increment
 *

 The data type (dcode) of array a may  be one of the following:

        1=BYTE        2=INTEGER*2     4=INTEGER*4

 REVISION HISTORY:                                          
   92-5-18  ..SP....  Made portable for UNIX - Adapted version from ASU.
   92-5-21  ..SP....  Added extra code to handle shifts of +/-32... since
                      these are undefined in ANSI C.  Unfortunately this
                      makes the code dependent on the word size, but I
                      guess that is what the above DCODE types are.
--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"

/*  Values of dcode - format of data in BUF buffer.  */

#define BYTE   1
#define HALF   2
#define FULL   4

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2(shfv, SHFV) (dcode, n, shift, buf, inc)
   int *dcode,*n,*shift,*buf,*inc;
{
   zshfv( *dcode, *n, *shift, buf, *inc);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zshfv(dcode, n, shift, buf, inc)
   int dcode,n,shift,*buf, inc;
{

  switch (dcode) {
    case BYTE:	byte_shift(n,shift,inc, (unsigned char *)buf);
		break;
    case HALF:	half_shift(n,shift,inc, (short *)buf);
		break;                  
    case FULL:	full_shift(n,shift,inc, (int *)buf);
		break;
    default:    zvmessage("*** SHFV - Illegal DCODE","");
                zabend();
                break;
  }
  return;
}
/*  ##################################################################  */

byte_shift(n,shift,inc,bbuf)
     int n, shift, inc;
     unsigned char *bbuf;
{
  register int i;

/*  =================================  */
  if (shift < -7){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = 0;
  }
  else if (shift < 0){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = *bbuf >> (-shift);
  }
  else if (shift > 7){
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,bbuf += inc)
        *bbuf = *bbuf << (shift);
  }
}       /*  0 shift means zero work.  */


/*  #################################################################  */

half_shift(n,shift,inc,hbuf)
     int n, shift, inc;
     short int *hbuf;
{
  register int i;
/*  =================================  */

  if (shift < -15){
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf >> (15);  /* The sign bit gets dragged all the way thru*/
  }
  else if (shift < 0){
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf >> (-shift);
  }
  else if (shift > 15) {
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,hbuf += inc)
        *hbuf = *hbuf << (shift);
  }
}
/*  ##################################################################  */

full_shift(n,shift,inc,fbuf)
     int n, shift, inc;
     int *fbuf;
{
  register int i;
/*  =================================  */

  if (shift < -31){
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf >> (31);
  }
  else if (shift < 0){
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf >> (-shift);
  }
  else if (shift > 31) {
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = 0;
  }
  else if (shift > 0) {
     for (i=0;i<n;i++,fbuf += inc)
        *fbuf = *fbuf << (shift);
  }
}

  
