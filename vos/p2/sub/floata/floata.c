/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                      FLOATA

	General routine for converting a byte or integer array
        to REAL*4. Note the complementary routine CONVRT.
        Byte, halfword or longword data can be converted.

	Fortran format of call:

	CALL FLOATA(DCODE,N,IBUF,RBUF)

	Parameters:

	DCODE ... Input data type.
                  1 = Byte data.
	          2 = Integer*2
                  4 = Integer*4
	N     ... Number of elements to be converted
	IBUF  ... Input array containing data.
	RBUF  ... REAL*4 array for result.
*/    
#include "xvmaininc.h"
#include "applic.h"
#include <zvproto.h>
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zfloata(dcode, n, ibuf, rbuf)
   int dcode, n;
   void *ibuf;
   float *rbuf;
{
  static int first = 1;
  static int dcodesav;
  static int tbuf[12];
         int stat;

/*  ==================================================================  */

 if ( first || dcode != dcodesav) {
  first = 0;         /*  call zvtrans_set first time or if dcode changes  */
  dcodesav = dcode;

  switch (dcode) {
  case 1:
    stat = zvtrans_set(tbuf, "BYTE", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  case 2:
    stat = zvtrans_set(tbuf, "HALF", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  case  4:
    stat = zvtrans_set(tbuf, "FULL", "REAL");
    if (stat != SUCCESS)   zmabend("FLOATA -Programming error");
    break;
  default:    
    zvmessage("*** FLOATA - Illegal DCODE","");
    zabend();
    break;
  }
 }
 zvtrans(tbuf, ibuf, rbuf, n);   /*  convert from int to float  */
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(floata, FLOATA) (dcode, n, ibuf, rbuf)
     int *dcode, *n;
     void *ibuf;
     float *rbuf;
{
   zfloata( *dcode, *n, ibuf, rbuf);
}

