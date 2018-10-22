/*---------------------------  sumv     ------------------------
 * SUMV  (SUM Vector)
 *
   REVISION HISTORY
      6-87   SP  CORRECTED PROBLEM WITH DCODE 6 FOR NEGATIVE DNS BY CHANGING
                 MOVZWL TO CVTWL
     11-92   SP  Made portable for UNIX, changed to use C - Adapted from ADDV

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	VICAR SUBROUTINE                                            sumv

	General routine for summing elements in array A and returning sum 
	in B (B is a scalar). B = SUM A(i). A and B can be of different
       data types as indicated.

	Fortran format of call:

	CALL SUMV(DCODE, N, A, B, INCA)

	Parameters:-

	DCODE......Data types
	           =1,   A is byte         B is byte
	           =2,   A is halfword     B is halfword
	           =3,   A is byte         B is halfword
                   =4,   A is fullword     B is fullword
	           =5,   A is byte         B is fullword
                   =6,   A is halfword     B is fullword
	           =7,   A is real(single) B is real
	           =8,   A is double       B is double
                   =9,   A is real         B is double
                  negative values -1 to -9 reverse of above
	N..........Number of elements to sum.
	A..........Source vector
        B..........Sum (B is an output scalar variable)
        INCA     - Source vector index increment
--------------------------------------------------------------*/
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdint.h>

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(sumv, SUMV) (dcode, n, avec, b, inca)
     int *dcode, *n, *inca;
     void *avec, *b;
{
   zsumv( *dcode, *n, avec, b, *inca);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

zsumv(dcode, n, avec, b, inca)
   int dcode, n, inca;
   void *avec, *b;
{
  int i;
  
  /* vector in, scalar out */
  uint8_t *bytein,   byteout;
  int16_t     *halfin,   halfout;
  int32_t          *fullin,   fullout;
  float         *realin,   realout;
  double        *doublein, doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = 0;
    for (i=0; i < n; i++, bytein+=inca) {
      byteout += *bytein;
    }
    *(uint8_t *)b = byteout; 
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = 0;
    for (i = 0; i <n;  i++, halfin+=inca){
      halfout += *halfin;
    }
    *(int16_t *)b = halfout;
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = 0;
    for (i = 0; i<n; i++,halfin+=inca){
      byteout += *halfin;
    }
    *(uint8_t *)b = byteout;
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = 0;
    for (i = 0; i< n; i++, bytein+=inca){
      halfout += *bytein;
    }
    *(int16_t *)b = halfout;
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = 0;
    for (i = 0; i<n; i++,fullin+=inca){
      fullout += *fullin;
    }
    *(int32_t *)b = fullout;
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = 0;
    for (i = 0; i< n; i++,fullin+=inca){
      byteout += *fullin;
    }
    *(uint8_t *)b = byteout;
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = 0;
    for (i = 0; i< n; i++, bytein+=inca){
      fullout += *bytein;
    }
    *(int32_t *)b = fullout;
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = 0;
    for (i = 0;i< n; i++, fullin+=inca){
        halfout += *fullin;
    }
    *(int16_t *)b = halfout;
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = 0;
    for (i = 0; i< n; i++, halfin+=inca){
      fullout += *halfin;
    }
    *(int32_t*)b = fullout;
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = 0.0;
    for (i = 0;i< n; i++, realin+=inca){
      realout += *realin;
    }
    *(float *)b = realout;
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = 0.0;
    for (i = 0; i< n; i++, doublein+=inca){
      doubleout += *doublein;
    }
    *(double *)b = doubleout;
    break;
  case -9:
    doublein = (double *) avec;
    realout = 0.0;
    for (i = 0; i< n; i++, doublein+=inca){
      realout += *doublein;
    }
    *(float *)b = realout;
    break;
  case 9:
    realin = (float *) avec;
    doubleout = 0.0;
    for (i = 0; i< n; i++,realin+=inca){
      doubleout += *realin;
    }
    *(double *)b = doubleout;
    break;
  default:    
    zvmessage("*** SUMV - Illegal DCODE","");
    zabend();
    break;
  }
}
