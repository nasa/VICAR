/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            DIVV

	General routine for dividing arrays. B(J) is replaced with 
	B(J)/A(I). A and B can be of different data types as indicated

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
*/    
/*                     ADAPTED FROM ASU VERSION  */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>
#include <stdint.h>


/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zdivv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  uint8_t *bytein,   *byteout;
  int16_t         *halfin,   *halfout;
  int32_t         *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (uint8_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = *byteout / *bytein;
    }
    break;
  case -2:
  case 2:
    halfin = (int16_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb){
      *halfout = *halfout / *halfin;
    }
    break;
  case -3:
    halfin = (int16_t *) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb){
      *byteout = (int)(*byteout) / *halfin;
    }
    break;
  case 3:
    bytein = (uint8_t *) avec;
    halfout = (int16_t *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = *halfout / (int)(*bytein);
    }
    break;
  case -4:
  case  4:
    fullin = (int32_t*) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullout / *fullin;
    }
    break;
  case -5:
    fullin = (int32_t*) avec;
    byteout = (uint8_t *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = (int)(*byteout) / *fullin;
    }
    break;
  case 5:
    bytein = (uint8_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *fullout / (int)(*bytein);
    }
    break;
  case -6:
    fullin = (int32_t*) avec;
    halfout = (int16_t *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = *halfout / *fullin;
    }
    break;
  case 6:
    halfin = (int16_t *) avec;
    fullout = (int32_t*) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *fullout / *halfin;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realout / *realin;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doubleout / *doublein;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = *realout / *doublein;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *doubleout / *realin;
    }
    break;
  default:    
    zvmessage("*** DIVV - Illegal DCODE","");
    zabend();
    break;
  }
}

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME2(divv, DIVV) (dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   zdivv( *dcode, *n, avec, bvec, *inca, *incb);
}

