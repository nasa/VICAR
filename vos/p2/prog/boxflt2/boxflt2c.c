/*  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
	VICAR SUBROUTINE                                            SUBV

	General routine for subtracting arrays.  Array B is replaced with the
	product of subtracting A from B.  A and B can be of different data 
        types as indicated

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
/* April 19, 2011 - rjb - added prototypes and zvproto.h      */
/* Jun 20, 2011 - rjb - cast varibles to avoid warnings with gcc 4.4.4 */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zvproto.h"        //resolves zvmessage and zabend

/* prototypes */
void FTN_NAME (rsubv)(int *dcode,int *n,void *avec,void *bvec,int *inca,int *incb);
void rzsubv(int dcode,int n,void *avec,void *bvec,int inca,int incb);
void FTN_NAME (zaire)(float out[],float in[],int *nsaddr,int *nswaddr);

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/


void FTN_NAME (rsubv)(dcode, n, avec, bvec, inca, incb)
     int *dcode, *n, *inca, *incb;
     void *avec, *bvec;
{
   rzsubv( *dcode, *n, avec, bvec, *inca, *incb);
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void rzsubv(dcode, n, avec, bvec, inca, incb)
   int dcode, n, inca, incb;
   void *avec, *bvec;
{
  int i;
  
  /* vectors */
  unsigned char *bytein,   *byteout;
  short         *halfin,   *halfout;
  long          *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  
  switch (dcode) {
  case -1:
  case 1:
    bytein = (unsigned char *) avec;
    byteout = (unsigned char *) bvec;
    for (i=0; i < n; i++, bytein+=inca, byteout+=incb) {
      *byteout = (unsigned char)(*byteout - *bytein);
    }
    break;
  case -2:
  case 2:
    halfin = (short *) avec;
    halfout = (short *) bvec;
    for (i = 0; i <n; i++, halfin+=inca, halfout+=incb) {
      *halfout = (short)(*halfout - *halfin);
    }
    break;
  case -3:
    halfin = (short *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i<n ;i++,halfin+=inca, byteout+=incb) {
      *byteout = (unsigned char)(*byteout - *halfin);
    }
    break;
  case 3:
    bytein = (unsigned char *) avec;
    halfout = (short *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,halfout+=incb){
      *halfout = (short)(*halfout - *bytein);
    }
    break;
  case -4:
  case  4:
    fullin = (long *) avec;
    fullout = (long *) bvec;
    for (i = 0; i<n ;i++,fullin+=inca,fullout+=incb){
      *fullout = *fullout - *fullin;
    }
    break;
  case -5:
    fullin = (long *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i< n; i++,fullin+=inca,byteout+=incb){
      *byteout = (unsigned char)(*byteout - *fullin);
    }
    break;
  case 5:
    bytein = (unsigned char *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< n; i++, bytein+=inca,fullout+=incb){
      *fullout = *fullout - *bytein;
    }
    break;
  case -6:
    fullin = (long *) avec;
    halfout = (short *) bvec;
    for (i = 0;i< n; i++, fullin+=inca,halfout+=incb){
        *halfout = (short)(*halfout - *fullin);
    }
    break;
  case 6:
    halfin = (short *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< n; i++, halfin+=inca,fullout+=incb){
      *fullout = *fullout - *halfin;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< n; i++, realin+=inca,realout+=incb){
      *realout = *realout - *realin;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,doubleout+=incb){
      *doubleout = *doubleout - *doublein;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< n; i++, doublein+=inca,realout+=incb){
      *realout = (float)(*realout - *doublein);
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< n; i++,realin+=inca,doubleout+=incb){
      *doubleout = *doubleout - *realin;
    }
    break;
  default:    
    zvmessage("*** SUBV - Illegal DCODE","");
    zabend();
    break;
  }
}


/************************************************************************/
/* Fortran-Callable ZAIRE                                             */
/************************************************************************/

/* Apr-19-2011 - R. J. Bambery - changed from int to float      */
void FTN_NAME (zaire)(out,in,nsaddr,nswaddr)
      float out[],in[];
      int *nsaddr,*nswaddr;

{
      int ns,nsw,outptr,inptr;
      int tmp,i;
      float total;
      ns = *nsaddr;
      nsw = *nswaddr;
      inptr = 0;
      outptr = 0; 
      tmp = inptr;
/*                                         */
      total = 0.0;
      for (i=1;i<=nsw;i++) {
        total += in[inptr++];
      }
/*                                         */
      ns--;
      out[outptr++] = total;
/*                                         */
      for (i=1;i<=ns;i++) {
          total = total - in[tmp++] + in[inptr++];
          out[outptr++] = total;
      }
}
