#include <stdio.h>

/* g: see mve.c */

add_(numargs, dcode, nelem, avec, bvec, ainc, binc)
     int *numargs, *dcode, *nelem, *avec, *bvec, *ainc, *binc;
{
  int i, j;
  
  /* vectors */
  unsigned char *bytein,   *byteout;
  short         *halfin,   *halfout;
  long          *fullin,   *fullout;
  float         *realin,   *realout;
  double        *doublein, *doubleout;
  
  /* increments */
  int in_inc, out_inc;
  
  switch (*numargs) {
  case 4:
    in_inc = 1;
    out_inc = 1;
    break;
  case 5:
    in_inc = *ainc;
    out_inc = 1;
    break;
  case 6:
    in_inc = *ainc;
    out_inc = *binc;
    break;
  default:
    printf("Wrong number of arguments to add\n");
    zabend();
  }
  
  switch (*dcode) {
  case -1:
  case 1:
    bytein = (unsigned char *) avec;
    byteout = (unsigned char *) bvec;
    for (i=0; i < *nelem; i++, bytein+=in_inc, byteout+=out_inc) {
      *byteout = *bytein + *byteout;
    }
    break;
  case -2:
  case 2:
    halfin = (short *) avec;
    halfout = (short *) bvec;
    for (i = 0; i <*nelem; i++, halfin+=in_inc, halfout+=out_inc){
      *halfout = *halfin + *halfout;
    }
    break;
  case -3:
    halfin = (short *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i<*nelem ;i++,halfin+=in_inc, byteout+=out_inc){
      *byteout = *halfin + *byteout ;
    }
    break;
  case 3:
    bytein = (unsigned char *) avec;
    halfout = (short *) bvec;
    for (i = 0; i< *nelem; i++, bytein+=in_inc,halfout+=out_inc){
      *halfout = *bytein + *halfout ;
    }
    break;
  case -4:
  case  4:
    fullin = (long *) avec;
    fullout = (long *) bvec;
    for (i = 0; i<*nelem ;i++,fullin+=in_inc,fullout+=out_inc){
      *fullout = *fullin + *fullout;
    }
    break;
  case -5:
    fullin = (long *) avec;
    byteout = (unsigned char *) bvec;
    for (i = 0; i< *nelem; i++,fullin+=in_inc,byteout+=out_inc){
      *byteout = *fullin + *byteout;
    }
    break;
  case 5:
    bytein = (unsigned char *) avec;
    fullout = (long *) bvec;
    for (i = 0; i< *nelem; i++, bytein+=in_inc,fullout+=out_inc){
      *fullout = *bytein + *fullout;
    }
    break;
  case -6:
    fullin = (long *) avec;
    halfout = (short *) bvec;
    for (i = 0;i< *nelem; i++, fullin+=in_inc,halfout+=out_inc){
        *halfout = *fullin + *halfout;
    }
    break;
  case -7:
  case  7:
    realin = (float *) avec;
    realout = (float *) bvec;
    for (i = 0;i< *nelem; i++, realin+=in_inc,realout+=out_inc){
      *realout = *realin + *realout;
    }
    break;
  case -8:
  case  8:
    doublein = (double *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< *nelem; i++, doublein+=in_inc,doubleout+=out_inc){
      *doubleout = *doublein + *doubleout;
    }
    break;
  case -9:
    doublein = (double *) avec;
    realout = (float *) bvec;
    for (i = 0; i< *nelem; i++, realin+=in_inc,realout+=out_inc){
      *realout = *realin + *realout;
    }
    break;
  case 9:
    realin = (float *) avec;
    doubleout = (double *) bvec;
    for (i = 0; i< *nelem; i++,realin+=in_inc,doubleout+=out_inc){
      *doubleout = *realin + *doubleout;
    }
    break;
  default:
    printf("Invalid DCODE value: %d\n", *dcode);
    zabend();
  }
}
