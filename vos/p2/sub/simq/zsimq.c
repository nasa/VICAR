#include "xvmaininc.h"
#include "ftnbridge.h"

int zsimq(a, b, n)
  float *a, *b;
  int n;
{
   int ifail = 0;

   FTN_NAME(simq)(a, b, &n, &ifail);
   return ifail;
}

int zdsimq2(a, b, n)
  double *a, *b;
  int n;
{
   int ifail = 0;

   FTN_NAME(dsimq2)(a, b, &n, &ifail);
   return ifail;
}
