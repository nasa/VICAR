#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of TRIAXTRAN */
/************************************************************************/

void ztriaxtran(a,b,c,cc,cp,ac,ap,nlimit,klimit,mlimit,inlat,
     inlon,infmt,outlat,outlon,outfmt,status)
double a;
double b;
double c;
double *cc;
double *cp;
double *ac;
double *ap;
int nlimit;
int klimit;
int mlimit;
double inlat;
double inlon;
int infmt;
double *outlat;
double *outlon;
int outfmt;
int *status;

{
FTN_NAME(triaxtran)(&a,&b,&c,cc,cp,ac,ap,&nlimit,&klimit,&mlimit,&inlat,&inlon,
                    &infmt,outlat,outlon,&outfmt,status);
}

