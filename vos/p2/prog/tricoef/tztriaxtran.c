#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* Unit test C-bridge for TTRIAXTRAN.F                                         */
/************************************************************************/

void FTN_NAME(tztriaxtran)(a,b,c,cc,cp,ac,ap,nlimit,klimit,mlimit,inlat,
     inlon,infmt,outlat,outlon,outfmt,status)
double *a;
double *b;
double *c;
double *cc;
double *cp;
double *ac;
double *ap;
int *nlimit;
int *klimit;
int *mlimit;
double *inlat;
double *inlon;
int *infmt;
double *outlat;
double *outlon;
int *outfmt;
int *status;
{
     ztriaxtran(*a,*b,*c,cc,cp,ac,ap,*nlimit,*klimit,*mlimit,*inlat,
     *inlon,*infmt, outlat, outlon, *outfmt, status);
}
