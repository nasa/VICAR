/************************************************************************/
/* C-callable version of MATH77 sort routines.                          */
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"

void zisort(buf,m,n)
int *buf;
int m,n;
{
FTN_NAME2(isort, ISORT) (buf,&m,&n);
}

void zssort(buf,m,n)
float *buf;
int m,n;
{
FTN_NAME2(ssort, SSORT) (buf,&m,&n);
}

void zdsort(buf,m,n)
double *buf;
int m,n;
{
FTN_NAME2(dsort, DSORT) (buf,&m,&n);
}

void zisortp(buf,m,n,ip)
int *buf;
int m,n;
int *ip;
{
FTN_NAME2(isortp, ISORTP) (buf,&m,&n,ip);
}

void zssortp(buf,m,n,ip)
float *buf;
int m,n;
int *ip;
{
FTN_NAME2(ssortp, SSORTP) (buf,&m,&n,ip);
}

void zdsortp(buf,m,n,ip)
double *buf;
int m,n;
int *ip;
{
FTN_NAME2(dsortp, DSORTP) (buf,&m,&n,ip);
}
