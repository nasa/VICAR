#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzdgelg)(r,a,m,n,eps,ifail)
double *r,
       *a;
int     m,
        n,
        eps,
        ifail;
{
/*  ============================================  */

      zdgelg(r,a,m,n,eps,ifail);
}
