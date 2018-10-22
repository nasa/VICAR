#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztranv - Converts from line,sample to lat,long and 
   the reverse for map projections					*/
/************************************************************************/

void ztranv(ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
            line,sample,lat,lon,rp,re,psi)
int *ind;
int itype, m;
float *line, *sample, *lat, *lon;
double xc,zc,th,th1,th2,lam,f,cas,rp,re,psi;

{
FTN_NAME(tranv)( ind, &itype,&m,&xc,&zc,&th,&th1,&th2,&lam,&f,&cas,
                 line,sample,lat,lon,&rp,&re,&psi);
}
