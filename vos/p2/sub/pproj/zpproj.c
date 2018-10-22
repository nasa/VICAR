#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zpproj - CONVERT LAT,LON TO LINE, SAMP FOR PERSPECTIVE*/
/************************************************************************/

void zpproj(data,line,samp,lat,lon,imode,ilat,radius,srange,ind)
int imode, ilat;
int *ind;
float *lat, *lon, *line, *samp, *radius, *srange;
float data[];
{
FTN_NAME(pproj)(data,line,samp,lat,lon,&imode,&ilat,radius,srange,ind);
}
