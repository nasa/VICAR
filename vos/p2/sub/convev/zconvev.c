#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zconvev - Converts from line,sample to lat,long and 
   the reverse 								*/
/************************************************************************/

void zconvev(ind,data,idata,line,samp,lat,lon,mode,conv)

int *ind,*idata;
int  mode;
float *line, *samp, *lat, *lon;
float *data, *conv;

{
FTN_NAME2(convev, CONVEV) (ind,data,idata,line,samp,lat,lon,&mode,conv);
}
