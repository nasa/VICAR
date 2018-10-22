#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcorcav - CONVERT LAT,LON TO LINE, SAMP FOR PERSPECTIVE*/
/************************************************************************/

void zcorcav(ind,lat,elo,omma,rsvec,focl,req,flat,line,samp,cl,cs,flag)
int *ind;
float *lat, *elo, *focl, *req, *flat, *line, *samp, *cl, *cs, *flag;
double *omma, *rsvec;
{
FTN_NAME2(corcav, CORCAV) (ind,lat,elo,omma,rsvec,focl,req,flat,line,samp,cl,cs,flag);
}
