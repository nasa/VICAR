#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztritra - TRANSFORM COORDINATES USING GEOMA PARAMS*/
/************************************************************************/

void ztritra(int* ind,float* conv, int nph,int npv,float* line,
	     float* samp,int mode)
{
FTN_NAME(tritra)( ind,conv,&nph,&npv,line,samp,&mode);
}
