#include "xvmaininc.h"
#include "ftnbridge.h"


void FTN_NAME(tzconvev)(ind,line,samp,lat,lon,mode,data,conv)

int    *ind, *mode;
float  *line, *samp, *lat, *lon;
float  *data, *conv;
{
/*  ==================================================================  */
        zvmessage("TRANS MERC", "");
	zconvev(ind,data,data,line,samp,lat,lon,*mode,conv);
}
