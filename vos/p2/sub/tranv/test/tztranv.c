#include "xvmaininc.h"
#include "ftnbridge.h"


void FTN_NAME(tztranv)() 
{
        int    ind, m, itype;
	double xc,zc,th,th1,th2,lam,f,cas,psi,rp,re;
	float  line,samp,lat,lon;
        char pbuf[81];
/*  ==================================================================  */
        zvmessage("TRANS MERC", "");
        itype =15;
	m = 2;
	line = 500.;
	samp = 500.;
	xc = 400.0000;       
	zc = 400.0000;     
	th = 0.0000000;  
	th1 = 0.0000000;  
	th2 = 0.000000;
	lam = 150.0000;       
	f = 7.000000;       
	cas = 1.00000;      
	psi = 0.000000;   
	rp = 1815.300;  
	re = 1830.000;
        ztranv(&ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
               &line,&samp,&lat,&lon,rp,re,psi);
        if (ind != 0) zmabend("Error in ztranv");

        sprintf( pbuf, "FROM L,S  %f   %f", line, samp);
        zvmessage(pbuf, "");
        sprintf( pbuf, "TO LT,LN  %f   %f", lat, lon);
        zvmessage(pbuf, "");
	m = 1;
        ztranv(&ind,itype,m,xc,zc,th,th1,th2,lam,f,cas,
               &line,&samp,&lat,&lon,rp,re,psi);
        if (ind != 0) zmabend("Error in ztranv");
        sprintf( pbuf, "AND BACK TO L,S  %f   %f", line, samp);
        zvmessage(pbuf, "");
}
