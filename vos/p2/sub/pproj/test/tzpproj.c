#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzpproj)(map)
float map[];
{
float lat, lon, line, samp, rtang, slant;
int ind, ilat;
char pbuf[133];
/*  ==================================================================  */
	line = 400.;
	samp = 500.;
start:	ilat = 1;	/* geodetic lat */
	zvmessage("DERIVE (LT,LN) FROM (L,S)","");
        zpproj(map,&line,&samp,&lat,&lon,2,ilat,&rtang,&slant,&ind);
	if (ind == 0) {
	  zvmessage("POINT IS OFF PLANET","");
	  zprnt(7,1,&rtang,"TANGENT RADIUS =.");
        }
	else {
          sprintf( pbuf, "L,S,LT,LN %10.3E %10.3E %10.3E %10.3E", 
                          line,samp,lat,lon); 
          zvmessage(pbuf, "");
	  zprnt(7,1,&rtang,"RADIUS =.");
	  zprnt(7,1,&slant,"SLANT DISTANCE =.");
	}
/*  put in a test for rtang option:  */
        if (line <= 400.) {
	  line = 1500.;
	  samp = 5000.;
 	  goto start;
        }
	lat = 30.;
	lon = 5.;
	zvmessage("DERIVE (L,S) FROM (LT,LN)","");
	zpproj(map,&line,&samp,&lat,&lon,1,ilat,&rtang,&slant,&ind);
	if (ind != 0) {
          sprintf( pbuf, "L,S,LT,LN %10.3E %10.3E %10.3E %10.3E", 
                          line,samp,lat,lon); 
          zvmessage(pbuf, "");
 	  return;
        }
        else 
 	  zvmessage("POINT IS ON BACKSIDE OF PLANET",""); 
}
