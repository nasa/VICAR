#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzcorcav)(omma,rsvec) 
double *omma, *rsvec;
{
float lat, elo, focl, req, flat, line, samp, cl, cs, flag, rpol;
int ind;
/*  ==================================================================  */
      req = 1815.0;
      rpol = 1815.0;
      flat = req - rpol;
      ind = 0;
      line  = 0.;
      samp  = 0.;
      focl = 1500.19 * 84.821;
      lat = -13.8018;
      elo = 360.0-150.1259;
      cl = 500.0;
      cs = 500.0;
  zcorcav(&ind,&lat,&elo,omma,rsvec,&focl,&req,&flat,&line,&samp,&cl,&cs,&flag);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");

}
