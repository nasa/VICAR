#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzippcov)(om, rs, ema, pts)
double *om, *rs, *ema;
float *pts;
{
float rlat, rlon, line, samp, cl, cs, flag;
/*  ==================================================================  */
      rlat =0.;
      rlon =0.;
      line = 614.10;
      samp = 614.38;
      cl = 500.0;
      cs = 500.0;
      flag = 99.99;
  zippcov(&rlat,&rlon,&line, &samp,pts, rs, om,ema, &cl,&cs,&flag);
      rlat = 57.0*rlat;
      rlon = 57.0*rlon;  /*  convert to degrees  */
      zprnt(7,1,&rlat," LAT =.");
      zprnt(7,1,&rlon," LONG =.");

}
