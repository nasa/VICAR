#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tztritra)(conv) 
float *conv;
{
      int nph, npv, ind, mode;
      float line, samp;
/*  ==================================================================  */

      nph = 24;
      npv = 23;
      ind = 0;
      line = 400.;
      samp = 400.;
      mode = 1;
      zprnt(7,1,&line,"LINE =.");
      zprnt(7,1,&samp," SAMP =.");
      ztritra(&ind,conv,nph,npv,&line,&samp,mode);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");

/*      TEST2 FOR TRITRA*/

      line = 498.56;
      samp = 498.59;
      mode = 0;
      zprnt(7,1,&line,"LINE =.");
      zprnt(7,1,&samp," SAMP =.");
      ztritra(&ind,conv,nph,npv,&line,&samp,mode);
      zprnt(7,1,&line," LINE =.");
      zprnt(7,1,&samp," SAMP =.");
}
