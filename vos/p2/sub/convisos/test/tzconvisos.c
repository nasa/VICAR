#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzconvisos)(conv) 
float *conv;
{
      int icam,nph, npv, ind, mode;
      float is_line, is_samp, os_line, os_samp;
/*  ==================================================================  */

      icam = 7;
      nph = 24;
      npv = 23;
      ind = 0;
      is_line = 400.;
      is_samp = 400.;
      mode = 1;
      zprnt(7,1,&is_line,"IS_LINE =.");
      zprnt(7,1,&is_samp," IS_SAMP =.");
      zconvisos("VGR-2",icam,&is_line,&is_samp,&os_line,&os_samp,
                                    mode,conv,nph,npv,&ind);
      zprnt(7,1,&os_line," OS_LINE =.");
      zprnt(7,1,&os_samp," OS_SAMP =.");

}
