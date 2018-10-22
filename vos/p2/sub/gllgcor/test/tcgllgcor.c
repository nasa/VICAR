#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

#define ERROR_TOLERANCE 0.00005

FTN_NAME(tcgllgcor)()
{

  int lc,lc1;
  float is_line,
	is_samp,
	os_line,
	os_samp,
        line_dif,
        samp_dif;
  char	output[128];


  sprintf(output,"Tolerance is set to %10.8f",ERROR_TOLERANCE);
  zvmessage(output,"");

  for (lc=(-100);lc<=900;lc+=100)
     for (lc1=(-100);lc1<=900;lc1+=100) {
        is_line = lc;
        is_samp = lc1;

        zgllgcor(&is_line,&is_samp,&os_line,&os_samp,1,0);
        zgllgcor(&is_line,&is_samp,&os_line,&os_samp,0,0);

        line_dif = fabs(is_line-(float)lc);
        samp_dif = fabs(is_samp-(float)lc1);

        if (line_dif > ERROR_TOLERANCE || samp_dif > ERROR_TOLERANCE) {

            sprintf(output,"line: %d %10.6f %10.6f samp: %d %10.6f %10.6f",
                       lc, is_line,line_dif,lc1,is_samp,samp_dif);
            zvmessage(output,"");
        }
  }
}
