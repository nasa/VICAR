#include  "xvmaininc.h"  
#include  "ftnbridge.h"
/************************************************************************/
/*  C-Callable Version SPICE2CONVEV                                     */
/************************************************************************/

void  zspice2convev(rbuf,fl,oal,oas,scale,mptype,data)
double  rbuf[];       /* spice buffer */  
double  fl;           /* focal length in mm. */
double  oal;          /* optical axis line object space  */
double  oas;          /* optical axis sample object space */
double  scale;        /* object space scale */
int     mptype;       /* projection type */
float   data[];       /* returned buffer */
/* Parameters fl,oal,oas and scale are declared as double to            */
/* receive the correct values due to the C parameter promotion feature. */
{float focal_length,opt_axis_line,opt_axis_samp,obj_space_scal;
       focal_length  = (float)fl;
       opt_axis_line = (float)oal;
       opt_axis_samp = (float)oas;
       obj_space_scal= (float)scale;
FTN_NAME(spice2convev) (rbuf,&focal_length,&opt_axis_line,&opt_axis_samp,
         &obj_space_scal,&mptype,data);
}
