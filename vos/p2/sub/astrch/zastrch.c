#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of ASTRCH                                         */
/************************************************************************/

void zastrch(his,lowdn,highdn,lper,hper,n)
void  *his;   /* input histogram; 1D array of int (input) */
int   *lowdn;  /* output low-end stretch limit (output) */
int   *highdn; /* output high-end stretch limit (output) */
double lper;   /* low-end percent saturation (input) */
double hper;   /* high-end percent saturation (input) */
int    n;      /* dimension of his (input) */

{
float f_lper, f_hper;
f_lper = (float) lper;
f_hper = (float) hper;

FTN_NAME2(astrch, ASTRCH) (his,lowdn,highdn,&f_lper,&f_hper,&n);
}
