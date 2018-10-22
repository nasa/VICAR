#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TASTRCH.F */
/************************************************************************/
void FTN_NAME(tzastrch) (his,lowdn,highdn,lper,hper,n)
void  *his;   /* input histogram; 1D array of int (input) */
int   *lowdn;  /* output low-end stretch limit (output) */
int   *highdn; /* output high-end stretch limit (output) */
float *lper;   /* low-end percent saturation (input) */
float *hper;   /* high-end percent saturation (input) */
int   *n;      /* dimension of his (input) */

{
      zastrch(his,lowdn,highdn,*lper,*hper,*n);
}



