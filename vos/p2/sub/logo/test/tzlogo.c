#include "xvmaininc.h"
#include "ftnbridge.h"
/*  To test zlogo from C, we virtually use the same code as the Fortran
    bridge in logo.c  */

void FTN_NAME(tzlogo)(idn,line,ilogo,bgr,buf)
     int *idn, *line, *ilogo, *bgr;
     unsigned char *buf;
{
   zlogo( *idn, *line, *ilogo, *bgr, buf);
}
