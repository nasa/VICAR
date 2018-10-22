/*  This is the C-Callable portion for the PGRID subroutine  */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zpgrid(loc,nr,nc,trix,mode)
int nr, nc, mode;
short int trix;
float *loc;
{
     FTN_NAME(pgrid)(loc,&nr,&nc,&trix,&mode);
}
