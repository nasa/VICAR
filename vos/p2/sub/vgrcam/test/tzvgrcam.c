#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/*  Unit test C-bridge for TVGRCAM.F */
/************************************************************************/

void FTN_NAME(tzvgrcam) (sln,buffer)
int   *sln;     /* Voyager camera serial number (input) */
void  *buffer;  /* four-word buffer (output) */


{
       zvgrcam(*sln,buffer);
}
