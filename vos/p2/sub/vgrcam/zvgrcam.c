#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2CH                                         */
/************************************************************************/

void zvgrcam(sln,buffer)
int   sln;       /* Voyager camera serial number (input) */
void  *buffer;  /* four-word buffer (output) */

{
FTN_NAME(vgrcam)(&sln,buffer);
}
