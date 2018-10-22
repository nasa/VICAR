/*   C-Callable VGROS Subroutine.                  */

#include "xvmaininc.h"
#include "ftnbridge.h"

void zvgros(icam,oloc)
int icam;
float *oloc;
{
     FTN_NAME(vgros)(&icam,oloc);
}
