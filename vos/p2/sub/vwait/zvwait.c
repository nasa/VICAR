#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zvwait( hundredths )
int hundredths;	
{
FTN_NAME(vwait)( &hundredths);
}
