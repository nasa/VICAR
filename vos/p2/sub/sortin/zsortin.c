#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zsortin(buf,n)
int *buf;	
int n;		
{
FTN_NAME(sortin)(buf,&n);
}

void zi2sort(buf,ip,n)
short *buf,*ip;
int n;
{
FTN_NAME(i2sort)(buf,ip,&n);
}
