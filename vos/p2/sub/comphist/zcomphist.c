#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/
void   zcomphist(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
FTN_NAME2(comphist, COMPHIST) (&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
void   zcomphist2(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    iunit,sl,ss,nl,ns;
{
FTN_NAME2(comphist2, COMPHIST2) (&iunit,&sl,&ss,&nl,&ns,ohist,ibuf);
}
