#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzcomphist)(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    *iunit,*sl,*ss,*nl,*ns;
{
/*  ============================================  */

      zcomphist(*iunit,*sl,*ss,*nl,*ns,ohist,ibuf);
}
/************************************************************************/
void FTN_NAME(tzcomphist2)(iunit,sl,ss,nl,ns,ohist,ibuf)
void   *ohist,*ibuf;	
int    *iunit,*sl,*ss,*nl,*ns;
{
/*  ============================================  */

      zcomphist2(*iunit,*sl,*ss,*nl,*ns,ohist,ibuf);
}
