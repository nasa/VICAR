#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzsortin)()
{
  int i,n,buf[10];
  short hbuf[10],hptr[10];

  n=10;
  for (i=0; i<n; i++) {
     buf[i] = -1000 * i + 8000;
     hbuf[i] = buf[i];
     hptr[i] = i + 1;
  }

  zprnt(4,10,buf,"Test zsortin: buf=");
  zsortin(buf,n);
  zprnt(4,10,buf," Sorted buf=");

  zprnt(2,10,hbuf,"Test zi2sort: buf=");
  zi2sort(hbuf,hptr,n);
  zprnt(2,10,hbuf,"Sorted buf=");
  zprnt(2,10,hptr,"Pointer=");
}
