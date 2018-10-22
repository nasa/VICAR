#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzminmax)() 
{
      int buf[32];

      int i, exclude;
      int dcode,n, min, max, imin, imax;
/*  ==================================================================  */

      for ( i=0; i<10; i++ )
      {
        buf[i] = i;
      }

      dcode = 4;
      n = 10;
      zminmax(dcode,n,buf,&min,&max,&imin,&imax);
       zvmessage("Should get MIN=0, MAX=9, IMIN=1, IMAX=10", "");
       zprnt(4,1, &min," MIN =");
       zprnt(4,1, &max," MAX =");
       zprnt(4,1, &imin," IMIN =");
       zprnt(4,1, &imax," IMAX =");

       zvmessage("***********", "");
       zvmessage("Now exclude 0.", "");
       zvmessage("Should get MIN=1, MAX=9, IMIN=2, IMAX=10", "");

      exclude=0;
      zminmaxe(dcode,n,buf, &exclude, &min,&max,&imin,&imax);
       zprnt(4,1, &min," MIN =");
       zprnt(4,1, &max," MAX =");
       zprnt(4,1, &imin," IMIN =");
       zprnt(4,1, &imax," IMAX =");
}
