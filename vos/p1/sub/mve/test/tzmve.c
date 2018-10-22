#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* 	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void FTN_NAME(tzmve)() 
{
      int buf[32];
      unsigned char  bbuf[20],bbug[10];
      short hbuf[20],hbug[10];
      int   fbuf[20],fbug[10];
      float rbuf[20],rbug[10];
      double dbuf[20],dbug[10];

      int i, ival, ns;

      for ( i=0; i<10; i++ )
      {
        ival = i - 4;
        bbuf[i] = ival;
        hbuf[i] = ival;
        fbuf[i] = ival;
        rbuf[i] = ival + 0.123412341234;
        dbuf[i] = ival + 0.123412341234;
      }
      
      hbuf[8] = -32768;
      hbuf[9] = 32767;
      fbuf[6] = -32769;
      fbuf[7] = -32768;
      fbuf[8] = 32767;
      fbuf[9] = 32768;

      ns = 10;
       zvmessage("Test #1:  AINC and BINC =1", "");
       zprnt(1,ns,bbuf," Initial bbuf=");
       zprnt(2,ns,hbuf," Initial hbuf=");
       zprnt(4,ns,fbuf," Initial fbuf=");
       zprnt(7,ns,rbuf," Initial rbuf=");
       zprnt(8,ns,dbuf," Initial dbuf=");
       zmve(-9,ns,dbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=-9");
       zmve(-8,ns,dbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=-8");
       zmve(-7,ns,rbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=-7");
       zmve(-6,ns,fbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=-6");
       zmve(-5,ns,fbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-5");
       zmve(-4,ns,fbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=-4");
       zmve(-3,ns,hbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-3");
       zmve(-2,ns,hbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=-2");
       zmve(-1,ns,bbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-1");
       zmve(0,ns,bbuf,buf,1,1);
       zmve(1,ns,bbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=1");
       zmve(2,ns,hbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=2");
       zmve(3,ns,bbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=3");
       zmve(4,ns,fbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=4");
       zmve(5,ns,bbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=5");
       zmve(6,ns,hbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=6");
       zmve(7,ns,rbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=7");
       zmve(8,ns,dbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=8");
       zmve(9,ns,rbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=9");

      for ( i=0; i<20; i++ )
      {
      ival = i;
      bbuf[i] = ival;
      hbuf[i] = ival;
      fbuf[i] = ival;
      rbuf[i] = ival;
      dbuf[i] = ival;
      }
      
      ns = 10;
       zvmessage(" Test #2:  AINC=2 and BINC=-1)", "");
       zprnt(1,20,bbuf," Initial bbuf=");
       zprnt(2,20,hbuf," Initial hbuf=");
       zprnt(4,20,fbuf," Initial fbuf=");
       zprnt(7,20,rbuf," Initial rbuf=");
       zprnt(8,20,dbuf," Initial dbuf=");
       zmve(-9,ns,dbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=-9");
       zmve(-8,ns,dbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=-8");
       zmve(-7,ns,rbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=-7");
       zmve(-6,ns,fbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=-6");
       zmve(-5,ns,fbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-5");
       zmve(-4,ns,fbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=-4");
       zmve(-3,ns,hbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-3");
       zmve(-2,ns,hbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=-2");
       zmve(-1,ns,bbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-1");
       zmve(0,ns,bbuf,&bbug[9],2,-1);
       zmve(1,ns,bbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=1");
       zmve(2,ns,hbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=2");
       zmve(3,ns,bbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=3");
       zmve(4,ns,fbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=4");
       zmve(5,ns,bbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=5");
       zmve(6,ns,hbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=6");
       zmve(7,ns,rbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=7");
       zmve(8,ns,dbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=8");
       zmve(9,ns,rbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=9");
}
