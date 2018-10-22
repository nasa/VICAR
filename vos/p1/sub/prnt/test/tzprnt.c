#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzprnt)(code,siz,abuf,hbuf,ibuf,rbuf,dbuf,zbuf) 

  int code[],siz[];
  unsigned char *abuf;
  short         *hbuf;
  int           *ibuf;
  float         *rbuf;
  double        *dbuf;
  float         *zbuf;
{
      int d, n, nn;

/*  ==================================================================  */



      zvmessage("Test #1:  with titles","");
      for ( d=0; d<7; d++ )
      {
        for ( nn=0; nn<3; nn=nn+2 )
        {
	    n = (nn+1)*siz[d];
	    if (d == 0)            zprnt(code[d], n, abuf, "DUMP ");
	    if (d == 1)            zprnt(code[d], n, abuf, "BYTE ");
	    if (d == 2)            zprnt(code[d], n, hbuf, "HALFWD ");
	    if (d == 3)            zprnt(code[d], n, ibuf, "FULLWD ");
	    if (d == 4)            zprnt(code[d], n, rbuf, "REAL*4 ");
	    if (d == 5)            zprnt(code[d], n, dbuf, "REAL*8 ");
	    if (d == 6)            zprnt(code[d], n, zbuf, "COMPLEX ");
	    zvmessage("","");
         }
      }
      zvmessage("Test #2:  repeat without titles","");

      for ( d=0; d<7; d++ )
      {
        for ( nn=0; nn<3; nn=nn+2 )
        {
	    n = (nn+1)*siz[d];
	    if (d == 0)            zprnt(code[d], n, abuf, "");
	    if (d == 1)            zprnt(code[d], n, abuf, "");
	    if (d == 2)            zprnt(code[d], n, hbuf, "");
	    if (d == 3)            zprnt(code[d], n, ibuf, "");
	    if (d == 4)            zprnt(code[d], n, rbuf, "");
	    if (d == 5)            zprnt(code[d], n, dbuf, "");
	    if (d == 6)            zprnt(code[d], n, zbuf, "");
	    zvmessage("","");
         }
      }

      zvmessage("Test #3:  repeat with titles AND n=1","");
      for ( d=0; d<7; d++ )
      {
	    n = 1;
	    if (d == 0)            zprnt(code[d], n, abuf, "DUMP ");
	    if (d == 1)            zprnt(code[d], n, abuf, "BYTE ");
	    if (d == 2)            zprnt(code[d], n, hbuf, "HALFWD ");
	    if (d == 3)            zprnt(code[d], n, ibuf, "FULLWD ");
	    if (d == 4)            zprnt(code[d], n, rbuf, "REAL*4 ");
	    if (d == 5)            zprnt(code[d], n, dbuf, "REAL*8 ");
	    if (d == 6)            zprnt(code[d], n, zbuf, "COMPLEX ");
	    zvmessage("","");
      }
}
