#include "vicmain_c"

void main44()
{
  int i,n,buf[10],buf2[10],ptr[10];
  float rbuf[10],rbuf2[10];
  double dbuf[10],dbuf2[10];

  n=10;
  for (i=0; i<n; i++) {
     buf[i] = -1000 * i + 8000;
     buf2[i] = buf[i];
     rbuf[i] = buf[i];
     rbuf2[i] = buf[i];
     dbuf[i] = buf[i];
     dbuf2[i] = buf[i];
  }

  zprnt(4,10,buf,"Test zisort: buf=");
  zisort(buf,1,n);
  zprnt(4,10,buf," Sorted buf =");

  zprnt(7,10,rbuf,"Test zssort: buf=");
  zssort(rbuf,1,n);
  zprnt(7,10,rbuf," Sorted buf =");

  zprnt(8,10,dbuf,"Test zdsort: buf=");
  zdsort(dbuf,1,n);
  zprnt(8,10,dbuf," Sorted buf =");

  zprnt(4,10,buf2,"Test zisortp: buf=");
  zisortp(buf2,1,n,ptr);
  for (i=0; i<n; i++) buf[i]=buf2[ptr[i]-1];
  zprnt(4,10,buf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");

  zprnt(7,10,rbuf2,"Test zssortp: buf=");
  zssortp(rbuf2,1,n,ptr);
  for (i=0; i<n; i++) rbuf[i]=rbuf2[ptr[i]-1];
  zprnt(7,10,rbuf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");

  zprnt(8,10,dbuf2,"\nTest zdsortp: buf=");
  zdsortp(dbuf2,1,n,ptr);
  for (i=0; i<n; i++) dbuf[i]=dbuf2[ptr[i]-1];
  zprnt(8,10,dbuf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");
}
