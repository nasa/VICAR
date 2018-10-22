#include "vicmain_c"
#include "ftnbridge.h"

void main44()
{
  char output[80];
  int n=3,ifail,i,j;
  float a[3][3],y[3];
  double da[3][3],dy[3];

  a[0][0]=33;  a[0][1]=-24;  a[0][2]= -8;
  a[1][0]=16;  a[1][1]=-10;  a[1][2]= -4;
  a[2][0]=72;  a[2][1]=-57;  a[2][2]=-17;

  y[0]=-359;   y[1]=281;     y[2]=85;

  for (i=0; i<3; i++) {
     dy[i] = y[i];
     for (j=0; j<3; j++) {
        da[i][j] = a[i][j];
     }
  }

  zvmessage(" Input matices:","");
  zvmessage("          A                 Y","");

  for (i=0; i<3; i++) {
     sprintf(output,"%6.2f %6.2f %6.2f    %6.2f",
	a[i][0],a[i][1],a[i][2],y[i]);
     zvmessage(output,"");
  }

  zvmessage(" ","");
  zvmessage("Single precision test:","");
  ifail = zsimq(a,y,n);
  if (ifail != 0) {
     zvmessage(" ***SIMQ failed","");
     zabend();
  }

  zvmessage("Output matrices:","");
  zvmessage("          A                 Y","");
  for (i=0; i<3; i++) {
     sprintf(output,"%6.2f %6.2f %6.2f    %6.2f",
	a[i][0],a[i][1],a[i][2],y[i]);
     zvmessage(output,"");
  }

  zvmessage(" ","");
  zvmessage("Double precision test:","");
  ifail = zdsimq2(da,dy,n);
  if (ifail != 0) {
     zvmessage(" ***dsimq2 failed","");
     zabend();
  }

  zvmessage("Output matrices:","");
  zvmessage("          A                 Y","");
  for (i=0; i<3; i++) {
     sprintf(output,"%6.2lf %6.2lf %6.2lf    %6.2lf",
	da[i][0],da[i][1],da[i][2],dy[i]);
     zvmessage(output,"");
  }
}
