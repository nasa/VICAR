/*   A C-bridge routine, called by TPBNAME.F, that tests the C-bridge version
   of PBNAME, ZPbname.c    
*/

#include "xvmaininc.h"
#include "ftnbridge.h"
void FTN_NAME(tzpbname) ()
{
  int i, jj, k1, k2;  
  char pbuf[32];
  char name[13];

  zvmessage(" ", " ");
  zvmessage(" ******  Testing C-Bridge Version  ****** ", " ");
  zvmessage(" ", " ");

  for (i=0; i < 999; i++) {
    k1 = zpbname(i, name) ;
    k2 = zpbid(name, &jj) ;
    if (k1 == 1  &&  k2 == 1) {
     snprintf( pbuf, 32, " id = %03d  name = %12s", jj, name);
     zvmessage(pbuf, " ");
    }
  }

}
