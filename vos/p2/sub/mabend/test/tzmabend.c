#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzmabend)() 
{
      static char msg[] = "This is a C message";
/*  ============================================  */

      if (zvptst("CCHAR"))   zmabend( msg);
      else                   zmabend("This is also a C message");
}
