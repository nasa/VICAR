#include "xvmaininc.h"
#include "ftnbridge.h"

/*
Bridge for RESET1, called from C
*/
void zreset1()

{
  FTN_NAME(reset1) ();
}
