/* C-bridge for FORTRAN routines init_spice */
#include "xvmaininc.h"
#include "ftnbridge.h"

initspice()
{
  FTN_NAME2_(init_spice, INIT_SPICE) ();
}
