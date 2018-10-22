#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Fortran-Callable (no C-version needed -- use #if                     */
/************************************************************************/

void FTN_NAME2(testos, TESTOS) ( ios )
  int *ios;	
{
#if VMS_OS
    *ios = 0;       /*  VMS  */
#else
#if UNIX_OS
    *ios = 1;       /*  UNIX  */
#else
    *ios = 2;       /*  other  */
#endif
#endif
}
