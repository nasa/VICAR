#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>

/* Moves len bytes of data from Fortran character*n variable "from" to	*/
/* non-character*n buffer "to".  No C-callable version is necessary.	*/
/* No error checking is performed on the length of the Fortran string;	*/
/* it's assumed the "len" parameter is valid.				*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(mvcl, MVCL)
(
  char *from,			/* Fortran CHARACTER*n variable */
  char *to,			/* output buffer, NOT a CHARACTER*n */
  int *len,			/* Length in bytes to move */
  ZFORSTR_PARAM
)
{
   ZFORSTR_BLOCK

   zmove(zsfor2ptr(from), to, *len);
}

