#include "xvmaininc.h"
#include "ftnbridge.h"
#include <zvproto.h>

/* Moves len bytes of data from a non-character*n buffer "from" to a	*/
/* Fortran character*n variable "to".  No C-callable version is		*/
/* necessary.  No error checking is performed on the length of the	*/
/* Fortran string; it's assumed the "len" parameter is valid.		*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(mvlc, MVLC)
(
  char *from,			/* input buffer, NOT a CHARACTER*n */
  char *to,			/* Fortran CHARACTER*n variable */
  int *len,			/* Length in bytes to move */
  ZFORSTR_PARAM
)
{
   ZFORSTR_BLOCK

   zmove(from, zsfor2ptr(to), *len);
}

