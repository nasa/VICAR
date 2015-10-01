#include "xvmaininc.h"
#include "ftnbridge.h"
#include "rtlintproto.h"

/* Moves bytes of data.  Syntactic sugar for the exec routine v2_move().*/
/* Note that the arguments are reversed with respect to v2_move()... 	*/
/* it's zmove(from,to,len) but it's v2_move(to,from,len).  Note that	*/
/* under Fortran, neither argument can be a character*n variable.	*/

/************************************************************************/
/* Fortran-Callable Version						*/
/************************************************************************/

void FTN_NAME2(xmove, XMOVE) (char *from, char *to, int *len)
{
   v2_move(to, from, *len);
}

/************************************************************************/
/* C-Callable Version							*/
/************************************************************************/

void zmove(void *from, void *to, int len)
{
   v2_move(to, from, len);
}
