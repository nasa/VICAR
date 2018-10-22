#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


void zbswap( in, n )
unsigned char *in;        /* array to be swapped */
int n;                    /* number of pairs of bytes in array IN */
{
FTN_NAME2(bswap, BSWAP) (in, &n);
}
