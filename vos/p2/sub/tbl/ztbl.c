#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: ztbl - replaces values in an array by means of   */
/* a table lookup.                                                      */
/************************************************************************/

void ztbl( buf, tab, n)
unsigned char *buf;                   /* array of values to be modified */
unsigned char *tab;	              /* lookup table of 256 elements */
int n;                                /* number of bytes in buf */

{
FTN_NAME(tbl)( buf, tab, &n);
}
