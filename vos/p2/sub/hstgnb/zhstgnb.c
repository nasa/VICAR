#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zhstgnb - generates a histogram on a picture	*/
/************************************************************************/

void zhstgnb( nsamp, pixlin, hist)
int nsamp;                       /* number of samples in a picture line */
void *pixlin;                    /* array containing one picture line   */
void *hist;      /* array containing running accumulation for histogram */

{
FTN_NAME2(hstgnb, HSTGNB) ( &nsamp, pixlin, hist);
}
