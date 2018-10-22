#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of FFTT                                         */
/************************************************************************/

void zfftt(power,mode,buffer)
int  power;     /* input size = 2**power elements  (input)*/
int  mode;      /* +1 for inverse transform        (input)
                  -1 for direct transform*/
void *buffer;   /* a complex*8 data array          (i/o)*/

{
FTN_NAME2(fftt, FFTT) (&power,&mode,buffer);
}

