#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* Unit test C-bridge for TFFTT.F                                         */
/************************************************************************/

void FTN_NAME(tzfftt) (power,mode,buffer)
int  *power;     /* input size = 2**power elements  (input)*/
int  *mode;      /* +1 for inverse transform        (input)
                  -1 for direct transform*/
void *buffer;   /* a complex*8 data array          (i/o)*/

{
      zfftt(*power,*mode,buffer);
}

