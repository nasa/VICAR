#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zcylpatch - Compute Line intercepting equator 	*/
/************************************************************************/
struct data
  {
  float rdata[38];
  int idata;
  float rdata40;
  };

void zcylpatch (rdata)
struct data  *rdata;			/* input structure of data */

{

FTN_NAME2(cylpatch, CYLPATCH) ( rdata); /* invoke cylpatch */

}
