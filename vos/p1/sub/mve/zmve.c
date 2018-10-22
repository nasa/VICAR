#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME2(mve, MVE) (int* dcode, int *n, int* a, int* b,
			int* inca, int* incb);

/************************************************************************/
/* C-Callable Version: zmve - move values from array a to array b	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void zmve(
  int dcode,			/* specifies numeric type of a and b	*/
  int n,			/* number of values ot move		*/
  void *a,			/* array FROM which values are moved	*/
  void *b,			/* array TO which values are moved	*/
  int inca,			/* spacing (step) of elements of a to be moved*/
  int incb			/* spacing (step) of elements of b	*/
)
{
FTN_NAME2(mve, MVE) ( &dcode, &n, a, b, &inca, &incb);
}
