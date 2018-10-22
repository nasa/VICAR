#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2                                         */
/************************************************************************/

void zrft2(matrix,nl,ns,flag,status)
void  *matrix;  /* input matrix; 2D array of float      (input/output) */
int   nl;       /* number of lines in the matrix (M)        (input) */
int   ns;       /* number of samples in the matrix (N)      (input) */
int   flag;     /* flag to do forward/reverse transform (input) */
int   *status;  /* error statuses for the situation where:
		  - the dimension of the input matrix in the
                   real direction, isn't even.  (status = -3)
                  - one of the dimensions of the input matrix
                   has too large a prime factor. (status = -2)
		   -one of the square-free factors of the 
                   matrix dimensions is too large. (status = -1)
                             -or-
                  -successful operation (status=1) */
{
FTN_NAME(rft2)(matrix,&nl,&ns,&flag,&status);
}

