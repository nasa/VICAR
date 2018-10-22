#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TRFT2.F */
/************************************************************************/
void FTN_NAME(tzrft2) (matrix,nl,ns,flag,status)
void  *matrix;  /* input matrix; 2D array of float      (input/output) */
int   *nl;       /* number of lines in the matrix        (input) */
int   *ns;       /* number of samples in the matrix      (input) */
int   *flag;    /* flag to do forward/reverse transform (input) */
int   *status; 
{
      zrft2(matrix,*nl,*ns,*flag,*status);
}



