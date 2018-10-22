#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
/************************************************************************/
/*                                                               	*/
/************************************************************************/


void FTN_NAME(tzdllsq)() 
{
double X[2][2], AUX[5][3];
int    M, N, L, status, IPIV[2];

double A[3][2];
double B[3][2];
double EPS, *ptr;
char   tbuf [132];
int    i, j;

    /* Assign initial values */
    A[0][0] = 1.1;
    A[1][0] = 1.2;
    A[2][0] = 1.0;
    A[0][1] = 0.9;
    A[1][1] = 1.0;
    A[2][1] = 1.0;

    B[0][0] = 2.2;
    B[1][0] = 2.3;
    B[2][0] = 2.1;
    B[0][1] = 4.4;
    B[1][1] = 4.6;
    B[2][1] = 4.2;

    M     = 3;
    N     = 2;
    L     = 2;

    /* Assign value to EPS */ 
    EPS = 1.e-15;

    /* Call DLLSQ to calculate the least squares solution for test values */
    status = zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX); 

    /* If a failure was detected then print contents of array and terminate */
    if (status != 0) {
        sprintf (tbuf, "ERROR IN DLLSQ IFAIL = %d", status );
        zvmessage (tbuf, "");
        return;
    }

    /* Otherwise DLLSQ was successful ... print contents of array */
    zvmessage ("SOLUTIONS", "");
    for (j = 0; j < L; j++) {
       for (i = 0; i < N; i++) {
            sprintf (tbuf, "%7.4f", X[i][j]);
            zvmessage (tbuf, "");
        }
    } 

    /* Check Error handling */
    zvmessage ("TEST ERROR HANDLING. SHOULD GET IER=1", "");

    /* Set the memory occupied by matrix 'A' to zero */
    ptr = &A[0][0];
    memset (ptr, '\0', sizeof(double));
    ptr = &A[1][0];
    memset (ptr, '\0', sizeof(double));
    ptr = &A[2][0];
    memset (ptr, '\0', sizeof(double));

    /* Assign value to EPS */ 
    EPS = 1.e-15;

    /* Call DLLSQ to calculate the least squares solution for test values */
    status = zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX); 

    /* Print 'IER' results without testing for error return */
    sprintf (tbuf, "ERROR IN DLLSQ IFAIL = %d", status );
    zvmessage (tbuf, "");

    return;
}
