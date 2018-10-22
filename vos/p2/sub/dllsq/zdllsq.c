#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for DLLSQ  -  calculate least squares solution.             */
/************************************************************************/

int zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX)
void   *A, *B, *X, *AUX;
double  EPS;
int     M, N, L, *IPIV[];
{

double  *from, *to, *old_A, *new_A, *old_B, *new_B, *old_X, *new_X;
int     i, j, status;

   old_A = (double *)A;     
   /* Invert matrix A[M][N] */
   new_A = (double *)malloc (sizeof(double)*M*N);
   for (i = 0; i < M; i++) {
      for (j = 0; j < N; j++) {
         from  = (old_A + (i*N) + j);
         to    = (new_A + (j*M) + i);
         *to   = *from;
      }
   }

   old_B = (double *)B;     
   /* Invert matrix B[M][L] */
   new_B = (double *)malloc (sizeof(double)*M*L);
   for (j = 0; j < L; j++) {
      for (i = 0; i < M; i++) {
         from = (old_B + (i*L) + j);
         to   = (new_B + (j*M) + i);
         *to  = *from;
      }
   }

   new_X = (double *)malloc (sizeof(double)*N*L);
   old_X = (double *)X;

   FTN_NAME2(dllsq, DLLSQ) ( new_A, new_B, &M, &N, &L, new_X, IPIV, &EPS, &status, AUX);

   /* Invert matrix X[N][L] */
   for (i = 0; i < N; i++) {
      for (j = 0; j < L; j++) {
         from = (new_X + (i*L) + j);
         to   = (old_X + (j*N) + i);
         *to  = *from;
      }
   }

   free (new_A);
   free (new_B);
   free (new_X);
   return status;
}
