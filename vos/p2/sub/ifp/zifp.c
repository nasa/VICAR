#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for ifp  -  Four point interpolation                        */
/************************************************************************/

int zifp (x, y, M, N, A, interp, ihalf)
void   *A;
float   x, y;
int     M, N, interp, ihalf;
{
int        i, j, status;
void      *ptr; 
unsigned char *ch_from, *ch_to, *ch_old_A, *ch_new_A;
short    int  *si_from, *si_to, *si_old_A, *si_new_A;

   /* If 'byte' input is specified then */
   if (ihalf == 0) {
      ch_old_A = (unsigned char *)A;     
      ch_new_A = (unsigned char *)malloc (sizeof(unsigned char)*M*N);
      ptr      = (void *)ch_new_A;
      /* Invert matrix A[M][N] */
      for (i = 0; i < M; i++) {
         for (j = 0; j < N; j++) {
            ch_from  = (ch_old_A + (i*N) + j);
            ch_to    = (ch_new_A + (j*M) + i);
           *ch_to    = *ch_from;
         }
      }
   } else {
      /* Else then 'integer*2 is the specified format */
      si_old_A = (short int *)A;     
      si_new_A = (short int *)malloc (sizeof(short int)*M*N);
      ptr      = (void *)si_new_A;
      for (i = 0; i < M; i++) {
         for (j = 0; j < N; j++) {
            si_from = (si_old_A + (i*N) + j);
            si_to   = (si_new_A + (j*M) + i);
           *si_to   = *si_from;
         }
      }
   }

   status = FTN_NAME2(ifp, IFP)( &x, &y, &M, &N, ptr, &interp, &ihalf);

   free (ptr);
   return status;
}
