/*------------------------------------------------------------------
 * C version of rangen
 *
 * this subroutine is to be called by fortran using:
 *     CALL rangen(seed,rand_num)
 * where seed is an integer:
 *          0 > seed     < 714025
 *   and    0 > rand_num < 1.0
 *
 *---------------------------------------------------------------- */
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <math.h>
#include <stdio.h>

#define M 714025
#define IA 1366
#define IC 150889

/************************************************************************/
/* Fortran-Callable Version                                             */
/************************************************************************/

void FTN_NAME2 (rangen, RANGEN) (idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
 zrangen(idum,rand_num);
  return;
}

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/


zrangen(idum,rand_num)
  long *idum;        /* input seed (1st call only)and  returned integer*/
  float *rand_num;       /* returned random number  0> *rand_num>1 */
{
  static long iy,ir[98];
  static int iff=0;
  int j;
  char msg[80];

  j=*idum;
  *idum%=(1<<24); /* Take modulo 24, to obtain seed between 0 and 2**24-1 */
                  /* Since mod function, %, in C behaves like a reminder  */
                  /* function, negative value of idum will still cause    */
                  /* mod function to return abs value between the range.  */
  if(abs(j) > ((1<<24)-1)) {
    sprintf(msg,"RANGEN::: Random Seed value of %d adjusted to %d",j,*idum);
    zvmessage(msg,0);
  }
  if(*idum<0 || iff==0)
  {
    iff=1;
    if ((*idum=(IC - (*idum)) % M) < 0) *idum= -(*idum);
    for (j=1;j<97;j++)
    {
      *idum=(IA*(*idum)+IC) % M;
      ir[j]=(*idum);
    }
    *idum=(IA*(*idum)+IC) % M;
    iy=(*idum);
  }
  j=1 + 97.0*iy/M;
  if (j>97||j<1) 
  {
     sprintf(msg,"RANGEN: This cannot happen.");
     zvmessage(msg,"");
  }
  iy=ir[j];
  *idum=(IA*(*idum)+IC) % M;
  iy=(*idum);
  *rand_num= (float) iy/M; 
}
  
  
