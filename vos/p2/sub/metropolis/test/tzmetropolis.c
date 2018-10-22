#include "xvmaininc.h"
#include <math.h>
#include "ftnbridge.h"
/**************************************************************************/
/* Example 2 Cost routine                                                 */
/* Returns the cost function resulting from guessing x (answer).          */
/* This could be any function or logical operation.                       */
/*                                                                        */
/* X     is the solution vector.                                          */
/* ARRAY is an array of N data points if needed.                          */
/* ARRAY2 is another array of N elements if needed.                       */
/* N      is the number of elements/data points in ARRAY & ARRAY2.        */
/* ERROR is the returned cost.                                            */
/* IND   is 0 for normal return, 1 for abnormal return.                   */
/*        If METROPOLIS senses an indicator of 1 it will generate another */
/*        guess somewhere else & try again.                               */
/*                                                                        */
/**************************************************************************/

void zcost2 (x, array, array2, n, error, ind) 

float *x;         /* The solution vector */
float *array;     /* An array of n data points, if needed */
float *array2;    /* Another array of n data points, if needed */

int n;              /* The number of data points in array and array2 */  
float *error;       /* The returned cost */
int *ind;           /* Status indicator */
{
  float xvp;
  double err,dxvp;
  dxvp = *(x);
  *ind=0;
  err=pow(dxvp,4.) - 16. * pow(dxvp,2.) + 5. * (dxvp);
  xvp = err;
  *error = xvp;
}

/**************************************************************************/
/* Example 1 Cost routine                                                 */
/* Returns the cost function resulting from guessing x (answer).          */
/* This could be any function or logical operation.                       */
/*                                                                        */
/* X     is the solution vector.                                          */
/* ARRAY is an array of N data points if needed.                          */
/* ARRAY2 is another array of N elements if needed.                       */
/* N      is the number of elements/data points in ARRAY & ARRAY2.        */
/* ERROR is the returned cost.                                            */
/* IND   is 0 for normal return, 1 for abnormal return.                   */
/*        If METROPOLIS senses an indicator of 1 it will generate another */
/*        guess somewhere else & try again.                               */
/*                                                                        */
/* (1)=radius (2)=x_center (3)=y_center                                   */
/**************************************************************************/

void zcost (x, array, array2, n, error, ind) 

float *x;         /* The solution vector */
float *array;     /* An array of n data points, if needed */
float *array2;    /* Another array of n data points, if needed */

int n;              /* The number of data points in array and array2 */  
float *error;       /* The returned cost */
int *ind;           /* Status indicator */

{
   double dr[10], d1, d2, d3;
   float *mpa, *xp, ferr;
   double sumdr, sum, range, m, derr;
   int j, nc; 
   *ind = 1;
   xp = x;
   if (*xp >= 5 && *xp <= 50)
      {
      ++xp;
      if (*xp >= 1 && *xp <= 100)
         {
         ++xp;
         if (*xp >= 1 && *xp <= 100)
            {
            sumdr = 0;
            sum = 0;
            range = 20;
            m = 0;
            nc = n;
            xp = x;
            mpa = array;
            for (j=0;j<nc;j++)
               {
               d1 = *xp;
               d2 = *(mpa + j) - *(xp + 1);
               d3 = *(mpa + (j + nc)) - *(xp + 2);
               dr[j] = fabs(d1 - sqrt(pow(d2,2.) + pow(d3,2.)));
               sum=sum+dr[j];
               if(dr[j]<range)
                  {
                  m++;
                  sumdr=sumdr+dr[j];
                  }
               }
            if(m==0) derr = sum/nc + range/nc;
            else derr = sumdr/m + range/m;
            ferr = derr;
            *error = ferr;
            *ind=0;
            }
         }
      }
}

/********************************************************************/
/* EXAMPLE # 2                                                      */
/* purpose to test the Metropolis algorithm in one dimension.       */
/* The example below is to find the LOWEST minimum of the           */
/* polynomial expressed by the COST function found at the           */
/* end of this example.                                             */
/********************************************************************/
zexample2 () 
{

void (*costp)();     /* The cost function to be called */
int narg;           /* The number of variables */  
float array[1];     /* An array of data */
float array2[1];    /* Another array of data */
float range[1];     /* Bounds of solution vector elements */
int numten;         /* The number of iteration to reduce the error by 10 */
float answer[1];    /* The solution vector */
int limit;          /* The total number of iterations permitted */
int norm;           /* The number of iterations between normalizations */
int npts;           /* The number of points in array and array2 */
int iprint;	    /* The number of iterations between printouts */
int ind;            /* Status indicator */
int *indp;
char msg[80], *mp;
void *ap;

numten=500;
limit=1500;
iprint=200;
narg=1;
norm=200;
npts=0;
mp=msg;
answer[0] = 3;
range[0] = 2;
indp = &ind;
costp = *zcost2;
ap = array;
zvmessage("C test case 2","");
zmetropolis(costp,narg,ap,array2,range,numten,answer,
                       limit,norm,npts,iprint,indp);

(void) sprintf(mp,"answer= %f\n", *answer); 
zvmessage(msg,""); 
zvmessage("solution should be about -2.9","");
}


/************************************************************************/
/*  Main Test routine for the "C" call to metropolis. This routine      */
/*  builds an array of two dimensions and invokes the "C" bridge        */
/*  zmetropolis.  The bridge will set up to reverse bridge to the zcost */
/*  routine associated with this invocation.                            */
/************************************************************************/

/********************************************************************/
/*  EXAMPLE # 1                                                     */
/*  Purpose: To test the Metropolis algorithm in three dimensions.  */
/*  The example below is to fit a circle to 6 data points, two      */
/*  of which do not lie on the circle.                              */
/*  Notice the cost function rewards for including lots of data     */
/*  points and is allowed to reject points which fall far from the  */
/*  fitted function.                                                */
/********************************************************************/

void FTN_NAME(tzmetropolis)(void)
{

void (*costp)();     /* The cost function to be called */
int narg;           /* The number of variables */  
float array[2][6];  /* An array of data */
float array2[1];    /* Another array of data */
float range[3];     /* Bounds of solution vector elements */
int numten;         /* The number of iteration to reduce the error by 10 */
float answer[3];    /* The solution vector */
int limit;          /* The total number of iterations permitted */
int norm;           /* The number of iterations between normalizations */
int npts;           /* The number of points in array and array2 */
int iprint;	    /* The number of iterations between printouts */
int ind;            /* Status indicator */
int *indp, j;
char msg[80], *mp;
float *answerp;
void *ap;

numten=1000;
limit=3000;
iprint=200;
narg=3;
norm=330;
npts=6;
mp=msg;
answerp=answer; 
ap=array;
/* sample,line pairs... */

array[0][0] = 10;
array[0][1] = 30;
array[0][2] = 10;
array[0][3] = 30;
array[0][4] = 80;
array[0][5] = 80;

array[1][0] = 50;
array[1][1] = 50;
array[1][2] = 70;
array[1][3] = 70;
array[1][4] = 30;
array[1][5] = 40;

/* (1)=radius (2)=x_center (3)=y_center */

answer[0]=30;
answer[1]=60;
answer[2]=50;

range[0]=15;
range[1]=20;
range[2]=20;

indp = &ind;
costp = *zcost;
zvmessage("C test case 1","");

zmetropolis(costp,narg,ap,array2,range,numten,answer,
                       limit,norm,npts,iprint,indp);

if (ind==0)
    {
        (void) sprintf(mp,"answer= ");
        mp=(mp+8); 
        for (j=0; j<narg; j++)
        {
           (void) sprintf(mp," %14.5f",*(answerp+j));
           mp=(mp+14);
        }
        zvmessage(msg,"");
    }
else zvmessage("answer = no solution","");
zvmessage(" solution should be about 14.1 20. 60.","");

zexample2();  /* try example 2 */

}


