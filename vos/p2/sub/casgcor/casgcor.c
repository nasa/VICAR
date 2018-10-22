#include <math.h>
#include "xvmaininc.h"
#include "ftnbridge.h"

/* The distortion constant must be a positive number. If it is smaller than  */
/* 6.58 E-11; G_FLOAT type doubles have to be used for the algorithm to work */
#define  DEFAULT_A			0.00000000893

#define  DEFAULT_OA_LINE		512.0
#define  DEFAULT_OA_SAMP		512.0

double cas_cubic_root();

/*---------------------------------------------------------------------------*/
/* C-Callable Version                                                        */
/*---------------------------------------------------------------------------*/

int zcasgcor(float* is_line,float* is_samp,float* os_line,float* os_samp,
	     int mode,int icam)
#if 0
  float *is_line,       /*  Image-space line coordinate             */
  float *is_samp,       /*  Image-space sample coordinate           */
  float *os_line,       /*  Object-space line coordinate            */
  float *os_samp;       /*  Object-space sample coordinate          */
  int mode;             /*  Conversion mode: 1=IS to OS, 0=OS to IS */
  int icam;             /*  2=full frame, 22,42=summation mode (WAC)*/
#endif
{
  double	oa_x, oa_y,	/*  Optical axis location                    */
	        x, y,		/*  Optical axis corrected location          */
		A_const,	/*  Theorical distortion constant            */
		os_rad,		/*  Object-space radius from optical axis    */
		is_rad,		/*  Image-space radius from optical axis     */
	        A_3,		/*  Intermediate value in IS->OS computation */
	        B_3,		/*  Intermediate value in IS->OS computation */
	        common_term1,	/*  Intermediate value in computations       */
	        common_term2;	/*  Intermediate value in computations       */
  /***  Substitute defaults for missing optional parameters  ---------***/
  oa_y = DEFAULT_OA_LINE;
  oa_x = DEFAULT_OA_SAMP;
  A_const = DEFAULT_A;

  /***  No geometric correction necessary for NAC (yet) --------------***/
  if(icam==1 || icam==21 || icam==41) {
    if (mode) {
        *os_line = *is_line;
        *os_samp = *is_samp;
    }
    else {
        *is_line = *os_line;
        *is_samp = *os_samp;
    }
    return (0);
  }

  /***  Image Space to Object Space  ---------------------------------***/
  if (mode) {
     if (icam==22) {			/* If 2x2 summation mode, */
        x = (*is_samp)*2. - oa_x;	/* convert to full-frame */
        y = (*is_line)*2. - oa_y;	/* coordinates */
     }
     else if (icam==42) {               /* If 4x4 summation mode, */
        x = (*is_samp)*4. - oa_x;       /* convert to full-frame */
        y = (*is_line)*4. - oa_y;       /* coordinates */
     }
     else {
        x = (*is_samp) - oa_x;
        y = (*is_line) - oa_y;
     }
     is_rad = sqrt((double)(x*x + y*y));

     if (is_rad == 0.0) {
        *os_line = *is_line;
        *os_samp = *is_samp;
     }
     else {
        common_term1 = is_rad/(2.0 * A_const);
        common_term2 = sqrt(pow(common_term1,(double)(2)) +
                          pow((double)(1.0/(3.0 * A_const)),(double)(3)));
        A_3 = common_term1 + common_term2;
        B_3 = common_term1 - common_term2;
        os_rad = cas_cubic_root(A_3) + cas_cubic_root(B_3);
        *os_line = (os_rad*y)/is_rad + oa_y;
        *os_samp = (os_rad*x)/is_rad + oa_x;
        if (icam==22) {
           *os_line = *os_line/2.;
           *os_samp = *os_samp/2.;
        }
        if (icam==42) {
           *os_line = *os_line/4.;
           *os_samp = *os_samp/4.;
        }

     }
  }

  /***  Object Space to Image Space  ---------------------------------***/
  else {
     if (icam == 22) {
        x = (*os_samp)*2 - oa_x;
        y = (*os_line)*2 - oa_y;
     }
     else if (icam == 42) {
        x = (*os_samp)*4 - oa_x;
        y = (*os_line)*4 - oa_y;
     }
     else {
        x = (*os_samp) - oa_x;
        y = (*os_line) - oa_y;
     }
     common_term1 = 1.0 + (A_const*(x*x + y*y));

     *is_line = y*common_term1 + oa_y;
     *is_samp = x*common_term1 + oa_x;
     if (icam==22) {
        *is_line = *is_line/2.;
        *is_samp = *is_samp/2.;
     }
     if (icam==42) {
        *is_line = *is_line/4.;
        *is_samp = *is_samp/4.;
     }

  }
  return (0);
}

/***  Determines the cubic root of x  -------------------------------------***/
double cas_cubic_root(x)
  double x;
{
  if     (x == 0) return ((double)(0));
  else if (x < 0) return ( -exp(log(fabs(x)) / 3.0) );
  else            return ( exp(log(x) / 3.0) );
}

/*---------------------------------------------------------------------------*/
/* Fortran-Callable Version                                                  */
/*---------------------------------------------------------------------------*/

void FTN_NAME2(casgcor, CASGCOR) (status,is_line,is_samp,os_line,
							 os_samp,mode,icam)
  int   *status;        /*  return status, currently information-less */
  float	*is_line;	/*  Image-space line coordinate             */
  float *is_samp;	/*  Image-space sample coordinate           */
  float	*os_line;	/*  Object-space line coordinate            */
  float	*os_samp;	/*  Object-space sample coordinate          */
  int	*mode;		/*  Conversion mode: 1=IS to OS, 0=OS to IS */
  int   *icam;		/*  2=full-frame, 22,42=summation mode (WAC)*/
{
  *status = zcasgcor(is_line,is_samp,os_line,os_samp,*mode,*icam);
}

