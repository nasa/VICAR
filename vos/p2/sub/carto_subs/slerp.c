/*******************************************************************************

  Title:    slerp
  Author:   Mike Burl 
  Date:     20060109
  Function: Functions to do spherical linear interpolation (slerp) between two 
              n-dimensional vectors.  Also includes a specialized version
              for doing slerp on unit quaternions that represent rotations. 
              Quaternions are a slightly different case because the quaternions
              q and -q both represent the same rotation.  

  NOTE:     DOT_THRESHOLD detects cases where sin(Omega) is too close to zero.
            The threshold was determined as: cos(asin(1e-06))

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "slerp.h"

#define DOT_THRESHOLD (1-0.5e-12)
#define SLERP_EPS 1e-12

/****************************/
/* QSLERP                   */
/****************************/
/* Perform spehrical linear interpolation of quaternions; a and b MUST 
   be (4 x 1) unit vectors; t must be in [0,1]. The quaternion case is
   slightly different than the vector case, because the quaternions q and
   -q both represent the same rotation. Given two quaternions q1 and q2
   that represent rotations, we want the angle between the two to be 
   between -pi/2 and pi/2 (negating one of the quaternions if necessary
   to achieve this), before we apply the standard slerp. */

int qslerp(double *a, double *b, double t, double *c)

{
  int           i;
  double        dab;
  double        tmp[4];
  /* // char       infunc[] = "qslerp"; */

  dab = D_ZERO;
  for (i = 0; i < 4; i++) {
    dab += a[i] * b[i];
  }
  if (dab < 0) {
    /* // Negate the second quaternion */
    for (i = 0; i < 4; i++) {
      tmp[i] = -b[i];
    }
  }
  else {
    /* // Use the second quaternion as-is */
    for (i = 0; i < 4; i++) {
      tmp[i] = b[i];
    }
  }

  slerp(4, a, tmp, t, c);

  return(OK);
}

/****************************/
/* SLERP                    */
/****************************/
/* Perform sperical linear interpolation; a and b MUST be n-dimensional
   unit vectors; t must be in [0,1]. The result is returned in c (as 
   an n-dimensional unit vector). */

int slerp(int n, double *a, double *b, double t, double *c)

{
  int           i;
  double        dab, abdab, dc;
  double        fac;
  double        Omega;
  double        s1;
  double        alpha, beta;
  char          infunc[] = "slerp";
  /*-----------------------------------------------------------------------------------------*/
  dab = D_ZERO;
  for (i = 0; i < n; i++) {
    dab += a[i] * b[i];
  }
  abdab = fabs(dab);
  
  if (abdab > (D_ONE + SLERP_EPS)) {
    /* // If we're way out of range, it probably means we weren't given unit vectors */
    fprintf(stderr, "ERROR (%s): dot product out of range = %.15f\n", infunc, dab);
    return(ERR);
  }
  if (dab > D_ONE) {
    /* // If an epsilon error puts us just slightly outside legal range, fix it. */
    dab = D_ONE;
  }
  if (dab >= DOT_THRESHOLD) {
    /* // If cos(Omega) is too close to 1, just linearly interpolate and normalize  */
    /* //   since this means sin(Omega) is too close to 0 */
    alpha = (D_ONE-t);
    beta  = t;
  }
  else if (dab <= -DOT_THRESHOLD) {
    /* // vectors are antipodal, there is no unique solution */
    fprintf(stderr, "ERROR (%s): vectors are antipodal => problem ill-posed, dot product = %.15f\n", infunc, dab);
    return(ERR);
  }
  else {
    /* // Usual slerp case */
    Omega = acos(dab);
    s1 = sin(Omega);
    alpha = sin((D_ONE-t)*Omega)/s1;
    beta = sin(t * Omega)/s1; 
  }

  /* Perform the interpolation */
  for (i = 0; i < n; i++) {
    c[i] = a[i]*alpha + b[i] * beta;
  }

  /* Normalize the result */
  /*   Unecessary in theory for the usual slerp case, but may be safer, given numerical errors */
  dc = D_ZERO;
  for (i = 0; i < n; i++) {
    dc += c[i]*c[i];
  }
  fac = D_ONE/sqrt(dc);
  for (i = 0; i < n; i++) {
    c[i] = c[i]*fac;
  }

  return(OK);
}
