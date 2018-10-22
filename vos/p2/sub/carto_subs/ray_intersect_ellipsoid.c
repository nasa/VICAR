/*******************************************************************************

  Title:    ray_intersect_ellipsoid
  Author:   Mike Burl 
  Date:     20051110
  Function: Functions to intersect a ray with a general ellipsoid. There is also
              a functoin to intersect a ray with a centered, axis-aligned ellipsoid.
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "ray_intersect_ellipsoid.h"
#include "mat33.h"

/****************************/
/* GLOBAL DECLARATIONS      */
/****************************/

int find_intersection_point(double A, double B, double C, double *t, double *d, double *p);

/****************************/
/* RAY_INTERSECT_ELLISPOID  */
/****************************/
/* Intersect a ray with a general ellipsoid. The ray starts at 3D point t and has direction d.
   The ellipsoid is centered about p0 and has inverse covariance structure SigmaInv.
   This means that points p on the ellispoid satisfy:
     (p-p0)' * SigmaInv * (p-p0) = 1 
   If an intersection is found, the point of intersection is placed in p and the function
   return value is OK. If no intersection is found, p is unchanged from entry and the
   function return value is ERR. 
*/

int ray_intersect_ellipsoid(double *t, double *d, double *p0, double *SigmaInv, double *p)

{
  double     A, B, C;
  double     v[3];
  int        status;
  /*  char       infunc[] = "ray_intersect_ellipsoid"; */

  vec31_subtract(t, p0, v);
  A = mat33_xtax(d, SigmaInv);
  B = 2*mat33_xtay(d, SigmaInv, v);
  C = mat33_xtax(v, SigmaInv) - D_ONE;

  status = find_intersection_point(A, B, C, t, d, p);

  return(status);
}

/*******************************/
/* RAY_INTERSECT_CA_ELLISPOID  */
/*******************************/
/* Intersect a ray with a centered, axis-aligned ellipsoid. Comments same as above
   for the general case, except here the ellispoid satisfies the simpler equation:

     \sum_{i=1}^{3} p_i^2 / s_i^2 = 1

  So the values in s are the lengths of the semi-axes along X, Y, and Z directions.
 */

int ray_intersect_ca_ellipsoid(double *t, double *d, double *s, double *p)

{
  double     A, B, C;
  int        i;
  double     lambda[3];
  int        status;
  /*  char       infunc[] = "ray_intersect_ca_ellipsoid"; */

  for (i = 0; i < 3; i++) {
    lambda[i] = s[i]*s[i];
  }

  A = D_ZERO;
  for (i = 0; i < 3; i++) {
    A += (d[i]*d[i]/lambda[i]);
  }

  B = D_ZERO;
  for (i = 0; i < 3; i++) {
    B += (d[i]*t[i]/lambda[i]);
  }
  B = D_TWO*B;

  C = D_ZERO;
  for (i = 0; i < 3; i++) {
    C += (t[i]*t[i]/lambda[i]);
  }
  C = C - D_ONE;

  status = find_intersection_point(A, B, C, t, d, p);

  return(status);
}

/****************************/
/* FIND_INTERSECTION_POINT  */
/****************************/
/* Given coefficients for quadratic equation in zc, find intersection point */

int find_intersection_point(double A, double B, double C, double *t, double *d, double *p)

{
  double     D, R, z1, z2;
  /*  char       infunc[] = "find_intersection_point"; */

  D = B*B - ((double) 4.0) * A * C;
  if (D >= 0) {
    R = sqrt(D);
    z1 = (-B - R)/(D_TWO*A);
    z2 = (-B + R)/(D_TWO*A);

    if ((z1 >= 0) || (z2 >= 0)) {
      /* At least one good point */
      if ((z1 >= 0) && ( z1 <= z2)) {
        vec31_axpy(z1, d, t, p);
      }
      else {
        vec31_axpy(z2, d, t, p);
      }
    }
    else {
      /* Intersection occurs with line but not with ray */
      return(ERR);
    }
  }
  else {
    /* No real-valued roots */
    return(ERR);
  }


  return(OK);
}
