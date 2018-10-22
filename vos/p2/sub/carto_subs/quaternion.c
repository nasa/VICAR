/*******************************************************************************

  Title:    quaternion
  Author:   Mike Burl 
  Date:     20051109
  Function: Functions to convert a quaternion into a rodrigues vector or an
              orthonormal matrix.

  History:  20070828 (MCB) - FIxed the "* f" bug in mat_to_quaternion().

            20070309 (MCB) - Introduced safe_sqrt instead of sqrt to avoid getting
              NaN's in rot_to_quaternion().

            20061024 (MCB) - Found a bug in mat_to_quaternion; wasn't dividing 
              by 2 in initial calculations.
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "quaternion.h"
#include "mat33.h"
#include "safe_sqrt.h"

#define QUATERNION_EPS 1e-12

/****************************/
/* RODRIGUES_TO_QUATERNION  */
/****************************/
/* Convert a rodrigues vector into a unit quaternion */

int rodrigues_to_quaternion(double *v, double *q)

{
  double         theta, c, s;
  /*  char       infunc[] = "rodrigues_to_quaternion"; */

  theta = safe_sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
  c = cos(theta/D_TWO);
  s = sin(theta/D_TWO);
  if (fabs(theta) > QUATERNION_EPS) {
    q[0] = c;
    q[1] = s * v[0]/theta;
    q[2] = s * v[1]/theta;
    q[3] = s * v[2]/theta;
  }
  else {
    q[0] = D_ONE;
    q[1] = D_ZERO;
    q[2] = D_ZERO;
    q[3] = D_ZERO;
  }
  return(OK);
}

/****************************/
/* QUATERNION_TO_RODRIGUES  */
/****************************/
/* Convert a unit quaternion into a rodrigues vector */

int quaternion_to_rodrigues(double *q, double *v)

{
  double     theta, f;
  /*  char       infunc[] = "quaternion_to_rodrigues"; */

  theta = D_TWO * acos(q[0]);
  f = safe_sqrt(D_ONE - q[0]*q[0]);

  if (fabs(theta) < QUATERNION_EPS) {
    /* The rotation is zero */
    v[0] = D_ZERO;
    v[1] = D_ZERO;
    v[2] = D_ZERO;
  }
  else {
    v[0] = theta * q[1]/f;
    v[1] = theta * q[2]/f;
    v[2] = theta * q[3]/f;
  }

  return(OK);
}

/**********************/
/* QUATERNION_TO_MAT  */
/**********************/
/* Convert a quaternion to an orthonormal rotation matrix */

int quaternion_to_mat(double *q, double *R)

{
  double     q00, q01, q02, q03;
  double     q11, q12, q13;
  double     q22, q23;
  double     q33;
  /*  char       infunc[] = "quaternion_to_mat"; */

  q00 = q[0] * q[0];
  q01 = q[0] * q[1];
  q02 = q[0] * q[2];
  q03 = q[0] * q[3];
  q11 = q[1] * q[1];
  q12 = q[1] * q[2];
  q13 = q[1] * q[3];
  q22 = q[2] * q[2];
  q23 = q[2] * q[3];
  q33 = q[3] * q[3];

  R[0] = q00 + q11 - q22 - q33;
  R[1] = D_TWO * (-q03 + q12);
  R[2] = D_TWO * (q02 + q13);
  R[3] = D_TWO * (q03 + q12);
  R[4] = q00-q11+q22-q33;
  R[5] = D_TWO * (-q01 + q23);
  R[6] = D_TWO * (-q02 + q13);
  R[7] = D_TWO * (q01 + q23);
  R[8] = q00 - q11 - q22 + q33;

  return(OK);
}

/**********************/
/* MAT_TO_QUATERNION  */
/**********************/
/* Convert a rotation matrix into a quaternion */

int mat_to_quaternion(double *R, double *q)
{
  double     q0, q1, q2, q3, f;
  char       infunc[] = "mat_to_quaternion";

  /* This is only correct up to a sign ambiguity */
  q0 = safe_sqrt(1 + R[0] + R[4] + R[8])/D_TWO ;
  q1 = safe_sqrt(1 + R[0] - R[4] - R[8])/D_TWO ;
  q2 = safe_sqrt(1 - R[0] + R[4] - R[8])/D_TWO ;
  q3 = safe_sqrt(1 - R[0] - R[4] + R[8])/D_TWO ;

  /* Determine which of this is the maximum */
  if ((q0 >= q1) && (q0 >= q2) && (q0 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q0);
    q[0] = q0;
    q[1] = (R[7]-R[5]) * f;
    q[2] = (R[2]-R[6]) * f;
    q[3] = (R[3]-R[1]) * f;
  }
  else if ((q1 >= q0) && (q1 >= q2) && (q1 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q1);
    q[0] = (R[7]-R[5]) * f;
    q[1] = q1;
    q[2] = (R[1]+R[3]) * f;
    q[3] = (R[2]+R[6]) * f;
  }
  else if ((q2 >= q0) && (q2 >= q1) && (q2 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q2);
    q[0] = (R[2]-R[6]) * f;
    q[1] = (R[1]+R[3]) * f;
    q[2] = q2;
    q[3] = (R[5]+R[7]) * f;
  }
  else if ((q3 >= q0) && (q3 >= q1) && (q3 >= q2)) {
    f = D_ONE/( ((double) 4.0) * q3);
    q[0] = (R[3]-R[1]) * f;
    q[1] = (R[2]+R[6]) * f;
    q[2] = (R[5]+R[7]) * f;
    q[3] = q3;
  }
  else {
    fprintf(stderr, "ERROR (%s): bug in code\n", infunc);
    printf("q0 = %.15f, q1 = %.15f, q2 = %.15f, q3 = %.15f\n", q0, q1, q2, q3);
    mat33_print(R);
    return(ERR);
  }
  return(OK);
}
