/* Functions to handle various operations on 3 x 3 matrices */
/* HISTORY:*/
/*   2002/07/24 (MCB) Found and fixed bug in mat33_inverse involving */
/*                      incorrect formula for element B[7]=-c5 */
/*   2002/05/22 (MCB) Improved mat33_mat33_mult to allow inplace op. */
/*   2002/05/21 (MCB) Found and fixed a bug in mat33_transpose. */

#include <stdio.h>
#include <math.h>
#include "burl.h"
#include "mat33.h"

#define EPS 1e-12

int mat33_mat33_mult(double *A, double *B, double *C) {

  double c0, c1, c2;
  double c3, c4, c5;
  double c6, c7, c8;

  c0 = A[0]*B[0]+A[1]*B[3]+A[2]*B[6];
  c1 = A[0]*B[1]+A[1]*B[4]+A[2]*B[7];
  c2 = A[0]*B[2]+A[1]*B[5]+A[2]*B[8];

  c3 = A[3]*B[0]+A[4]*B[3]+A[5]*B[6];
  c4 = A[3]*B[1]+A[4]*B[4]+A[5]*B[7];
  c5 = A[3]*B[2]+A[4]*B[5]+A[5]*B[8];

  c6 = A[6]*B[0]+A[7]*B[3]+A[8]*B[6];
  c7 = A[6]*B[1]+A[7]*B[4]+A[8]*B[7];
  c8 = A[6]*B[2]+A[7]*B[5]+A[8]*B[8];

  mat33_assign(C, c0, c1, c2, c3, c4, c5, c6, c7, c8);

  return(OK);
}

int mat33_print(double *A) {
  printf("%.15f %.15f %.15f\n", A[0], A[1], A[2]);
  printf("%.15f %.15f %.15f\n", A[3], A[4], A[5]);
  printf("%.15f %.15f %.15f\n", A[6], A[7], A[8]);

  return(OK);
}

int vec31_print(double *A) {
  printf("%.15f\n", A[0]);
  printf("%.15f\n", A[1]);
  printf("%.15f\n", A[2]);

  return(OK);
}

double mat33_det(double *A) {
  double det;

  det = A[0]*(A[4]*A[8]-A[5]*A[7]) - A[1]*(A[3]*A[8]-A[5]*A[6]) + A[2]*(A[3]*A[7]-A[4]*A[6]);

  return(det);
}

int mat33_copy(double *A, double *B) {

  A[0] = B[0];
  A[1] = B[1];
  A[2] = B[2];
  A[3] = B[3];
  A[4] = B[4];
  A[5] = B[5];
  A[6] = B[6];
  A[7] = B[7];
  A[8] = B[8];

  return(OK);
}

int vec31_copy(double *A, double *B) {

  A[0] = B[0];
  A[1] = B[1];
  A[2] = B[2];

  return(OK);
}

int mat33_inverse(double *A, double *B) {
  double c0, c1, c2;
  double c3, c4, c5;
  double c6, c7, c8;
  double det, idet;

  c0 = (A[4]*A[8]-A[5]*A[7]);
  c1 = (A[3]*A[8]-A[5]*A[6]);
  c2 = (A[3]*A[7]-A[4]*A[6]);
  c3 = (A[1]*A[8]-A[2]*A[7]);
  c4 = (A[0]*A[8]-A[2]*A[6]);
  c5 = (A[0]*A[7]-A[1]*A[6]);
  c6 = (A[1]*A[5]-A[2]*A[4]);
  c7 = (A[0]*A[5]-A[2]*A[3]);
  c8 = (A[0]*A[4]-A[1]*A[3]);
  det = A[0]*c0 - A[1]*c1 + A[2]*c2;
  idet = D_ONE/det;
  mat33_assign(B, c0*idet, -c3*idet, c6*idet, -c1*idet, c4*idet, -c7*idet, c2*idet, -c5*idet, c8*idet);

  return(OK);
}

int mat33_assign(double *A, double a0, double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8) {
  A[0] = a0;
  A[1] = a1;
  A[2] = a2;
  A[3] = a3;
  A[4] = a4;
  A[5] = a5;
  A[6] = a6;
  A[7] = a7;
  A[8] = a8;

  return(OK);
}

int mat33_transpose(double *A, double *B) {
  /* Do it with all these temp variable to allow transpose in place */
  double a0, a1, a2;
  double a3, a4, a5;
  double a6, a7, a8;

  a0 = A[0];
  a1 = A[1];
  a2 = A[2];
  a3 = A[3];
  a4 = A[4];
  a5 = A[5];
  a6 = A[6];
  a7 = A[7];
  a8 = A[8];

  mat33_assign(B, a0, a3, a6, a1, a4, a7, a2, a5, a8);

  return(OK);
}

int vec31_assign(double *V, double v0, double v1, double v2) {
  V[0] = v0;
  V[1] = v1;
  V[2] = v2;

  return(OK);
}

int mat33_vec31_mult(double *A, double *V, double *B) {
  double c0, c1, c2;
  c0 = A[0]*V[0] + A[1]*V[1] + A[2]*V[2];
  c1 = A[3]*V[0] + A[4]*V[1] + A[5]*V[2];
  c2 = A[6]*V[0] + A[7]*V[1] + A[8]*V[2];
  vec31_assign(B, c0, c1, c2);

  return(OK);
}

int vec31_add(double *A, double *B, double *C) {
  C[0] = A[0]+B[0];
  C[1] = A[1]+B[1];
  C[2] = A[2]+B[2];

  return(OK);
}

int vec31_subtract(double *A, double *B, double *C) {
  C[0] = A[0]-B[0];
  C[1] = A[1]-B[1];
  C[2] = A[2]-B[2];

  return(OK);
}

int mat33_add(double *A, double *B, double *C) {

  int  i;

  for (i = 0; i < 8; i++) {
    C[i] = A[i] + B[i];
  }

  return(OK);
}

int mat33_scale(double *A, double s, double *B) {
  int i;
  for (i = 0; i < 8; i++) {
    B[i] = s*A[i];
  }
  return(OK);
}

int vec31_scale(double *A, double s, double *B) {

  B[0] = s*A[0];
  B[1] = s*A[1];
  B[2] = s*A[2];

  return(OK);
}

/*MCB: 20051110 - changed name from mat33_xax to mat33_xtax */
double mat33_xtax(double *X, double *A) {
  int    i, j;
  double d;

  d = D_ZERO;
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      d += X[i] * A[i*3+j] * X[j];
    }
  }
  return(d);
}

double mat33_xtay(double *X, double *A, double *Y) {
  int    i, j;
  double d;

  d = D_ZERO;
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      d += X[i] * A[i*3+j] * Y[j];
    }
  }
  return(d);
}

double vec31_vec31_dot(double *A, double *B) {
  return( A[0]*B[0] + A[1]*B[1] + A[2]*B[2]);
}

int vec31_axpy(double a, double *X, double *Y, double *Z) {
  int    i;

  for (i = 0; i < 3; i++) {
    Z[i] = a*X[i] + Y[i];
  }

  return(OK);
}

int vec31_vec31_cross(double *A, double *B, double *C) {
  C[0] = A[1]*B[2] - A[2]*B[1];
  C[1] = A[2]*B[0] - A[0]*B[2];
  C[2] = A[0]*B[1] - A[1]*B[0];

  return(OK);
}

double vec31_norm(double *A) {
  return( sqrt(A[0]*A[0] + A[1]*A[1] + A[2]*A[2]) );
}

int vec31_unit(double *A) {
  double s;
  char   infunc[] = "vec31_unit";

  s = vec31_norm(A);

  if (s < EPS) {
    fprintf(stderr, "ERROR (%s): cannot normalize 0 vector\n", infunc);
    return(ERR);
  }
  vec31_scale(A, D_ONE/s, A);

  return(OK);
}

int vec31_vec31_basis(double *A, double *B, double *C) {
  /* Compute an orthonormal basis from two vectors A and B */
  /* Each *ROW* of C will be a unit vector for the basis */
  double Ahat[3], Bhat[3];
  double D[3], E[3];
  char   infunc[] = "vec31_vec31_basis";

  vec31_assign(Ahat, A[0], A[1], A[2]);
  if (vec31_unit(Ahat) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use A for basis\n", infunc);
    return(ERR);
  }
  vec31_assign(Bhat, B[0], B[1], B[2]);
  if (vec31_unit(Bhat) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use B for basis\n", infunc);
    return(ERR);
  }
  
  vec31_vec31_cross(Ahat, Bhat, D);
  if (vec31_unit(D) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use D for basis\n", infunc);
    return(ERR);
  }

  vec31_vec31_cross(D, Ahat, E);

  /* Place vectors into appropriate rows of C */
  vec31_assign(C,   Ahat[0], Ahat[1], Ahat[2]);
  vec31_assign(C+3, E[0], E[1], E[2]);
  vec31_assign(C+6, D[0], D[1], D[2]);

  return(OK);
}
