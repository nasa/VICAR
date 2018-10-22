/*******************************************************************************

  Title:    rodrigues
  Author:   Mike Burl 
  Date:     20030611
  Function: Functions to convert a rodrigues-style rotation vector into an orthonormal 
              rotation matrix via rodrigues' formula and vice-versa. Note that
              the rodrigues vector is equivalent to the unit-vector axis of
              rotation multiplied by the angle of rotation about that axis (in
              radians).

  History: 20070125 (MCB) - Added check that the incoming matrix in 
              rodrigues_mat2vec has det = 1. Also, fixed a longstanding bug
              with respect to Bouguet's method of disambiguating signs in 
              the theta = pi case.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "rodrigues.h"

#define RODRIGUES_EPS 1e-12

/**********************/
/* RODRIGUES_VEC2MAT  */
/**********************/
/* Convert a rotation vector into an orthonormal rotation matrix
   via rodrigues' formula */

int rodrigues_vec2mat(double *wrot, double *R)

{
  double     theta;
  double     alpha, beta, gamma;
  double     w0, w1, w2;
  /*  char       infunc[] = "rodrigues_vec2mat"; */

  theta = sqrt(wrot[0]*wrot[0] + wrot[1]*wrot[1] + wrot[2]*wrot[2]);
  if (theta < RODRIGUES_EPS) {
    /* R = identity matrix */
    R[0] = D_ONE;  R[1] = D_ZERO; R[2] = D_ZERO;
    R[3] = D_ZERO; R[4] = D_ONE;  R[5] = D_ZERO;
    R[6] = D_ZERO; R[7] = D_ZERO; R[8] = D_ONE;
  }
  else {
    alpha = cos(theta);
    beta  = sin(theta);
    gamma = 1-alpha;
    w0   = wrot[0]/theta;
    w1   = wrot[1]/theta;
    w2   = wrot[2]/theta;
    R[0] = alpha            + gamma*w0*w0; 
    R[1] =         -beta*w2 + gamma*w0*w1;
    R[2] =          beta*w1 + gamma*w0*w2;
    R[3] =          beta*w2 + gamma*w1*w0;
    R[4] = alpha            + gamma*w1*w1;  
    R[5] =         -beta*w0 + gamma*w1*w2;
    R[6] =         -beta*w1 + gamma*w2*w0;
    R[7] =          beta*w0 + gamma*w2*w1;
    R[8] = alpha            + gamma*w2*w2;
  }

  return(OK);
}
/**********************/
/* RODRIGUES_MAT2VEC  */
/**********************/
/* Convert an orthonormal rotation matrix into a rodrigues-style rotation vector
   via rodrigues' formula */

int rodrigues_mat2vec(double *R, double *wrot)

{
  double     c, s, theta;
  double     uabs, vabs, wabs;
  double     u, v, w;
  double     fac;
  double     mvec[3];
  int        i, hash, idx;
  int        n_hashvec = 11;
  int        hashvec[11] = { 0, -1, -3, -9, 9, 3, 1, 13, 5, -7, -11 };
  int        svec[11][3] = {
              { 1,  1,  1 },
              { 1,  0, -1 },
              { 0,  1, -1 },
              { 1, -1,  0 },
              { 1,  1,  0 },
              { 0,  1,  1 },
              { 1,  0,  1 },
              { 1,  1,  1 },
              { 1,  1, -1 },
              { 1, -1, -1 },
              { 1, -1,  1 }};
  char       infunc[] = "rodrigues_mat2vec";

  c = (R[0] + R[4] + R[8] - D_ONE)/D_TWO;
  theta = acos(c);
  s = sin(theta);

  if (fabs(theta) < RODRIGUES_EPS) {
    /* No rotation */
    wrot[0] = D_ZERO;
    wrot[1] = D_ZERO;
    wrot[2] = D_ZERO;
  }
  else if (fabs(theta-M_PI) < RODRIGUES_EPS) {
    /* Rotation by PI */
    uabs = sqrt((R[0]+D_ONE)/D_TWO);
    vabs = sqrt((R[4]+D_ONE)/D_TWO);
    wabs = sqrt((R[8]+D_ONE)/D_TWO);

    mvec[0] = (R[1] + D_ONE)/D_TWO;
    mvec[1] = (R[5] + D_ONE)/D_TWO;
    mvec[2] = (R[2] + D_ONE)/D_TWO;

    hash = 0;
    if (mvec[0] > RODRIGUES_EPS) {
      hash += 9;
    }
    else if (mvec[0] < -RODRIGUES_EPS) {    
      hash -= 9;
    }
    if (mvec[1] > RODRIGUES_EPS) {
      hash += 3;
    }
    else if (mvec[1] < -RODRIGUES_EPS) {    
      hash -= 3;
    }
    if (mvec[2] > RODRIGUES_EPS) {
      hash += 1;
    }
    else if (mvec[2] < -RODRIGUES_EPS) {    
      hash -= 1;
    }
    idx = -1;
    for (i = 0; i < n_hashvec; i++) {
      if (hash == hashvec[i]) {
        idx = i;
        break;
      }
    }
    if (idx == -1) {
      fprintf(stderr, "ERROR (%s): invalid hash = %d\n", infunc, hash);
      return(ERR);
    }
    wrot[0] = theta * uabs * svec[idx][0];
    wrot[1] = theta * vabs * svec[idx][1];
    wrot[2] = theta * wabs * svec[idx][2];
  }
  else {
    /* the normal case */
    fac = theta/(D_TWO*s);
    u   = R[7] - R[5];
    v   = R[2] - R[6];
    w   = R[3] - R[1];
    wrot[0] = fac * u;
    wrot[1] = fac * v;
    wrot[2] = fac * w;
  }

  return(OK);
}
