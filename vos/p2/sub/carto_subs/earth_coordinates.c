/*******************************************************************************
Title:    earth_coordinates.c
Author:   Mike Burl
Date:     2005/01/03

Function: Convert from geodetic LLH (geodetic latitude, longitude, and 
            height) coordinates to local tangent plane ENU (east,
            north, up) coordinates. There are also several helper 
            functions in here that may be independently useful:
            ecef_from_geodetic,  enu_T_ecef, enu_from_ecef, define_WGS1984

History: 2008/06/11 (MCB) - Fixed "Equator bug" and "Southern Hemisphere bug" 
           in enu_T_ecef. (See enu_T_ecef.m)
    
         2006/12/14 (MCB) - Changed C++ style comments to strict C.

         2005/11/10 (MCB) - Changed name of file from enu_from_geodetic.c to
           earth_coordinates.c. Changed functions to eliminate much of the 
           internal allocation of memory that was previously happening in here.
           Also, added dual "_to_" functions to match with the "_from_" functions.

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "earth_coordinates.h"
#include "verbosity_manager.h"
#include "io_view.h"
#include "qmalloc.h"
#include "burl.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* enu_from_geodetic                  */
/**************************************/

int enu_from_geodetic(double *ENU, double *LLH0, int n_rows, double *LLH)
{
  double         *XYZ; /* (n_rows X 3) */
  double         *T;   /* (4 X 4) */
  int            i, j;
  char           infunc[] = "enu_from_geodetic";
  /*--------------------------------------------------------------*/
  /* First transform to Earth-centered Earth-fixed cartesian coords */
  XYZ = (double *) qmalloc(n_rows*3, sizeof(double), 0, infunc, "XYZ");
  ecef_from_geodetic(XYZ, n_rows, LLH);
  /*  write_dfile("/tmp/grid_ecef", n_rows, 3, XYZ); */

  if (get_verbosity_level() >= 1) {
    printf("XYZ:\n");
    for (i = 0; i < n_rows; i++) {
      for (j = 0; j < 3; j++) {
        printf("%lf ", XYZ[i*3+j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  /* Determine transformation from ECEF coords to ENU (local tangent plane) */
  T = (double *) qmalloc(4*4, sizeof(double), 1, infunc, "T");
  enu_T_ecef(T, LLH0);

  if (get_verbosity_level() >= 1) {
    printf("T:\n");
    for (i = 0; i < 4; i++) {
      for (j = 0; j < 4; j++) {
        printf("%lf ", T[i*4+j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  /* Apply transformation to convert ECEF coords to ENU */
  ecef_to_enu(n_rows, XYZ, T, ENU);

  if (get_verbosity_level() >= 1) {
    printf("ENU:\n");
    for (i = 0; i < n_rows; i++) {
      for (j = 0; j < 3; j++) {
        printf("%lf ", ENU[i*3+j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  free((void *) XYZ);
  free((void *) T);

  return(OK);
}

/**************************************/
/* ecef_from_geodetic                 */
/**************************************/
/* XYZ and LLH should both be (n_rows X 3) */

int ecef_from_geodetic(double *XYZ, int n_rows, double *LLH)
{
  ellipsoid_struct   *ellipsoid;
  double         beta, cbeta, sbeta;
  double         lambda, clambda, slambda;
  double         h, N;
  int            k, ind;
  /*  char           infunc[] = "ecef_from_geodetic"; */

  /*--------------------------------------------------------------*/
  define_ellipsoid("WGS1984", &ellipsoid);
  ind = 0;
  for (k = 0; k < n_rows; k++) {
    beta = LLH[ind] * DEG2RAD;
    cbeta = cos(beta);
    sbeta = sin(beta);
    lambda = LLH[ind+1] * DEG2RAD;
    clambda = cos(lambda);
    slambda = sin(lambda);
    h      = LLH[ind+2];
    N = ellipsoid->a / sqrt(1.0 - ellipsoid->e2 * sbeta * sbeta);
    XYZ[ind] = (N + h) * cbeta * clambda;
    XYZ[ind+1] = (N+h) * cbeta * slambda;
    XYZ[ind+2] = (ellipsoid->rat2 * N + h) * sbeta;
    ind += 3;
  }
  free_ellipsoid(ellipsoid);

  return(OK);
}

/**************************************/
/* geodetic_from_ecef                 */
/**************************************/
int geodetic_from_ecef(double *LLH, int n_rows, double *XYZ)
{
  ellipsoid_struct   *ellipsoid;
  int                k, ind;
  double             p, theta, argy, argx, phi, N;
  /*  char               infunc[] = "geodetic_from_ecef"; */

  /*--------------------------------------------------------------*/
  define_ellipsoid("WGS1984", &ellipsoid);
  ind = 0;
  for (k = 0; k < n_rows; k++) {
    p = sqrt(XYZ[ind]*XYZ[ind] + XYZ[ind+1]*XYZ[ind+1]);
    theta = atan2(XYZ[ind+2]*ellipsoid->a, p * ellipsoid->b);
    argy = XYZ[ind+2] + ellipsoid->ep2 * ellipsoid->b * pow(sin(theta), 3.0);
    argx = p - ellipsoid->e2*ellipsoid->a*pow(cos(theta), 3.0);
    phi = atan2(argy, argx);
    N = ellipsoid->a / sqrt(1.0 - ellipsoid->e2 * sin(phi) * sin(phi));

    LLH[ind] = phi * RAD2DEG;
    LLH[ind+1] = atan2(XYZ[ind+1], XYZ[ind]) * RAD2DEG;
    LLH[ind+2] = p/cos(phi) - N;
    ind += 3;
  }
  free_ellipsoid(ellipsoid);

  return(OK);
}

/**************************************/
/* enu_T_ecef                         */
/**************************************/
/* Determine the homogeneous transformation T from ecef coordinates */
/* to enu coordinates using tangent plane at the point LL0 to */
/* define the ENU coordinate system. */
/* */
/* T is (4 X 4), LLH0 is (1 X 3) and contains */
/*   geodetic coordinates of tangent point */

int enu_T_ecef(double *T, double *LLH0)
{
  double         beta, tbeta;
  double         lambda, clambda, slambda;
  double         fac3;
  double         *V; /* (3 X 3) */
  double         XYZ[3];
  char           infunc[] = "ecef_from_geodetic";

  /*--------------------------------------------------------------*/
  geodetic_to_ecef(1, LLH0, XYZ);
  /* printf("%lf %lf %lf\n", XYZ[0], XYZ[1], XYZ[2]); */

  beta = LLH0[0] * DEG2RAD;
  lambda = LLH0[1] * DEG2RAD;
  clambda = cos(lambda);
  slambda = sin(lambda);
  tbeta = tan(beta);

  fac3 = D_ONE/sqrt(D_ONE + tbeta*tbeta);

  /* 2008/06/11 (MCB) - Bug Fix - changed calc of V[1], V[4], V[7] to resolve equator bug */
  /*   and Southern Hemisphere bug */
  V = (double *) qmalloc(3*3, sizeof(double), 1, infunc, "V");
  V[0] = -slambda; 
  V[1] = -clambda * tbeta * fac3;
  V[2] =  clambda * fac3;
  V[3] =  clambda;
  V[4] = -slambda * tbeta * fac3;
  V[5] =  slambda * fac3;
  V[6] =  D_ZERO;
  V[7] =  fac3;
  V[8] =  tbeta * fac3;

  /* Fill in entries of T = [[V', -V'*[X0; Y0; Z0]]; [0, 0, 0, 1]]; */
  T[0] = V[0];
  T[1] = V[3];
  T[2] = V[6];
  T[3] = -(V[0]*XYZ[0] + V[3]*XYZ[1] + V[6]*XYZ[2]);
  T[4] = V[1];
  T[5] = V[4];
  T[6] = V[7];
  T[7] = -(V[1]*XYZ[0] + V[4]*XYZ[1] + V[7]*XYZ[2]);
  T[8] = V[2];
  T[9] = V[5];
  T[10] = V[8];
  T[11] = -(V[2]*XYZ[0] + V[5]*XYZ[1] + V[8]*XYZ[2]);
  T[12] = D_ZERO;
  T[13] = D_ZERO;
  T[14] = D_ZERO;
  T[15] = D_ONE;

  free((void *) V);

  return(OK);
}


/**************************************/
/* enu_from_ecef                      */
/**************************************/
/* XYZ and ENU should be (n_rows X 3) */
int enu_from_ecef(double *ENU, double *T, int n_rows, double *XYZ)
{
  int            ind, k;
  double         fac;
  /*  char           infunc[] = "enu_from_ecef"; */

  /*--------------------------------------------------------------*/
  ind = 0;
  for (k = 0; k < n_rows; k++) {
    fac        = T[12]*XYZ[ind] + T[13]*XYZ[ind+1] + T[14]*XYZ[ind+2] + T[15];
    ENU[ind]   = (T[0]*XYZ[ind] + T[1]*XYZ[ind+1]  + T[2]*XYZ[ind+2]  + T[3])/fac; 
    ENU[ind+1] = (T[4]*XYZ[ind] + T[5]*XYZ[ind+1]  + T[6]*XYZ[ind+2]  + T[7])/fac;
    ENU[ind+2] = (T[8]*XYZ[ind] + T[9]*XYZ[ind+1]  + T[10]*XYZ[ind+2] + T[11])/fac;
    ind += 3;
  }

  return(OK);
}

/**************************************/
/* define_ellipsoid                   */
/**************************************/
int define_ellipsoid(char *gname, ellipsoid_struct **G_adr)
{
  ellipsoid_struct *G;
  int          l;
  char         infunc[] = "define_ellipsoid";

  if (strcasecmp(gname, "WGS1984") == 0) {
    *G_adr  = G = (ellipsoid_struct *) qmalloc(1, sizeof(ellipsoid_struct), 0, infunc, "G");
    l = strlen(gname);
    G->gname = (char *) qmalloc((l+1), sizeof(char), 0, infunc, "G->name");
    strcpy(G->gname, gname);
 
    G->a    = ((double) 6378137.0);
    G->invf = ((double) 298.257223563);
    G->f    = D_ONE/G->invf;
    G->b    = (D_ONE-G->f) * G->a;
    G->rat2 = (D_ONE-G->f)*(D_ONE - G->f);
    G->e2   = (D_ONE - G->rat2);
    G->ep2  = (D_ONE/G->rat2 - D_ONE);
  }
  else {
    fprintf(stderr, "ERROR (%s): Unknown ellipsoid |%s|\n", infunc, gname);
    return(ERR);
  }

  return(OK);
}

/**************************************/
/* free_ellipsoid                     */
/**************************************/

int free_ellipsoid(ellipsoid_struct *G)
{
  if (G != NULL) {
    if (G->gname != NULL) {
      free((void *) G->gname);
    }
    free((void *) G);
  }

  return(OK);
}


/**************************************/
/* ecef_to_enu                        */
/**************************************/
int ecef_to_enu(int n_rows, double *XYZ, double *T, double *ENU)
{ 
  int  status;
  /* char   infunc[] = "ecef_to_enu"; */

  status = enu_from_ecef(ENU, T, n_rows, XYZ);

  return(status);
}

/**************************************/
/* geodetic_to_enu                    */
/**************************************/
int geodetic_to_enu(int n_rows, double *LLH, double *LLH0, double *ENU)
{ 
  int  status;
  /* char   infunc[] = "ecef_to_enu"; */

  status = enu_from_geodetic(ENU, LLH0, n_rows, LLH);

  return(status);
}

/**************************************/
/* geodetic_to_ecef                   */
/**************************************/
int geodetic_to_ecef(int n_rows, double *LLH, double *XYZ)
{
  int   status;
  /* char   infunc[] = "geodetic_to_ecef"; */

  status = ecef_from_geodetic(XYZ, n_rows, LLH);

  return(status);
}


/**************************************/
/* ecef_to_geodetic                   */
/**************************************/
int ecef_to_geodetic(int n_rows, double *XYZ, double *LLH)
{
  int   status;
  /* char   infunc[] = "ecef_to_geodetic"; */

  status = geodetic_from_ecef(LLH, n_rows, XYZ);

  return(status);
}

