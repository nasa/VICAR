/*******************************************************************************

  Title:     georeference_camera
  Author:    Mike Burl 
  Date:      2005/11/10
  Function:  This function constructs a set of correspondences between image plane (u,v)
               (where u = sample, v = line) coordinates and geodetic (lat, lon, height)
               coordinates. The set of correspondences can be used to estimate an equivalent
               RPC model. See B-0116 for background and additional details.

  History:  2007/05/24 (MCB) - Changed APIs again so we now take:
              (1) an elapsed TT  (in seconds) since the instant 2000-01-01T12:00:00 TT and 
              (2) an elpased UT1 (in seconds) since the instant 2000-01-01T12:00:00 UT1.

            2007/02/26 (MCB) - Changed APIs to take TDB and UT1 expressed in seconds
              since J2000.0 epoch rather than UTC_string and Delta_UT1 as before.

            2007/02/21 (MCB) - Converted API to take orientation as a unit quaternion 
              rather than as a rodrigues/rotation vector.
 
            2007/02/20 (MCB) - Added changes to support position and orientation being
              defined relative to the quasi-inertial true-of-date (TOD) frame, while still
              supporting the previous convention that everything must be defined relative 
              to ECEF. 

              The new wframe argument indicates which frame (ECEF (0) or TOD (1)) is the
              reference frame for w_t_c and w_o_c. 
              
              If the reference is ECEF (wframe == 0), then the time of the observation
                (UTC_string) and the offset between UTC and UT1 (Delta_UT1) are both
                ignored.

              However, if the reference is TOD (wframe == 1), then the
                time of the observation (UTC_string) and the offset between UTC
                and UT1 (Delta_UT1) are essential for determining the correct
                conversion from TOD to ECEF. 

            2006/12/14 (MCB) - Added 0.1 hack to calculation of nu, nv, nh to avoid
              epsilon erros in calculation of number of steps along each dimension of 
              the grid. Also changed C++ style comments to strict C style.

  Notes:
            Delta_UT1 = UT1 - UTC; leap seconds are used to keep the absolute value
              of this difference less than 0.9.

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "qmalloc.h"
#include "rodrigues.h"
#include "quaternion.h"
#include "eos_coords.h"
#include "earth_coordinates.h"
#include "georeference_camera.h"
#include "ray_intersect_ellipsoid.h"
#include "mat33.h"

/****************************/
/* GEOREFERENCE_CAMERA_SV_C */
/****************************/

int georeference_camera_sv_c(double *urange, double *vrange, double *hrange, 
    double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, 
    double fu, double fv, double q, double u0, double v0, double *kappa, 
    double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr)
{
  double    TOD_R_SV[3*3];
  double    SV_R_C[3*3];
  double    TOD_R_C[3*3];
  double    dum[3];
  double    TOD_t_C[3];
  double    TOD_q_C[4];
  int       status;
  int       wframe;
  /*  char      infunc[] = "georeference_camera_sc_c"; */

  /* Perhaps not the best way, but convert quaternions to rotation matrices */
  quaternion_to_mat(TOD_q_SV, TOD_R_SV);
  printf("TOD_R_SV:\n");
  mat33_print(TOD_R_SV);
  printf("\n");

  quaternion_to_mat(SV_q_C, SV_R_C);
  printf("SV_R_C:\n");
  mat33_print(SV_R_C);
  printf("\n");

  /* Multiply to get composite rotation */
  mat33_mat33_mult(TOD_R_SV, SV_R_C, TOD_R_C);
  mat33_print(TOD_R_C);

  /* Convert TOD_R_C back to quaternion */
  mat_to_quaternion(TOD_R_C, TOD_q_C);

  /* Compute TOD_t_C = TOD_R_SV * SV_t_C + TOD_t_SV  */  
  mat33_vec31_mult(TOD_R_SV, SV_t_C, dum);
  vec31_add(dum, TOD_t_SV, TOD_t_C);

  wframe = GEOREFERENCE_TOD;
  status = georeference_camera(urange, vrange, hrange, wframe, TOD_t_C, TOD_q_C, fu, fv, q, u0, v0, kappa, TT, UT1, gr_adr, gc_adr, G_adr);

  return(status);
}

/***********************/
/* GEOREFERENCE_CAMERA */
/***********************/
int georeference_camera(double *urange, double *vrange, double *hrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr)
{
  ellipsoid_struct     *ellipsoid;
  double               w_o_c[3];
  double               J2000_R_TOD[3*3], ECEF_R_TOD[3*3];
  double               TOD_R_C[3*3];
  double               ECEF_R_C[3*3], ECEF_t_C[3];
  int                  nu, nv, nh, N;
  int                  i, j, k, n;
  double               u, v, un, vn, hh;
  double               *tmp;
  double               vec[3], d[3], s[3], p[3];
  double               PLH[3];
  int                  ind, status;
  double               *G;
  int                  gr, gc, gk;
  char                 infunc[] = "georeference_camera";

  define_ellipsoid("WGS1984", &ellipsoid);

  /* Convert w_q_c into a rodrigues/rotation vector for internal use */
  quaternion_to_rodrigues(w_q_c, w_o_c);

  /* Create ECEF_R_C from w_o_c, which was given wrt TOD */
  /*   Also, create ECEF_t_C from w_t_c, which was also given wrt TOD */
  if (wframe == GEOREFERENCE_TOD) {
    /* Determine coordinate transformation between ECEF and TOD; */
    /*   Here, we do not care about J2000, but it is calculated anyway */
    eos_coords(TT, UT1, J2000_R_TOD, ECEF_R_TOD);
    printf("ECEF_R_TOD:\n");
    mat33_print(ECEF_R_TOD);
    printf("\n");

    rodrigues_vec2mat(w_o_c, TOD_R_C);
    mat33_mat33_mult(ECEF_R_TOD, TOD_R_C, ECEF_R_C);
    mat33_vec31_mult(ECEF_R_TOD, w_t_c, ECEF_t_C); /* Convert the translation vector to ECEF too */
  }
  else {
    rodrigues_vec2mat(w_o_c, ECEF_R_C);
    vec31_copy(w_t_c, ECEF_t_C);
  }
  printf("ECEF_R_C:\n");
  mat33_print(ECEF_R_C);
  printf("\n\n");
  printf("ECEF_t_C:\n");
  vec31_print(ECEF_t_C);
  printf("\n\n");

  /* Determine number of steps along each dimension of grid */
  nu = 1 + (int) floor((urange[2]-urange[0]+0.1)/urange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  nv = 1 + (int) floor((vrange[2]-vrange[0]+0.1)/vrange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  nh = 1 + (int) floor((hrange[2]-hrange[0]+0.1)/hrange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  N = nu *nv * nh;

  /* Allocate temporary array to hold correspondences */
  tmp = (double *) qmalloc(N*5, sizeof(double), 0, infunc, "tmp");

  n = 0;
  for (i = 0; i < nu; i++) {
    u = urange[0] + i*urange[1];
    un = (u-u0)/fu;
    for (j = 0; j < nv; j++) {
      v = vrange[0] + j*vrange[1];
      vn = (v-v0)/fv;

      /* Determine the ray direction */
      vec31_assign(vec, un-q*vn, vn, D_ONE);
      mat33_vec31_mult(ECEF_R_C, vec, d);

      /* Now intersect with ellipsoids of different dilations (approximate) */
      for (k = 0; k < nh; k++) {
        hh = hrange[0] + k*hrange[1];
        vec31_assign(s, (ellipsoid->a)+hh, (ellipsoid->a)+hh, (ellipsoid->b)+hh);
        status = ray_intersect_ca_ellipsoid(ECEF_t_C, d, s, p);
        if (status != ERR) {
          ind = n*5;
          /* Convert intersection point to geodetic coords */
          ecef_to_geodetic(1, p, PLH);
          
          /* Stuff correspondences into tmp */
          tmp[ind] = u;        /* sample */
          tmp[ind+1] = v;      /* line */
          tmp[ind+2] = PLH[0]; /* Geodetic latitude (in decimal degrees) */
          tmp[ind+3] = PLH[1]; /* Longitude (in decimal degrees) */
          tmp[ind+4] = PLH[2]; /* Height (in meters) */
          n = n+1;
        }
      }
    }
  }

  /* Final packaging step */
  gr = n;
  gc = 5;
  gk = gr*gc;
  if (n < N) {
    /* Didn't get all the intersections we expected. Copy correspondences */
    /*   into a smaller array that is jsut the right size */
    G = (double *) qmalloc(gr*gc, sizeof(double), 0, infunc, "G");
    for (k = 0; k < gk; k++) {
      G[k] = tmp[k];
    }
    free((void *) tmp);
    *G_adr = G;
  }
  else {
    *G_adr = tmp; /* no need to recopy */
  }
  *gr_adr = gr;
  *gc_adr = gc;
  
  free_ellipsoid(ellipsoid);
  return(OK);
}
