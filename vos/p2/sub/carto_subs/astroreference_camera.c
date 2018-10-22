/*******************************************************************************

  Title:     astroreference_camera
  Author:    Mike Burl 
  Date:      2007/01/25
  Function:  This function constructs a set of correspondences between image plane (u,v)
               (where u = sample, v = line) coordinates and J2000 right ascension and
               declination (DEC) coordinates.

  History:  2007/05/24 (MCB) - Changed APIs again so we now take:
              (1) an elapsed TT  (in seconds) since the instant 2000-01-01T12:00:00 TT and 
              (2) an elpased UT1 (in seconds) since the instant 2000-01-01T12:00:00 UT1.

            2007/02/26 (MCB) - Changed APIs so that we now take a TDB and UT1 time
              expressed in seconds since J2000.0 epoch rather than a UTC_string and 
              Delta_UT1 as we did previously.

            2007/02/21 (MCB) - Changed function API so that the orientation is now taken
              as a quaternion (4-element unit vector with scalar part first) rather
              than as a rodrigues rotation vector.

            2007/02/20 (MCB) - 
              The new wframe argument indicates which frame (ECEF (0) or TOD (1)) is the
                reference frame for w_t_c and w_o_c. 
              
              If the reference is ECEF (wframe == 0), then the time of the observation
                (UTC_string) and the offset between UTC and UT1 (Delta_UT1) are both
                essential to convert to TOD (and eventually to J2000).

              If the reference is TOD (wframe == 1), then the exact offset between UTC
                and UT1 (Delta_UT1) is unimportant, but the time of the observation
                (UTC_string) is essential for determining the correct conversion from 
                TOD to J2000 (due to time dependence of precession and nutation of 
                TOD axes wrt J2000 frame).

            2007/01/25 (MCB) - Based on x_eos_coords.cpp and georeference_camera.c
 
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "qmalloc.h"
#include "rodrigues.h"
#include "quaternion.h"
#include "eos_coords.h"
#include "astroreference_camera.h"
#include "mat33.h"

/******************************/
/* ASTROREFERENCE_CAMERA_SV_C */
/******************************/

int astroreference_camera_sv_c(double *urange, double *vrange, 
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
  int       wframe;
  int       status;
  /*  char      infunc[] = "astroreference_camera_sv_c"; */

  /* Perhaps not the best way, but convert quaternions to rotation matrices */
  quaternion_to_mat(TOD_q_SV, TOD_R_SV);
  quaternion_to_mat(SV_q_C, SV_R_C);
  
  /* Multiply to get composite rotation */
  mat33_mat33_mult(TOD_R_SV, SV_R_C, TOD_R_C);

  /* Convert TOD_R_C back to quaternion */
  mat_to_quaternion(TOD_R_C, TOD_q_C);

  /* Compute TOD_t_C = TOD_R_SV * SV_t_C + TOD_t_SV  */  
  mat33_vec31_mult(TOD_R_SV, SV_t_C, dum);
  vec31_add(dum, TOD_t_SV, TOD_t_C);

  wframe = ASTROREFERENCE_TOD;
  status = astroreference_camera(urange, vrange, wframe, TOD_t_C, TOD_q_C, fu, fv, q, u0, v0, kappa, TT, UT1, gr_adr, gc_adr, G_adr);

  return(status);
}

/*************************/
/* ASTROREFERENCE_CAMERA */
/*************************/
int astroreference_camera(double *urange, double *vrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr)
{
  double               w_o_c[3];
  double               J2000_R_TOD[3*3], ECEF_R_TOD[3*3];
  double               TOD_R_ECEF[3*3];
  double               J2000_R_C[3*3];
  double               TOD_R_C[3*3], ECEF_R_C[3*3];
  double               RA, DEC;
  int                  nu, nv;
  int                  i, j;
  double               u, v, un, vn;
  double               vec[3], d[3];
  int                  ind;
  double               *G;
  int                  gr, gc;
  char                 infunc[] = "astroreference_camera";

  /* Determine coordinate transformations */
  /*   If wframe is TOD, we do not care about ECEF_R_TOD, however, it is */
  /*   calculated anyway. */
  eos_coords(TT, UT1, J2000_R_TOD, ECEF_R_TOD);
  printf("J2000_R_TOD:\n");
  mat33_print(J2000_R_TOD);
  printf("\n");

  /* Convert quaternion into rodrigues vector for internals */
  quaternion_to_rodrigues(w_q_c, w_o_c);

  /* Create TOD_R_C from w_o_c */
  if (wframe == ASTROREFERENCE_ECEF) {
    rodrigues_vec2mat(w_o_c, ECEF_R_C);
    mat33_inverse(ECEF_R_TOD, TOD_R_ECEF);
    mat33_mat33_mult(TOD_R_ECEF, ECEF_R_C, TOD_R_C);
  }
  else {
    rodrigues_vec2mat(w_o_c, TOD_R_C);
  }
  printf("TOD_R_C:\n");
  mat33_print(TOD_R_C);
  printf("\n");

  /* Express camera coordinate system wrt J2000 */
  mat33_mat33_mult(J2000_R_TOD, TOD_R_C, J2000_R_C);

  printf("J2000_R_C:\n");
  mat33_print(J2000_R_C);

  /* Determine number of steps along each dimension of grid */
  nu = 1 + (int) floor((urange[2]-urange[0]+0.1)/urange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  nv = 1 + (int) floor((vrange[2]-vrange[0]+0.1)/vrange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  gr = nu *nv;
  gc = 4;

  /* Allocate array to hold correspondences */
  G = (double *) qmalloc(gr*gc, sizeof(double), 0, infunc, "G");

  ind = 0;
  for (i = 0; i < nu; i++) {
    u = urange[0] + i*urange[1];
    un = (u-u0)/fu;
    for (j = 0; j < nv; j++) {
      v = vrange[0] + j*vrange[1];
      vn = (v-v0)/fv;

      /* Determine the ray direction */
      vec31_assign(vec, un-q*vn, vn, D_ONE);
      mat33_vec31_mult(J2000_R_C, vec, d);
      vec31_unit(d); /* Make into a unit vector */

      /* Convert d into RA and DEC */
      DEC = RAD2DEG * asin(d[2]);
      RA  = RAD2DEG * atan2(d[1], d[0]);
          
      /* Stuff correspondences into G */
      G[ind] = u; /* sample */
      G[ind+1] = v; /* line */
      G[ind+2] = RA;  /* right ascension in decimal degrees */
      G[ind+3] = DEC; /* declination in decimal degrees */
      ind += gc;
    }
  }
  *gr_adr = gr;
  *gc_adr = gc;
  *G_adr = G;

  return(OK);
}
