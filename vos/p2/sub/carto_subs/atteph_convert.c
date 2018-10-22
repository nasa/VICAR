/*******************************************************************************

  Title:     atteph_convert
  Author:    Mike Burl 
  Date:      2006/01/09
  Function:  This function interpolates attitude and ephemeris quaternions and
               returns the result as either (a) two (4 X 1) quaternions stacked into
               an (8 X 1) result vector, or (b) two (3 X 1) vectors stacked into a (6 X 1)
               result vector. In both cases, the ephemeris (position) is at the top of 
               the result vector (elements 0:3 or 0:2) and the attitude (orientation)
               is at the bottom of the result vector (elements 4:7 or 3:5).

  Note:      The time t must be such that:

                peph[4] <= t <= seph[4]
                patt[4] <= t <= satt[4] 

  History:  

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "slerp.h"
#include "quaternion.h"
#include "atteph_convert.h"

#define ATTEPH_EPS 1e-12

/***********************/
/* ATTEPH_CONVERT8     */
/***********************/
int atteph_convert8(double *peph, double *patt, double *seph, double *satt, double t, double *v)
{
  int                  i;
  double               r, s;
  double               att[4];
  char                 infunc[] = "atteph_convert8";

 
  /* Interpolate position */
  if ((t < peph[4]) || (t > seph[4])) {
    fprintf(stderr, "ERROR (%s): bad ephemeris timepoints %lf %lf %lf\n", infunc, peph[4], t, seph[4]);
    return(ERR);
  }
  if (fabs(seph[4]-peph[4]) > ATTEPH_EPS) {
    r = (t - peph[4])/(seph[4]-peph[4]);
    /* // Just linear interpolation for the position */
    for (i = 0; i < 4; i++) {
      v[i] = (D_ONE-r) * peph[i] + r* seph[i];
    }
  }
  else {
    /* No interpolation necessary; just copy peph */
    for (i = 0; i < 4; i++) {
      v[i] = peph[i];
    }
  }

  /* Interpolate attitude */
  if ((t < patt[4]) || (t > satt[4])) {
    fprintf(stderr, "ERROR (%s): bad attitude timepoints %lf %lf %lf\n", infunc, patt[4], t, satt[4]);
    return(ERR);
  }
  if (fabs(satt[4]-patt[4]) > ATTEPH_EPS) {
    s = (t - patt[4])/(satt[4]-patt[4]);
    qslerp(patt, satt, s, att); /* Quaternion slerp (BUG: 20060327 - was commented) */
    for (i = 0; i < 4; i++) {
      v[i+4] = att[i];
    } 
  }
  else {
    /* No interpolation necessary; just copy patt */
    for (i = 0; i < 4; i++) {
      v[i+4] = patt[i];
    } 
  }
  
  return(OK);
}

/***********************/
/* ATTEPH_CONVERT6     */
/***********************/
int atteph_convert6(double *peph, double *patt, double *seph, double *satt, double t, double *v)
{
  int                  i;
  int                  status;
  double               vtmp[8];
  char                 infunc[] = "atteph_convert6";

  status = atteph_convert8(peph, patt, seph, satt, t, vtmp);
  if (status != OK) {
    fprintf(stderr, "ERROR (%s): atteph_convert8 returned status = %d\n", infunc, status);
    return(status);
  }

  /* Copy position into v */
  for (i = 0; i < 3; i++) {
    v[i]   = vtmp[i+1];
  }

  /* Convert attitude quaternion to rodrigues vector */
  
  quaternion_to_rodrigues(vtmp+4, v+3);
  
  return(OK);
}
