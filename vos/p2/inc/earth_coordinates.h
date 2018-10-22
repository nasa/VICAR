#ifndef __EARTH_COORDINATES_H
#define __EARTH_COORDINATES_H

/* HISTORY: 2005/11/10 (MCB) - Changed name of this file from enu_from_geodetic.h to
   earth_coordinates.h. Changed interfaces to many of these functions so that
   there is no longer internal allocation of arrays. The caller of these functions 
   knows what size arrays are required and can handle the allocation. Also, added the 
   geodetic_from_ecef functions, as well as the "_to_" functions to match the "_from_".
   functions.
*/

typedef struct  
{
  char     *gname; /* Name of ellipsoid */
  double   a;     /* semimajor (equatorial) axis in meters */
  double   invf;  /* inverse flattening ratio */
  double   f;     /* flattening ratio */
  double   b;     /* semiminor (polar) axis in meters */
  double   rat2;  /* (b/a)^2 */
  double   e2;    /* eccentricity squared = (1-rat2) */
  double   ep2;   /* eccentricity prime squared = (1/rat2-1) */
} ellipsoid_struct;

#ifdef __cplusplus
extern "C" {
#endif

  int enu_from_ecef(double *ENU, double *T, int n_rows, double *XYZ);    
  int enu_from_geodetic(double *ENU, double *LLH0, int n_rows, double *LLH);
  int ecef_from_geodetic(double *XYZ, int n_rows, double *LLH);
  int geodetic_from_ecef(double *LLH, int n_rows, double *XYZ);

  int enu_T_ecef(double *T, double *LLH0);

  int define_ellipsoid(char *gname, ellipsoid_struct **G_adr);
  int free_ellipsoid(ellipsoid_struct *G);

  /* The following "to" functions mirror the "from" functions above */
  int ecef_to_enu(int n_rows, double *XYZ, double *T, double *ENU);    
  int geodetic_to_enu(int n_rows, double *LLH, double *LLH0, double *XYZ);
  int geodetic_to_ecef(int n_rows, double *LLH, double *XYZ);
  int ecef_to_geodetic(int n_rows, double *XYZ, double *LLH);


#ifdef __cplusplus
}
#endif

#endif


