#ifndef __EOS_COORDS_H
#define __EOS_COORDS_H


#ifdef __cplusplus
extern "C" {
#endif

  int eos_coords(double TDB, double UT1, double *J2000_R_TOD, double *ECEF_R_TOD);

#ifdef __cplusplus
}
#endif

#endif


