#ifndef __GEOREFERENCE_CAMERA_H
#define __GEOREFERENCE_CAMERA_H

#ifndef GEOREFERENCE_ECEF 
#define GEOREFERENCE_ECEF 0
#endif

#ifndef GEOREFERENCE_TOD 
#define GEOREFERENCE_TOD 1
#endif

int georeference_camera(double *urange, double *vrange, double *hrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);
int georeference_camera_sv_c(double *urange, double *vrange, double *hrange, double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, double fu, double fv, double q, double u0, double v0, double *kappa, double TDB, double UT1, int *gr_adr, int *gc_adr, double **G_adr);

#endif
