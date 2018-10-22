#ifndef __RAY_INTERSECT_ELLIPSOID_H
#define __RAY_INTERSECT_ELLIPSOID_H

int ray_intersect_ellipsoid(double *t, double *d, double *p0, double *SigmaInv, double *p);
int ray_intersect_ca_ellipsoid(double *t, double *d, double *s, double *p);

#endif
