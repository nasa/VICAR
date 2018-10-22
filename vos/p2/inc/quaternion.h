#ifndef __QUATERNION_H
#define __QUATERNION_H

int rodrigues_to_quaternion(double *v, double *q);
int quaternion_to_rodrigues(double *q, double *v);
int quaternion_to_mat(double *q, double *R);
int mat_to_quaternion(double *R, double *q);

#endif
