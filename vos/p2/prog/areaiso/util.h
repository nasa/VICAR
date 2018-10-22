/* util.h */
#ifndef _NR_UTILS_H_
#define _NR_UTILS_H_
 
#ifdef __STDC__
 
void nrerror(char error_text[]);
float *vector(long nl, long nh);
int *ivector(long nl, long nh);
unsigned char *cvector(long nl, long nh);
long *lvector(long nl, long nh);
double *dvector(long nl, long nh);
float **matrix(long nrl, long nrh, long ncl, long nch);
double **dmatrix(long nrl, long nrh, long ncl, long nch);
int  **imatrix(long nrl, long nrh, long ncl, long nch);
char **cmatrix(long nrl, long nrh, long ncl, long nch);
float ***f3tensor(long nrl, long nrh, long ncl, long ndl, long ndl2, long ndh);
int ***i3tensor(long nrl, long nrh, long ncl, long ndl, long ndl2, long ndh);
void free_vector(float *v, long nl, long nh);
void free_ivector(int *v, long nl, long nh);
void free_cvector(unsigned char *v, long nl, long nh);
void free_dvector(double *v, long nl, long nh);
void free_lvector(long *v, long nl, long nh);
void free_matrix(float **m, long nrl, long nrh, long ncl, long nch);
void free_imatrix(int **m, long nrl, long nrh, long ncl, long nch);
void free_dmatrix(double **m, long nrl, long nrh, long ncl, long nch);
void free_lmatrix(long **m, long nrl, long nrh, long ncl, long nch);
void free_cmatrix(char **m, long nrl, long nrh, long ncl, long nch);
void free_f3tensor(float ***t,long  nrl,long  nrh,long  ncl,long  nch,long  ndl, long ndh);
void free_i3tensor(int ***t,long  nrl,long  nrh,long  ncl,long  nch,long  ndl, long ndh);
void rev_short(short int  *shortone);
void rev_long(long *longone);
void rev_double(double *longone);
 
 
#else
 
void nrerror();
float *vector();
int *ivector();
unsigned char *cvector();
long *lvector();
double *dvector();
float **matrix();
double **dmatrix();
int  **imatrix();
char **cmatrix();
void free_vector();
void free_ivector();
void free_cvector();
void free_dvector();
void free_lvector();
void free_matrix();
void free_imatrix();
void free_dmatrix();
void free_lmatrix();
void free_cmatrix();
float ***f3tensor();
int ***i3tensor();
void free_f3tensor();
void free_i3tensor();
void rev_short();
void rev_long();
void rev_double();
 
#endif
 
#endif
#define NR_END 1
#define FREE_ARG char*
