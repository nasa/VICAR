#ifndef CARTOGRIDUTILS_H
#define CARTOGRIDUTILS_H

extern double gridtol;
void cartogetline(double *x,double*y,
	     int istart, int inc, int nmax, int bign, int* nfound, int* null9);

void getline2(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound);

void getline3(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound);

void gridfill(double *x,double *y, int istart, int inc, int nmax);

void tgrid(int npoints,
	   int* nlinret,
	   double* ptx,
	   double* pty,
	   int* ntriang,
	   int** tcon1,
	   int** tcon2,
	   int** tcon3,
	   int gridnah,
	   double* csol,
	   double* optx,
	   double* opty,
	   int zgeom);

#endif
