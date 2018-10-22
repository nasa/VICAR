
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

void spline(x, y,  n, yp1,ypn,  y2)
double x[], y[];
int n;
double yp1,  ypn, y2[];
{
  int i, k;
   double p, qn, sig, un, *u;

   u = (double *)malloc(n*sizeof(double));
   if(yp1 > 0.99e30)
      y2[1] = u[1] = 0.0;
   else
   {
      y2[1] = -0.5;
      u[1] = (3.0/(x[2] - x[1]))*((y[2] - y[1])/(x[2] - x[1]) - yp1);
   }
   for ( i = 2; i <= n-1; i++) {
      sig=(x[i] -x[i-1])/(x[i+1] - x[i-1]);
      p=sig*y2[i-1]+2.0;
      y2[i]=(sig-1.0)/p;
      u[i]=(y[i+1]-y[i])/(x[i+1]-x[i]) -(y[i]-y[i-1])/(x[i]-x[i-1]);
      u[i]=(6.0*u[i]/(x[i+1] -x[i-1]) -sig*u[i-1])/p;
   }
   if (ypn > 0.99e30)
      qn= un = 0.0;
   else {
     qn= 0.5;
     un=(3.0/(x[n] -x[n-1]))*(ypn-(y[n]-y[n-1])/(x[n]-x[n-1]));
   }
   y2[n]=(un-qn*u[n-1])/(qn*y2[n-1] + 1.0);
   for(k= n-1; k >= 1; k--)
      y2[k] = y2[k]*y2[k+1]+ u[k];
   free(u);
}

void splint(xa, ya, y2a, n, x, y)
double xa[], ya[], y2a[];
int n;
double x, *y;
{
   int klo, khi, k;
   double h, b, a;

   klo = 1;
   khi = n;
   while (khi-klo > 1) {
       k= (khi+ klo) >>1;
       if (xa[k] > x) khi = k;
       else klo = k;
   }
   h=xa[khi]-xa[klo];
   if ( h == 0.0) {
     printf("bad xa input\n");
     exit(1);
   }
   a=(xa[khi]-x)/h;
   b=(x-xa[klo])/h;
   y[1] = a*ya[klo] + b*ya[khi] + ((a*a*a - a)*
        y2a[klo]+(b*b*b-b)*y2a[khi])*(h*h)/6.0;
   y[2] = (ya[khi] - ya[klo])/(xa[khi] - xa[klo]) -
          (3.0*a*a - 1.0)/6.0*h* y2a[klo] +
          (3.0*b*b - 1.0)/6.0*h* y2a[khi];
   y[3] = a*y2a[klo] + b*y2a[khi];
}

void int_zxy(xa,xtmp1, n, x, y)
double xa[], xtmp1[];
int n;
double x, *y;
{
   int klo, khi, k;
   double h, b, a;

   klo = 1;
   khi = n;
   while (khi-klo > 1) {
       k= (khi+ klo) >>1;
       if (xa[k] > x) khi = k;
       else klo = k;
   }
   h=xa[khi]-xa[klo];
   if ( h == 0.0) {
     printf("bad xa input\n");
     exit(1);
   }
   *y = (xtmp1[khi] - xtmp1[klo])/h;
}


/*
void splie2(x1a, x2a, ya, m, n,y2a, y2b)
float x1a[], x2a[], **ya;
int m,  n;
float **y2a, **y2b;
{
   int i, j;
   float *ytmp, *y2tmp;
   ytmp = (float *)malloc(sizeof(float)*(m + 2));
   y2tmp = (float *)malloc(sizeof(float)*(m + 2));
   for(j=1; j<=m; ++j)
   {
       spline(x1a, ya[j], n,1.0e30, 1.0e30, y2a[j]);
       printf("m = %dm = %d \n", j, m);
   }
   for(j=1; j<n; ++j)
   {
       printf("m = %d n = %d\n", j, n);
       for(i = 1; i <= m; ++i)
          ytmp[i] = ya[i][j];
       spline(x2a, ytmp, m,1.0e30, 1.0e30, y2tmp);
       for(i = 1; i <= m; ++i)
          y2b[i][j] = y2tmp[i];
   }
   free(ytmp);
   free(y2tmp);
}
void splin2( x1a, x2a, ya, y2a, y2b,  m, n, x1, x2, y)
float x1a[], x2a[], **ya, **y2a, **y2b;
int m,  n;
float x1, x2, *y;
{
   int i, j;
   float z[4];
   float *ytmp, *yytmp;
   float *xtmp, *x2tmp, *x2tmp1;

   ytmp=vector(1,m);
   yytmp=vector(1,m);
   xtmp=vector(1,n);
   x2tmp=vector(1,n);
   x2tmp1=vector(1,n);

   for(j = 1; j<=m; j++)
   {
      splint(x2a, ya[j], y2a[j], n, x2, z);
      yytmp[j] = z[1];
   } 
   spline(x1a, yytmp, m, 1.0e30, 1.0e30, ytmp);
   splint(x1a, yytmp, ytmp, m, x1, z);
   printf("z = %f %f %f\n", z[1], z[2], z[3]);
    
   free_vector(yytmp, 1, m);
   free_vector(ytmp, 1, m);
  
   for(j = 1; j<=n; j++)
   {
      for(i = 1; i <= m; ++i)
      {
          xtmp[i] = ya[i][j];
          x2tmp[i] = y2b[i][j];
      }
      splint(x2a, xtmp, x2tmp, n, x2, z );
      x2tmp1[j] = z[1];
      spline(x2a, xtmp, m,1.0e30, 1.0e30, x2tmp);
      splint(x2a, xtmp, x2tmp, m, x2, z);
      printf("z = %f %f %f\n", z[1], z[2], z[3]);
      free_vector(x2tmp, 1, n);
      free_vector(x2tmp1, 1, n);
      free_vector(xtmp, 1, n);
   }
}
*/
void splin2( x, y, z, z2x, z2y, cols, rows, x1, y1, zx, zy, zxy)
double x[], y[], **z, **z2x, **z2y;
int cols,  rows;
double x1, y1, zx[4], zy[4];
double *zxy;
{
   int i, j;
   double *ytmp, *y2tmp;
   double *xtmp, *x2tmp, *x2tmp1, *xtmp1;
   double ztmp[4];

   ytmp=(double *)malloc((rows+1)*sizeof(double));
   y2tmp=(double *)malloc((rows+1)*sizeof(double));
   xtmp=(double *)malloc((cols+1)*sizeof(double));
   xtmp1=(double *)malloc((cols+1)*sizeof(double));
   x2tmp=(double *)malloc((cols+1)*sizeof(double));
   x2tmp1=(double *)malloc((cols+1)*sizeof(double));

   for(j = 1; j<=rows; j++)
   {
      splint(x, z[j], z2x[j], cols, x1, ztmp);
      ytmp[j] = ztmp[1];
   } 
   spline(y, ytmp, rows, 1.0e30, 1.0e30, y2tmp);
   splint(y, ytmp, y2tmp, rows, y1, zy);
    
  
   for(j = 1; j<=cols; j++)
   {
      for(i = 1; i <= rows; ++i)
      {
          ytmp[i] = z[i][j];
          y2tmp[i] = z2y[i][j];
      }
      splint(y, ytmp, y2tmp, rows, y1, ztmp );
      xtmp[j] = ztmp[1];
      xtmp1[j] = ztmp[2];
   }
      spline(x, xtmp, cols,1.0e30, 1.0e30, x2tmp);
      splint(x, xtmp, x2tmp, cols, x1, zx);
      int_zxy(x, xtmp1,  cols, x1, zxy);
      free(x2tmp);
      free(x2tmp1);
      free(xtmp);
      free(xtmp1);
      free(ytmp);
      free(y2tmp);
}
void splie2(x, y, z, cols, rows,z2x, z2y)
double  x[], y[], **z;
int cols,  rows;   
double **z2x, **z2y;
{
   int i, j;
   double *ytmp, *y2tmp;
   ytmp = (double *)malloc(sizeof(double)*(rows + 2));
   y2tmp = (double *)malloc(sizeof(double)*(rows + 2));
   for(j=1; j<=rows; ++j)
   {
       spline(x, z[j], cols,1.0e30, 1.0e30, z2x[j]);
   }
   for(j=1; j<=cols; ++j)
   {
       for(i = 1; i <= rows; ++i)
          ytmp[i] = z[i][j];
       spline(y, ytmp, rows,1.0e30, 1.0e30, y2tmp);
       for(i = 1; i <= rows; ++i)
          z2y[i][j] = y2tmp[i];
   }
   free(ytmp);
   free(y2tmp);
}
