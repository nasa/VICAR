
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include "util.h"
void rev_short( shortone)
short int *shortone;
{
    struct short_bytes {
          char byte1;
          char byte2;
                    } *shortptr;
    unsigned char temp;
 
    shortptr = (struct short_bytes *) shortone;
    temp = shortptr->byte1;
    shortptr ->byte1 = shortptr->byte2;
    shortptr->byte2 = temp;
}

void rev_long(longone)
long *longone;
{
   struct long_bytes {
            char byte1;
            char byte2;
            char byte3;
            char byte4;
                      } *longptr;
   unsigned char temp;
 
   longptr = (struct long_bytes *) longone;
   temp = longptr->byte1;
   longptr->byte1 = longptr->byte4;
   longptr->byte4 = temp;
   temp = longptr->byte2;
   longptr->byte2 = longptr->byte3;
   longptr->byte3 = temp;
}
void rev_double(doubleone)
double *doubleone;
{
/* The bytes of the double pointed to bu doubleone will be
   revesed. */    
   struct double_bytes {
            char byte1;
            char byte2;
            char byte3;
            char byte4;
            char byte5;
            char byte6;
            char byte7;
            char byte8;
                      } *doubleptr;
   unsigned char temp;
 
   doubleptr = (struct double_bytes *) doubleone;
   temp = doubleptr->byte1;
   doubleptr->byte1 = doubleptr->byte8;
   doubleptr->byte8 = temp;
   temp = doubleptr->byte2;
   doubleptr->byte2 = doubleptr->byte7;
   doubleptr->byte7 = temp;
   temp = doubleptr->byte3;
   doubleptr->byte3 = doubleptr->byte6;
   doubleptr->byte6 = temp;
   temp = doubleptr->byte4;
   doubleptr->byte4 = doubleptr->byte5;
   doubleptr->byte5 = temp;
}




float *vector(nl, nh)
long nl;
long nh;
/*allocate a float vector with subscript range v[nl,`.. nh]*/
{
   float *v;

   v=(float *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(float)));
   if(!v) nrerror("allocation failure in vector()");
   return v-nl+NR_END;
}

int *ivector(nl, nh)
long nl, nh;
/*allocate a int vector with subscript range v[nl,`.. nh]*/
{
   int *v;

   v=(int *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(int)));
   if(!v) nrerror("allocation failure in ivector()");
   return v-nl+NR_END;
}

unsigned char *cvector(nl, nh)
long nl,  nh;
/*allocate a char vector with subscript range v[nl,`.. nh]*/
{
   unsigned char *v;

   v=(unsigned char *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(unsigned char)));
   if(!v) nrerror("allocation failure in cvector()");
   return v-nl+NR_END;
}

long *lvector(nl, nh)
long nl, nh;
/*allocate a long vector with subscript range v[nl,`.. nh]*/
{
   long *v;

   v=(long *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(long)));
   if(!v) nrerror("allocation failure in lvector()");
   return v-nl+NR_END;
}

double *dvector(nl, nh)
long nl, nh;
/*allocate a double vector with subscript range v[nl,`.. nh]*/
{
   double *v;

   v=(double *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(double)));
   if(!v) nrerror("allocation failure in dvector()");
   return v-nl+NR_END;
}

float **matrix(nrl, nrh, ncl, nch)
long nrl, nrh, ncl, nch;
/*allocate a float matrix with subscript range m[nrl .. nrh][ncl..nch]*/
{
   long i, nrow=nrh-nrl+1, ncol = nch-ncl+1;
   float **m;

  /*allocate pointers of rows */
   m=(float **) malloc((size_t)((nrow+NR_END)*sizeof(float*)));
   if(!m) nrerror("allocation failure 1 in matrix()");
   m +=NR_END;
   m -= nrl;

  /*allocate rows and set pointers to them */
  m[nrl]=(float *)malloc((size_t)((nrow*ncol+NR_END)*sizeof(float)));
  if(!m[nrl]) nrerror("allocation failure 2 in matrix()");
   m[nrl] += NR_END;
   m[nrl] -=ncl;

   for(i = nrl + 1; i < nrh; i++)
      m[i] = m[i-1] + ncol;

   /* return pointer ro array of pointers to rows */
   return m;
}

double **dmatrix( nrl, nrh, ncl, nch)
long nrl, nrh, ncl, nch;
/*allocate a double matrix with subscript range m[nrl .. nrh][ncl..nch]*/
{
   long i, nrow=nrh-nrl+1, ncol = nch-ncl+1;
   double **m;

  /*allocate pointers of rows */
   m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double *)));
   if(!m) nrerror("allocation failure 1 in matrix()");
   m +=NR_END;
   m -= nrl;

  /*allocate rows and set pointers to them */
  m[nrl]=(double *)malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
  if(!m[nrl]) nrerror("allocation failure 2 in matrix()");
   m[nrl] += NR_END;
   m[nrl] -=ncl;

   for(i = nrl + 1; i < nrh; i++)
      m[i] = m[i-1] + ncol;

   /* return pointer ro array of pointers to rows */
   return m;
}

int **imatrix(nrl, nrh,ncl, nch)
long nrl, nrh,ncl, nch;
/*allocate a int matrix with subscript range m[nrl .. nrh][ncl..nch]*/
{
   long i, nrow=nrh-nrl+1, ncol = nch-ncl+1;
   int **m;

  /*allocate pointers of rows */
   m=(int **) malloc((size_t)((nrow+NR_END)*sizeof(int *)));
   if(!m) nrerror("allocation failure 1 in matrix()");
   m +=NR_END;
   m -= nrl;

  /*allocate rows and set pointers to them */
  m[nrl]=(int *)malloc((size_t)((nrow*ncol+NR_END)*sizeof(int)));
  if(!m[nrl]) nrerror("allocation failure 2 in matrix()");
   m[nrl] += NR_END;
   m[nrl] -=ncl;

   for(i = nrl + 1; i < nrh; i++)
      m[i] = m[i-1] + ncol;

   /* return pointer ro array of pointers to rows */
   return m;
}

char **cmatrix(nrl, nrh, ncl, nch)
long nrl, nrh, ncl, nch;
/*allocate a int matrix with subscript range m[nrl .. nrh][ncl..nch]*/
{
   long i, nrow=nrh-nrl+1, ncol = nch-ncl+1;
   char **m;

  /*allocate pointers of rows */
   m=(char **) malloc((size_t)((nrow+NR_END)*sizeof(char *)));
   if(!m) nrerror("allocation failure 1 in matrix()");
   m +=NR_END;
   m -= nrl;

  /*allocate rows and set pointers to them */
  m[nrl]=(char *)malloc((size_t)((nrow*ncol+NR_END)*sizeof(char)));
  if(!m[nrl]) nrerror("allocation failure 2 in matrix()");
   m[nrl] += NR_END;
   m[nrl] -=ncl;

   for(i = nrl + 1; i < nrh; i++)
      m[i] = m[i-1] + ncol;

   /* return pointer ro array of pointers to rows */
   return m;
}

void free_vector(v, nl, nh)
float *v;
long nl, nh;
/* free a float vector allocated with vector() */
{
  free((FREE_ARG)(v+nl-NR_END));
}

void free_ivector( v, nl, nh)
int *v;
long nl, nh;
/* free a int vector allocated with vector() */
{
  free((FREE_ARG)(v+nl-NR_END));
}

void free_lvector(v, nl, nh)
long *v; 
long nl, nh;
/* free a float vector allocated with vector() */
{
  free((FREE_ARG)(v+nl-NR_END));
}

void free_dvector(v, nl, nh)
double *v;
long nl, nh;
/* free a float vector allocated with vector() */
{
  free((FREE_ARG)(v+nl-NR_END));
}

void free_cvector(v, nl, nh)
unsigned char *v; 
long nl, nh;
/* free a float vector allocated with vector() */
{
  free((FREE_ARG)(v+nl-NR_END));
}

void free_matrix(m, nrl, nrh, ncl, nch)
float **m;
long nrl, nrh, ncl, nch;
/*free a float matrix allocated by matrix() */
{
   free((FREE_ARG)(m[nrl]+ncl-NR_END));
   free((FREE_ARG)(m+nrl-NR_END));
}

void free_imatrix(m, nrl, nrh, ncl, nch)
int **m;
long nrl, nrh, ncl, nch;
/*free a float matrix allocated by matrix() */
{
   free((FREE_ARG)(m[nrl]+ncl-NR_END));
   free((FREE_ARG)(m+nrl-NR_END));
}

void free_dmatrix(m, nrl, nrh, ncl, nch)
double **m;
long nrl, nrh, ncl, nch;
/*free a float matrix allocated by matrix() */
{
   free((FREE_ARG)(m[nrl]+ncl-NR_END));
   free((FREE_ARG)(m+nrl-NR_END));
}

void free_cmatrix(m, nrl, nrh, ncl, nch)
char **m;
long nrl, nrh, ncl, nch;
/*free a float matrix allocated by matrix() */
{
   free((FREE_ARG)(m[nrl]+ncl-NR_END));
   free((FREE_ARG)(m+nrl-NR_END));
}

void nrerror(error_text)
char error_text[];
{
   fprintf(stderr,"Numberical Recipes run_time error ...\n");
   fprintf(stderr, "%s\n", error_text);
   fprintf(stderr,"...now exiting to system ...\n");
   exit(1);
}

float ***f3tensor(nrl, nrh, ncl, nch, ndl, ndh)
long nrl, nrh, ncl, nch, ndl, ndh;
{
   long i, j, nrow = nrh-nrl+1, ncol=nch-ncl+1, ndep=ndh-ndl+1;
   float ***t;
   /*allocate pointers to point to rows */
   t=(float ***)malloc((size_t)((nrow+NR_END)*sizeof(float**)));
   if(!t) nrerror("allocation failure 1 in f3tensor()");
   t +=NR_END;
   t -= nrl;

/*allocate pointers to rows and set pointers to them */
   t[nrl]=(float **)malloc((size_t)((nrow*ncol+NR_END)*sizeof(float*)));
   if (!t[nrl]) nrerror("allocation failure 2 in f3tensor()");
   t[nrl] +=NR_END;
   t[nrl] -=ncl;

   /*allocate rows and set pointers to them */
   t[nrl][ncl] = (float*) malloc((size_t)((nrow*ncol*ndep+NR_END)*sizeof(float)));
   if(!t[nrl][ncl]) nrerror("allocation failure 3 in f3tensor()");
   t[nrl][ncl] += NR_END;
   t[nrl][ncl] -= ndl;

   for(j=ncl+1; j<=nch; j++) t[nrl][j] = t[nrl][j-1]+ndep;
   for(i=nrl+1; i <=nrh; i++) {
      t[i]=t[i-1] + ncol;
      t[i][ncl]=t[i-1][ncl] +ncol*ndep;
      for(j=ncl+1; j<=nch; j++) t[i][j]=t[i][j-1] +ndep;
   }

   return t;
}

void free_f3tensor(t, nrl, nrh, ncl, nch, ndl, ndh)
float ***t;
long nrl, nrh, ncl, nch, ndl, ndh;
{
   free((FREE_ARG) (t[nrl][ncl] +ndl-NR_END));
   free((FREE_ARG) (t[nrl]+ncl-NR_END));
   free((FREE_ARG) (t+nrl-NR_END));
}

int ***i3tensor(nrl, nrh, ncl, nch, ndl, ndh)
long nrl, nrh, ncl, nch, ndl, ndh;
{
   long i, j, nrow = nrh-nrl+1, ncol=nch-ncl+1, ndep=ndh-ndl+1;
   int ***t;
   /*allocate pointers to point to rows */
   t=(int ***)malloc((size_t)((nrow+NR_END)*sizeof(int**)));
   if(!t) nrerror("allocation failure 1 in f3tensor()");
   t +=NR_END;
   t -= nrl;

/*allocate pointers to rows and set pointers to them */
   t[nrl]=(int **)malloc((size_t)((nrow*ncol+NR_END)*sizeof(int*)));
   if (!t[nrl]) nrerror("allocation failure 2 in f3tensor()");
   t[nrl] +=NR_END;
   t[nrl] -=ncl;

   /*allocate rows and set pointers to them */
   t[nrl][ncl] = (int*) malloc((size_t)((nrow*ncol*ndep+NR_END)*sizeof(int)));
   if(!t[nrl][ncl]) nrerror("allocation failure 3 in f3tensor()");
   t[nrl][ncl] += NR_END;
   t[nrl][ncl] -= ndl;

   for(j=ncl+1; j<=nch; j++) t[nrl][j] = t[nrl][j-1]+ndep;
   for(i=nrl+1; i <=nrh; i++) {
      t[i]=t[i-1] + ncol;
      t[i][ncl]=t[i-1][ncl] +ncol*ndep;
      for(j=ncl+1; j<=nch; j++) t[i][j]=t[i][j-1] +ndep;
   }

   return t;
}

void free_i3tensor(t, nrl, nrh, ncl, nch, ndl, ndh)
int ***t;
long nrl, nrh, ncl, nch, ndl, ndh;
{
   free((FREE_ARG) (t[nrl][ncl] +ndl-NR_END));
   free((FREE_ARG) (t[nrl]+ncl-NR_END));
   free((FREE_ARG) (t+nrl-NR_END));
}
