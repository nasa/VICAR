#include <math.h>
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

#define TOLF 1.0e-7
#define MAXITS 2000
#define TOLMIN 1.0e-6
#define TOLX 1.0e-6
#define STPMX 100.0
#define ALF 1.0e-4

static double dmaxarg1, dmaxarg2;
#define DMAX(a, b) (dmaxarg1 = (a), dmaxarg2 = (b),  (dmaxarg1) > (dmaxarg2) ? (dmaxarg1) : (dmaxarg2))

void lnsrch();
double ffmin();
int nn;

void newt( n, check,length, x, x0, y, y0, E, F, G, indx, x_lat, x_long,
         y_lat, y_long, e, f, g, g2, p, xold, xnew, fjac, fvec)
int n;
int *check;
double length;
double *x, *x0, *y, *y0, *E, *F, *G;
int *indx;
double *x_lat, *x_long, *y_lat, *y_long;
double *e, *f, *g;
double *g2, *p, *xold, *xnew, **fjac, *fvec;
{
   int i, its, j;
   double d, den, func, fold, stpmax, sum, temp, test;
   nn = n*2;
   calc_efg(n, length, x, x0, y, y0, e, f, g, x_lat, x_long, y_lat, y_long);
   calc_fvec(n, E, F, G, e, f, g, fvec);
   test=0.0;
   for(i = 1; i<=nn; i++)
   {
      if(fabs(fvec[i]) > test) test = fabs(fvec[i]);
   }
   if(test <0.01*TOLF){
      for(j = 1; j <= n; j++) 
      {
         x[j] = xnew[j]; 
         y[j] = xnew[j + n]; 
      }
      printf("from test is very small\n");
      return;
   }
   for(sum = 0.0, i=1; i <= n; i++) sum +=x[i]*x[i];
   for( i=1; i <= n; i++) sum +=y[i]*y[i];
   stpmax=STPMX*DMAX(sqrt(sum), (double)nn);
   for(its=1; its <=MAXITS; its++){
      calc_efg(n, length, x, x0, y, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      calc_fvec(n, E, F, G, e, f, g, fvec);
      calc_fjac(n, length, E, F, G, e, f, g, x, x_lat, x_long, y_lat, y_long, fjac);
      func=ffmin(nn, fvec);
      for(i =1; i<= nn; i++) {
         for(sum=0.0, j=1; j<=nn; j++){
            sum += fjac[j][i]*fvec[j];
         }
         g2[i] = sum;
      }
      for(i = 1; i <= n; i++){ xnew[i] = x[i];}
      for(i = 1; i <= n; i++){ xnew[i + n] = y[i];}
      for(i = 1; i <= nn; i++){ xold[i] = xnew[i];}
      fold = func;
      for(i=1; i<=nn; i++){
         p[i] = -fvec[i];
/*
         printf(" before lu p = %f i = %d\n", p[i], i);
*/
      }
/*
      for( i = 1; i <= nn; ++i)
      {
         for(j = 1; j <= nn; ++j)
         {
            if(fjac[i][j] != 0)
               printf("*");
            else
               printf("0");
         }
         printf("\n");
      }
         printf("\n");
*/
      ludcmp(fjac, nn, indx, &d);
      lubksb(fjac, nn, indx, p);
/*
for(i = 1; i <= nn; i+=30)
{
      printf(" p = %f i = %d\n", p[i], i);
}
      gaussj(fjac, p, nn);
      sor_test(fjac, p, nn);
for(i = 1; i <= nn; ++i)
{
   if(fabs(p[i]) > 1.0){
    printf("something wrong\n"); exit(1);
   }
 }
*/
      lnsrch(n*2,check,length, xold, fold, g2, p,xnew, &func, stpmax, x, x0,
             y, y0, E, F, G, x_lat, x_long, y_lat, y_long, e, f, g, fvec);
      func = ffmin(n*2, fvec);
/* printf("func = %f\n", func); */
      test = 0.0;
      for(i = 1; i<=nn; i++)
      {
         if(fabs(fvec[i]) > test ) test=fabs(fvec[i]);
      }
      if(test <TOLF) {
         *check=0;
          for(j = 1; j <= n; j++) 
          {
             x[j] = xnew[j]; 
             y[j] = xnew[j + n]; 
          }
          printf("success return\n");
          return;
      }
      if((*check) == 1){
/*
    printf("*check = %d test = %f  hihihihih\n", *check, test);
*/
         test=0.0;
         func = ffmin(n*2, fvec);
         den=DMAX(func, (double)n*0.5);
         for(i =1; i<= n*2; i++) {
            temp=fabs(g2[i])*DMAX(fabs(xnew[i]), 1.0)/den;
            if(temp > test) test=temp;
         }
         *check=(test <TOLMIN ? 1: 0);
          if(*check == 1)
          {
             for(j = 1; j <= n; j++)
             {
                x[j] = xnew[j];
                y[j] = xnew[j + n];
             }
             return;
          }
      }
/*
      for(i = 1; i <= nn; i+=30)
      {
        printf("i = %d fvec = %f\n", i, fvec[i]);
      }
*/
      test=0.0;
      for(i=1; i <= nn; i++) {
         temp=(fabs(xnew[i] - xold[i] ))/DMAX(fabs(xnew[i]), 1.0);
         if(temp > test) test = temp;
      }
      if(test < TOLX){
         for(j = 1; j <= n; j++)
         {
            x[j] = xnew[j];
            y[j] = xnew[j + n];
         }
/*printf("return 3\n");*/
         return;

      } 
  }
  printf("MAXITS exceeded in newt\n");
  exit(1);
}

double ffmin(n, fvec)
int n;
double *fvec;
{
   int i;
   double sum;
   for(sum = 0.0, i = 1; i <=n; i++) sum += fvec[i]*fvec[i];
/*
   printf("fvecsum = %f\n", sum);
*/
   return 0.5*sum;
}

void lnsrch(n,check, length, xold, fold, g2, p, xnew, func, stpmax, x, x0,
             y, y0, E, F, G, x_lat, x_long, y_lat, y_long, e, f, g, fvec)
int n;
int *check;
double length;
double *xold;
double fold, *g2, *p, *xnew, *func, stpmax;
double *x, *x0, *y, *y0, *E, *F, *G, *x_lat, *x_long;
double *y_lat, *y_long, *e, *f, *g;
double *fvec;
{
   int i;
   double a, alam, alam2, alamin, b, disc, f2, fold2, rhs1, rhs2, slope, sum, temp, test, tmplam;
/*
    printf("p=%f i = 2\n", p[2]);
printf("STPMAK %f\n", stpmax);
*/
   *check = 0;
   for(sum = 0.0, i = 1; i <= n; i++) sum +=p[i]*p[i];
   sum=sqrt(sum);
   if(sum > stpmax)
   {
     for (i = 1; i <=n; i++) p[i] *=stpmax/sum;
   }
   for(slope = 0.0, i=1; i<= n; i++)
   {
     slope +=g2[i]*p[i];
   }
   test=0.0;
   for(i = 1; i<=n; i++){
      temp=fabs(p[i])/DMAX(fabs(xold[i]), 1.0);
      if (temp > test ) test=temp;
   }
   alamin=TOLX/test;
/*
printf("alamin = %f test = %f TOLX = %f\n", alamin, test, TOLX);
*/
   if(alamin > 0.0000001) alamin = 0.0000001;
   alam=1.0;
   for(;;){
/*
printf("alam = %f\n", alam);
*/
      for(i = 1; i<= n; i++){ xnew[i]= xold[i] + alam*p[i];}
      for(i = 1; i <= n/2; ++i)
      {
         x[i] = xnew[i];
         y[i] = xnew[i + n/2]; 
      }
      calc_efg(n/2,length, x, x0, y, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      calc_fvec(n/2, E, F, G, e, f, g, fvec);
      *func = ffmin(n, fvec);
/*
         printf(" fvec = %f i = %d p[i] = %falam = %f\n",
          fvec[i], i, p[i], alam);
*/
/*
printf("seond ...func = %f\n", *func);
      for(i = 1; i <= n/2; ++i)
      {
         printf(" x = %f x0 = %f y = %f y0 = %f  i = %d \n",
         x[i], x0[i], y[i], y0[i],  i );
      }
      for(i = 1; i <= n; ++i)
      {
         printf(" fvec = %f i = %d p[i] = %f\n",
          fvec[i], i, p[i]);
      }
exit(1);
*/
      if(alam < alamin){
/*
         for(i = 1; i<= n; i++)  xnew[i] = xold[i];
*/
         *check = 1;
         return;
      }
   else if(*func <= fold+ALF*alam*slope){
/*
      printf("*func = %f fold = %f\n", *func, fold);
*/
      return;
   }
   else {
      if (alam == 1.0)    
          tmplam = -slope/(2.0*(*func-fold-slope));
      else {
          rhs1 = *func-fold-alam*slope;
          rhs2= f2-fold2-alam2*slope;
          a=(rhs1/(alam*alam) -rhs2/(alam2*alam2))/(alam-alam2);
          b=(-alam2*rhs1/(alam*alam)+alam*rhs2/(alam2*alam2))/(alam-alam2);
          if(a == 0.0) tmplam = -slope/(2.0*b);
          else {
              disc= b*b-3.0*a*slope;
              if(disc <0.0) 
              {
                 printf("error exit\n");
                 exit(1);
              } 
              else tmplam = (-b+sqrt(disc))/(3.0*a);
         }
         if(tmplam > 0.5*alam)
            tmplam = 0.5*alam;
      }
    }
    alam2 = alam;
    f2 = *func;
    fold2 = fold;
    alam=DMAX(tmplam, 0.1*alam);
  }
}
