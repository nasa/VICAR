#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "zmabend.h"
#include "zvproto.h"

#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"

/*=====================================================================
dgauss

dgauss solves a set of linear equations via gaussian elimination

arguments:

     1. a: input and output, double *a;
	m by m coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector; output solution.
     3. m: input, int m;
	number of linear equations.
     4. eps: input, double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     5. ierror: output int *ierror;
	result 0=OK, 1=pivot is zero, K=loss of significance warning
	pivot less than eps times max element of a

The matrix a is stored by column order

*/
void dgauss( double * a, double * r, int m, double eps, int * ierror )
{
   double piv,tol,pfac,temp;
   int i=0,j,k,l,n,kt,lt,m2,ll,lu,il;

   if (m<=0) { *ierror = -1; return; }
   *ierror = 0; piv = 0.; m2 = m*m;
   for (j=1;j<=m2;j++)
      {
      temp = a[j-1]; if (temp<0.) temp = -temp;
      if (temp<=piv) continue;
      piv = temp;
      i = j;
      }
   tol = eps*piv;

   ll = 1;
   for (k=1;k<=m;k++)
     {
      if (piv<=0.) { *ierror = -1; return; }
      if (*ierror==0&&piv<tol) *ierror = k-1;
      pfac = 1./a[i-1];
      j = (i-1)/m; i = i-j*m-k; j = j+1-k;
      kt = k+i;
      temp = pfac*r[kt-1];
      r[kt-1] = r[k-1];
      r[k-1] = temp;
      if (k>=m) break;
      lu = ll+m-k;
      if (j>0)
	 {
	 n = j*m;
	 for (l=ll;l<=lu;l++)
	    {
	    temp = a[l-1];
	    lt = l+n;
	    a[l-1] = a[lt-1];
	    a[lt-1] = temp;
	    }
	 }
      for (l=ll;l<=m2;l+=m)
	 {
	 lt = l+i;
	 temp = pfac*a[lt-1];
	 a[lt-1] = a[l-1];
	 a[l-1] = temp;
	 }
      a[ll-1] = (double)j;
      piv = 0.;
      ll = ll+1;
      j = 0;
      for (n=ll;n<=lu;n++)
	 {
	 pfac = -a[n-1];
	 il = n+m;
	 j = j+1;
	 for (l=il;l<=m2;l+=m)
	    {
	    lt = l-j;
	    a[l-1] = a[l-1]+pfac*a[lt-1];
	    temp = a[l-1]; if (temp<0.) temp = -temp;
	    if (temp<=piv) continue;
	    piv = temp;
	    i = l;
	    }
	 kt = k+j;
	 r[kt-1] = r[kt-1]+pfac*r[k-1];
	 }
      ll = ll+m;
      }

   if (m<=0) *ierror = -1;
   if (m<=1) return;
   il = m2+m;
   ll = m+1;
   for (i=2;i<=m;i++)
      {
      n = ll-i;
      il = il-ll;
      l = il-m;
      l = (int)(a[l-1]+.5);
      temp = r[n-1];
      lt = n;
      for (k=il;k<=m2;k+=m)
	 {
	 lt = lt+1;
	 temp = temp-a[k-1]*r[lt-1];
	 }
      k = n+l;
      r[n-1] = r[k-1];
      r[k-1] = temp;
      }
   return;
}

/*=====================================================================
lsqfit

lsqfit solves for the minimum least squares fit (for ax=r, the minimum
over all x of L2 norm of r-ax)

The matrix a is stored by column order

8/2002 alz normalizing columns to 1 in attempt to get cubic to work

arguments:

     1. a: input and output, double *a;
	m by n coefficient matrix, destroyed.
     2. r: input and output, double *r;
	input right hand m-vector.
     3. m: input, int m;
	number of linear equations.
     4. n: input, int n;
	number of independent coords; dimension of x.
     5. x: output, double *x;
	solution vector.
     6. eps: input double eps;
	gaussian pivot tolerance (usually set to 1.e-14)
     7. ierror: output int *ierror;
	result 0=OK; K=singular at kth column
	-1=zero matrix a; -2=m<n

*/

void lsqfit( double * a, double * r, int m, int n, double * x, double eps, int * ierror )
{
   double *buf,*alznorm; int *ipiv;
   int i,j,k,il,iu,kpiv=0,id,jl,ii,kl;
   double piv,h,sig,tol,beta,alzmax;

   if ((buf=(double *)malloc(16*n))==NULL) zmabend("malloc failed");
   if ((ipiv=(int *)malloc(4*n))==NULL) zmabend("malloc failed");
   if ((alznorm=(double *)malloc(8*n))==NULL) zmabend("malloc failed");
   
   /* normalize the columns, then fix solution at end */
   
   for (i=0;i<n;i++)
      {
      alzmax = fabs(a[i*m]);
      for (j=1;j<m;j++) if (fabs(a[i*m+j])>alzmax) alzmax = fabs(a[i*m+j]);
      if (alzmax>0.0) for (j=0;j<m;j++) a[i*m+j] /= alzmax;
      alznorm[i] = alzmax;
      }

   /* end of normalize the columns */

   if (m<n) { *ierror = -2; goto done; }
   piv = 0.;
   iu = 0;
   for (k=1;k<=n;k++)
      {
      ipiv[k-1] = k;
      h = 0.;
      il = iu+1;
      iu = iu+m;
      for (i=il;i<=iu;i++) h = h+a[i-1]*a[i-1];
      buf[k-1] = h;
      if (h<=piv) continue;
      piv = h;
      kpiv = k;
      }
   if (piv<=0.) { *ierror = -1; goto done; }
   sig = sqrt(piv);
   tol = sig*fabs(eps);
   
   il = -m;
   for (k=1;k<=n;k++)
      {
      il = il+m+1;
      iu = il+m-k;
      i = kpiv-k;
      if (i>0)
	 {
	 h = buf[k-1];
	 buf[k-1] = buf[kpiv-1];
	 buf[kpiv-1] = h;
	 id = i*m;
	 for (i=il;i<=iu;i++)
	    {
	    j = i+id;
	    h = a[i-1];
	    a[i-1] = a[j-1];
	    a[j-1] = h;
	    }
	 }
      if (k>1)
	 {
	 sig = 0.;
	 for (i=il;i<=iu;i++) sig = sig+a[i-1]*a[i-1];
	 sig = sqrt((double)sig);
	 if (sig<tol) { *ierror = k-1; goto done; }
	 }
      h = a[il-1];
      if (h<0.) sig = -sig;
      ipiv[kpiv-1] = ipiv[k-1];
      ipiv[k-1] = kpiv;
      beta = h+sig;
      a[il-1] = beta;
      beta = 1./(sig*beta);
      j = n+k;
      buf[j-1] = -sig;
      if (k<n)
	 {
	 piv = 0.;
	 id = 0;
	 jl = k+1;
	 kpiv = jl;
	 for (j=jl;j<=n;j++)
	    {
	    id = id+m;
	    h = 0.;
	    for (i=il;i<=iu;i++)
	       {
	       ii = i+id;
	       h = h+a[i-1]*a[ii-1];
	       }
	    h = beta*h;
	    for (i=il;i<=iu;i++)
	       {
	       ii = i+id;
	       a[ii-1] = a[ii-1]-a[i-1]*h;
	       }
	    ii = il+id;
	    h = buf[j-1]-a[ii-1]*a[ii-1];
	    buf[j-1] = h;
	    if (h<=piv) continue;
	    piv = h;
	    kpiv = j;
	    }
	 }
      h = 0.;
      ii = il;
      for (i=k;i<=m;i++)
	 {
	 h = h+a[ii-1]*r[i-1];
	 ii = ii+1;
	 }
      h = beta*h;
      ii = il;
      for (i=k;i<=m;i++)
	 {
	 r[i-1] = r[i-1]-a[ii-1]*h;
	 ii = ii+1;
	 }
      }

   *ierror = 0;
   piv = 1./buf[2*n-1];
   x[n-1] = piv*r[n-1];
   if (n>1)
      {
      jl = (n-1)*m+n;
      for (j=2;j<=n;j++)
	 {
	 jl = jl-m-1;
	 k = n+n+1-j;
	 piv = 1./buf[k-1];
	 kl = k-n;
	 id = ipiv[kl-1]-kl;
	 il = 2-j;
	 h = r[kl-1];
	 il = il+n;
	 iu = il+j-2;
	 ii = jl;
	 for (i=il;i<=iu;i++)
	    {
	    ii = ii+m;
	    h = h-a[ii-1]*x[i-1];
	    }
	 i = il-1;
	 ii = i+id;
	 x[i-1] = x[ii-1];
	 x[ii-1] = piv*h;
	 }
      }
      
   done:
   free(buf);
   free(ipiv);
   for (i=0;i<n;i++)
      if (x[i]!=0.0)
         {
         for (j=0;j<n;j++) x[j] /= alznorm[j];
         return;
         }
   *ierror = -1;
   return;
}
