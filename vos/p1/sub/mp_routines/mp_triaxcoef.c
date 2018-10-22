/* rewritten in C
   Thomas Roatsch, DLR, March 2001
   removed nlimit, mlimit,klimit from parameter list,
   it was already defined in mp_routines.h */
   
/*  subroutine triaxcoef

  Function extracted from program TRICOEF.COM by Jean Lorre.

	Date extracted:		October 1993
	Extracted by:		Justin McNeill

  This has been modified to now include both authali  as well as conformal
  auxiallary coefficient functionality. Code was adopted from Jean Lorre's
  tricoef routine. Recent changes also made in the randum number generator
  function, to 'rangen', to correct for errors occurring on the sgi system
  with the older 'ran1' function   -  pxa  august '96

  oct96 -lwk- hard-coded the random-number seed in order to allow comparison
		of test logs;  this will be replaced in the future by 
		allowing the user to specify this (as well as the 3 limits),
		which will require a new routine to give the user access

  jun98 -lwk- replaced rangen with modified version of RAN1 because of 
		problems on DEC-Unix  */

#include <math.h>
#include "zvproto.h"
#include "mp_private.h"

#define maxpts 800
#define maxeqs 400
#define latpts 72
#define lonpts 72

/* was Fortran common block */
static double a,b,c;
static double lat[maxpts],lon[maxpts];
static double costheta[maxpts],sintheta[maxpts],f[maxpts];

/* ran1 stuff */
#define randim 97
static int    ix1,ix2,ix3;
static double r[randim];

/*********************************************************************/
double ran1( int *idum)
{
/* Returns random number between 0.0 and 1.0. To initialize provide
   negative idum value.*/
#define m1  259200
#define ia1 7141
#define ic1 54773
#define m2  134456
#define ia2 8121
#define ic2 28411
#define m3  243000
#define ia3 4561
#define ic3 51349

int j;
double help;
double rm1,rm2;
rm1 = 1.0/m1;
rm2= 1.0/m2;

if (*idum != 1)
   {
   ix1 = (ic1 - *idum) % m1;
   ix1 = (ia1 * ix1 + ic1) % m1;
   ix2 = ix1 % m2;
   ix1 = (ia1 * ix1 + ic1) % m1;
   ix3 = ix1 % m3;
   for (j=0; j<randim; j++)
      {
      ix1 = (ia1 * ix1 + ic1) % m1;
      ix2 = (ia2 * ix2 + ic2) % m2;
      r[j] = ( (double) ix1 + ( (double) ix2) * rm2 ) * rm1;
      }
   *idum=1;
   }
ix1 = ( ia1 * ix1 + ic1) % m1;
ix2 = ( ia2 * ix2 + ic2) % m2;
ix3 = ( ia3 * ix3 + ic3) % m3;
j   = 1 + (randim * ix3)/m3;
if(j > randim) j=randim;
if(j < 1) j=1;
help = r[j-1];
r[j-1] = ( (double) ix1 + ( (double) ix2) * rm2 ) * rm1;

return help;
}
/*********************************************************************/
double p1(int m, int n, int i)      /* equation 76 */
{
double help;
help = 2.0*a*m * sintheta[i] * cos(n*lat[i]) * cos(2.0*m*lon[i]);
return help;
}

double p2(int m, int n, int i)      /* equation 77 */
{
double help;
help = -2.0*a*m * costheta[i] * sin(n*lat[i])* sin(2.0*m*lon[i]) -
       a*n*f[i] * cos(n*lat[i]) * cos(2.0*m*lon[i]);
return help;
}

double p3(int m, int n, int i)       /* equation 78 */
{
double help;
help = -2.0*a*m * cos(n*lat[i]) * cos(2.0*m*lon[i]) -
       a*n*f[i] * sin(n*lat[i]) *sin(2.0*m*lon[i]) * costheta[i];
return help;
}

double p4(int m, int n, int i)       /* equation 79 */
{
double help;
help = a*n*f[i] * cos(n*lat[i]) * cos(2.0*m*lon[i])* sintheta[i];
return help;
}
/*********************************************************************/
int mp_dsimq(double *am, double *bm, int n)

/*        PURPOSE
          OBTAIN SOLUTION OF A SET OF SIMULTANEOUS LINEAR EQUATIONS,
          AX=B

       DESCRIPTION OF PARAMETERS
          AM - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
               N BY N.
          BM  - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
              REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
          N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.

          return:
               0 FOR A NORMAL SOLUTION
               1 FOR A SINGULAR SET OF EQUATIONS */
{
double biga,save;
int    ks;
int    j,jj,jy,i,ij,it,imax,i1,i2,k;
int    iqs,ix,ixj,jx,ixjx,jjx;
int    ia,ib,ic;

/* FORWARD SOLUTION */
ks=0;
jj=-n;

for (j=0; j<n; j++)
   {
   jy=j+1;
   jj=jj+n+1;
   biga=0.0;
   it=jj-j;
   
   for (i=j-1; i<n-1; i++)
      {
      /* SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN */
      ij=it+i;
      if (fabs(am[ij]) > fabs(biga))
         {
         biga = am[ij];
         imax=i+1;
         }
      }
/* TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX) */
   if(biga == 0) return 1;

/* INTERCHANGE ROWS IF NECESSARY */
   i1=j+n*(j-1);
   it=imax-j;
   for (k=j-1; k<n-1; k++)
      {
      i1=i1+n;
      i2=i1+it;
      save=am[i1];
      am[i1]=am[i2];
      am[i2]=save;
      /* DIVIDE EQUATION BY LEADING COEFFICIENT */
      am[i1]=am[i1]/biga;
      }
   save=bm[imax];
   bm[imax]=bm[j];
   bm[j]=save/biga;

/* ELIMINATE NEXT VARIABLE */
   if (j == n-1) goto go_70; 
   iqs=n*j;
   for (ix=jy; ix<n; ix++)
      {
      ixj=iqs+ix;
      it=j-ix;
      for (jx=jy; jx<n; jx++)
         {
         ixjx=n*jx+ix;
         jjx=ixjx+it;
         am[ixjx]=am[ixjx] - am[ixj] * am[jjx];
         }
      bm[ix] = bm[ix] - bm[j] * am[ixj];
      }
   }

/* BACK SOLUTION */
      
go_70:
it=n*n-1;
for (j=0; j<n-1; j++)
   {
   ia=it-j-1;
   ib=n-j-2;
   ic=n-1;
   for (k=0; k<=j; k++)
      {
      bm[ib] = bm[ib] - am[ia] * bm[ic];
      ia=ia-n;
      ic=ic-1;
      }
   }

return 0;
}
/*********************************************************************/
double simp1(double areafun[lonpts+1][latpts+1],
           int i, int lonlmt)
 
/* Compute elements for Simpsons integration. */
{
double sum1;
int j;

sum1=areafun[0][i]+areafun[lonlmt][i];
for (j=1; j<=lonlmt-1;j=j+2)
   sum1=sum1+4.0*areafun[j][i];
for (j=2; j<=lonlmt-2; j=j+2)
   sum1=sum1+2.0*areafun[j][i];
sum1=sum1*PI_OVER_2/(lonpts*3);
return sum1;
}
/*********************************************************************/
double simp2(int latlmt, double c, double elllat[lonpts+1],
             double areafun[lonpts+1][latpts+1])
 
/* Computes areas of zones */
{
double fsimlon, sum2;
int i;

i=0;
sum2 = simp1(areafun,i,lonpts);
for (i=1; i <= latlmt-1; i=i+2)
   {
   fsimlon = simp1(areafun,i,lonpts);
   sum2 = sum2 + 4.0*fsimlon;
   }
for (i=2; i<=latlmt-2; i=i+2)
   {
   fsimlon = simp1(areafun,i,lonpts);
   sum2=sum2+2.0*fsimlon;
   }
i=latlmt;
fsimlon = simp1(areafun,i,lonpts);
sum2=sum2+fsimlon;
sum2=c*c*sum2*elllat[latlmt]/(latlmt*3);
return sum2;
}
/*********************************************************************/
void authalic_coef(double coef[KLIMIT+1][MLIMIT+1],
                   double coefp[NLIMIT])
 
/* To compute coefficients permitting the snyder to authalic computation */
{ 
double elllat[latpts+1],elllon[lonpts+1];
double areafun [lonpts+1][latpts+1];
double acoef[MLIMIT+1][latpts+1];
double autlat[latpts+1];
double autlon[lonpts+1][latpts+1];
double cb2,ba2;
int    i,j,k,m,n;
double clon,clat,slon,slat,flon,flat;
double t1,t2,t3,sinthet,costhet;
double area,tarea,sphiaut,sum1,autfunc,tcirc,fsimlon;
int    latlmt,lonlmt;

for (i=0; i<=latpts; i++)
   for (j=0; j<=MLIMIT; j++)
      acoef[j][i]=0;
for (i=0;i<=latpts; i++)
   for (j=0; j<=lonpts; j++)
      autlon[j][i]=0;
for (i=0; i<=MLIMIT; i++)
   for (j=0; j<=KLIMIT; j++)
      coef[j][i]=0;
for (i=0; i<=latpts; i++)
   autlat[i]=0;
for (i=0; i<NLIMIT; i++)
   coefp[i]=0;

ba2=b*b;
cb2=c*c/ba2;
 
for (i=0; i<latpts; i++)
   elllat[i]=i*PI_OVER_2/latpts;
elllat[latpts]=(latpts-0.001)*PI_OVER_2/latpts;
for (j=0; j<=lonpts; j++)
   elllon[j]=j*PI_OVER_2/lonpts;

/* compute area function for integration */
for (i=0; i<=latpts; i++)
   for (j=0; j<=lonpts; j++)
      {
      slon=sin(elllon[j]);
      clon=cos(elllon[j]);
      slat=sin(elllat[i]);
      clat=cos(elllat[i]);
      flon=slon*slon+ba2*clon*clon;       /* eqn 9 */
      flat=slat*slat+cb2*clat*clat;       /* eqn 10 */
      t1=(1.0-ba2)*slon*clon*slat;
      t2=sqrt(slat*slat+cb2*cb2*flon*clat*clat);
      t3=sqrt(slon*slon+ba2*ba2*clon*clon);
      costhet=t1/(t2*t3);                /*eqn 48 */
      sinthet=sqrt(fabs(1.0-costhet*costhet));
      areafun[j][i]=clat*t3*t2*sinthet/(flon*flon*flat*flat);
      }
/* compute total area of triaxial ellipsoid */
tarea = simp2(latpts,c, elllat, areafun);

/* compute authalic latitudes */
latlmt=0;
autlat[0]=0.0;
for (latlmt=6; latlmt<=latpts; latlmt=latlmt+6)
   {
   area = simp2(latlmt,c, elllat, areafun);
   sphiaut=area/tarea;
   if(fabs(sphiaut) > 0.9999999) autlat[latlmt]=PI_OVER_2;
   else
      autlat[latlmt]=
         atan(sphiaut/sqrt(fabs(1.0-sphiaut*sphiaut)));
   }
    
/* compute coefficients for authalic latitude using simpson integration */
for (n=1; n<=NLIMIT; n++)
   {
   sum1=0.0;
   for (i=6; i<=latpts-6; i=i+12)
      {
      autfunc=(autlat[i]-elllat[i])*sin(2*n*elllat[i]);
      sum1=sum1+4.0*autfunc;
      }
   for (i=12; i<=latpts-12; i=i+12)
      {
      autfunc=(autlat[i]-elllat[i])*sin(2*n*elllat[i]);
      sum1=sum1+2.0*autfunc;
      }
   coefp[n-1]=4.0*sum1/latpts;
   }

/* compute authalic longitude */
for (i=0; i<=latpts; i=i+6)
   {
   /* first compute area of zone of quadrant along latitude */
   tcirc = simp1(areafun,i,lonpts);
   /* now compute portion of zone for each step of longitude */
   autlon[0][i]=0.0;
   for (lonlmt=6; lonlmt<=lonpts; lonlmt=lonlmt+6)
      {
      fsimlon = simp1(areafun,i,lonlmt);
      autlon[lonlmt][i]=(fsimlon/tcirc)*PI_OVER_2;
      }
   }   
 
/* compute elements for simpson integration for acoef of authalic longitude */
for (m=0; m<=MLIMIT; m++)
   for (i=0; i<=latpts; i=i+6)
      {
      sum1=0.0;
      for (j=6; j<=lonpts-6; j=j+12)
         {
         autfunc=(autlon[j][i]-elllon[j])*sin(2*m*elllon[j]);
         sum1=sum1+4.0*autfunc;
         }
      for (j=12; j<=lonpts-12; j=j+12)
         {
         autfunc=(autlon[j][i]-elllon[j])*sin(2*m*elllon[j]);
         sum1=sum1+2.0*autfunc;
         }
      acoef[m][i]=4.0*sum1/lonpts;
      }
 
/* compute elements for simpson integration for C coef of authalic longitude */
for (m=0; m<=MLIMIT; m++)
   {
   k=0;
   sum1=acoef[m][0];
   for (j=6; j<=latpts-6; j=j+12) sum1=sum1+4.0*acoef[m][j];
   for (j=12; j<=latpts-12; j=j+12) sum1=sum1+2.0*acoef[m][j];
   sum1=sum1+acoef[m][latpts];
   coef[0][m]=2.0*sum1/latpts;
   }
 
for (m=1; m<=MLIMIT; m++)
   for (k=1; k<=KLIMIT; k++)
      {
      sum1=acoef[m][0];
      for (j=6; j<=latpts-6; j=j+12)
         {
         autfunc=acoef[m][j]*cos(2*k*elllat[j]);
         sum1=sum1+4.0*autfunc;
         }
      for (j=12; j<=latpts-12; j=j+12)
         {
         autfunc=acoef[m][j]*cos(2*k*elllat[j]);
         sum1=sum1+2.0*autfunc;
         }
      sum1=sum1+acoef[m][latpts]*cos(2*k*elllat[latpts]);
      coef[k][m]=4.0*sum1/latpts;
      } 
return;
}
/*********************************************************************/
void comp_points(int npoints, int *idum)
{
double degtorad,rannum;
int    i;
double slon,clon,slat,clat,flon,flat,t1,t2,t3;

degtorad=atan(1.0)/45.0;
for (i=0; i< npoints; i++)
    {
    rannum = ran1(idum);
    lat[i]=degtorad*85.*rannum;
    rannum = ran1(idum);
    lon[i]=degtorad*90.*rannum;
    }
    
/* compute & save values unique to each point.
   removed division by a since it is 1 */
for (i=0; i< npoints; i++)
   {
   slon = sin(lon[i]);
   clon = cos(lon[i]);
   slat = sin(lat[i]);
   clat = cos(lat[i]);
   flon = slon*slon + b*b * clon*clon;    /* eqn 9 */
   flat = slat*slat + c*c/b/b * clat*clat;    /* eqn 10 */
   t1   = (1.0-b*b) *slon * clon * slat;
   t2   = slat*slat + pow(c,4.0)/pow(b,4.0) * flon * clat*clat;
   t3   = slon*slon + pow(b,4.0) * clon*clon;
   costheta[i] = t1 / sqrt(t2) / sqrt(t3);  /* eqn 48 */
   sintheta[i] = sin(acos(costheta[i]));
   f[i] = cos(lat[i]) * sqrt(t3) * flat / (flon*sqrt(t2)); /* eqn 50 */
   }

}
/*********************************************************************/
 


int mp_triaxcoef( double radii[3], double cc[MLIMIT][NLIMIT],
                  double cp[MLIMIT][NLIMIT], double ac[KLIMIT+1][MLIMIT+1],
                  double ap[NLIMIT])

{

int    status;
int    i,ii,ip,iq,j,m,n,k;
double amatrix[maxeqs*maxeqs],bvector[maxeqs];

int    nequations,npoints,idum;
double t;

a=radii[0];
b=radii[1];
c=radii[2];
if ( a == 0)
   {
   zvmessage("mp_triaxcoef: radius[0] is zero","");
   return -401;
   } 

/* disable this check  -lwk- */
/*
if((c >=b) || (b >=a ))
   {
   zvmessage("mp_triaxcoef: Radii out of order, should be decreasing","");
   zvmessage("Oblate spheroids not supported","");
   return -402;
   }
*/
c=c/a;
b=b/a;
a=1.0;

nequations=NLIMIT*MLIMIT*2;
if(nequations > maxeqs)
   {
   zvmessage("mp_triaxcoef: Too many equations requested","");
   return -403;
   }

npoints=nequations*2;          /* npoints > nequations/2 (Snyder) */
if(npoints > maxpts) npoints=maxpts;

/* compute the fit points from a random grid in the range lat=0 to 85
   long=0 to 90. */
idum=-768825576;		/* establish a seed for random numbers */
comp_points(npoints, &idum);
   
/* load matrices to do the linear fit. eqn 80 */
ip=1;
iq=0;
for (i=0; i<MLIMIT*NLIMIT; i++)       /* first 1/2 of equations loop */
   {
   j=i;
   m=1;
   n=0;
   for (k=0; k<MLIMIT*NLIMIT; k++)    /* first 1/2 of terms in equation */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t  +p1(m,n,ii) * p1(ip,iq,ii) + p3(m,n,ii) * p3(ip,iq,ii);
      if(n == NLIMIT-1)
         {
         n=0;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   m=0;
   n=1;
   for (k=MLIMIT*NLIMIT; k<nequations; k++)  /* second 1/2 of terms */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t + p2(m,n,ii) * p1(ip,iq,ii) + p4(m,n,ii) * p3(ip,iq,ii);
      if(n == NLIMIT)
         {
         n=1;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   t=0.0;
   for (ii=0; ii<npoints; ii++)
      t=t + (f[ii]/cos(lat[ii]) - sintheta[ii]) * p1(ip,iq,ii) +
        (1.0-f[ii]*sintheta[ii]/cos(lat[ii])) * p3(ip,iq,ii);

    bvector[i]=t;
    if(iq == NLIMIT-1)
       {
       iq=0;
       ip=ip+1;
       }
       else iq=iq+1;

   } /* end of  first 1/2 of equations loop */

ip=0;
iq=1;
for (i=MLIMIT*NLIMIT; i<nequations; i++)  /* second 1/2 of equations loop */
   {
   j=i;
   m=1;
   n=0;
   for (k=0; k<MLIMIT*NLIMIT; k++)    /* first 1/2 of terms in equation */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
         t=t + p1(m,n,ii) * p2(ip,iq,ii) + p3(m,n,ii) * p4(ip,iq,ii);
      if(n == NLIMIT-1)
         {
         n=0;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   m=0;
   n=1;
   for (k=MLIMIT*NLIMIT; k<nequations; k++)  /* second 1/2 of terms */
      {
      t=0.0;
      for (ii=0; ii<npoints; ii++)
          t=t + p2(m,n,ii) * p2(ip,iq,ii) + p4(m,n,ii) * p4(ip,iq,ii);
      if(n == NLIMIT)
         {
         n=1;
         m=m+1;
         }
      else n=n+1;
      amatrix[j]=t;
      j=j+nequations;
      }

   t=0.0;
   for (ii=0; ii<npoints; ii++)
      t=t + (f[ii]/cos(lat[ii]) - sintheta[ii]) * p2(ip,iq,ii) +
        (1.0-f[ii]*sintheta[ii]/cos(lat[ii])) * p4(ip,iq,ii);
    bvector[i]=t;
    if(iq == NLIMIT)
       {
       iq=1;
       ip=ip+1;
       }
       else iq=iq+1;

   } /* end of  second 1/2 of equations loop */



/* perform the solution.*/
status = mp_dsimq(amatrix,bvector,nequations);
if (status != 0) return status;

/* Recompute NPOINTS random points for RMS error computation.
   The same points won't do since we've fitted to them. */
comp_points(npoints, &idum);

k=0;
m=0;
for (i=0;i<NLIMIT; i++)
   {
   k=m*NLIMIT;
   for (j=0; j<MLIMIT; j++)
      {
      cc[j][i]=bvector[j+k];
      cp[j][i]=bvector[NLIMIT*MLIMIT+j+k];
      }
   m=m+1;
   }

/* compute authalic coefficients : authcoef1 & 2 */
authalic_coef(ac, ap);

return 0;
}
