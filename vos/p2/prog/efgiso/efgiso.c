/* efgiso */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
/* #include "util.h" */
#include "vicmain_c"

#define INTERVAL 5.0
#define STEP_LEGHTH 1.0
#define PI 3.14159265359
#define record_length 2000
#define NR_END 1
 
void calc_derivatives();
void calc_fund_forms();
void int_zxy();
void splie2();
void splin2();
double **dmatrix();

void main44(void)
{

   FILE *IN;
   int i, j, k, l, m, n;
   double x, y, zx[4], zy[4];
   double e[4], f[4], g[4];
   double xr, yr;
   double dx[4], dy[4], dz[4];
   double d2x[4], d2y[4], d2z[4];
   double dxt[4], dyt[4], dzt[4], r;
   double zxy;
   double **xm, **ym;
   double rst[5];
   double *lat, *lng, **radius, **radius2, **r2long, **r2lat;
   int num_cols, num_rows;
   int map_num_cols, map_num_rows;
   float  xtmp,ytmp,ztmp;
   double gam[3][4];
 
   char planet_name[30],path_name[80],filename[80];
   char msg[80];
   int status,count,def,line,sample,nlw,nsw;
   int ounit1,ounit2,ounit3,ounit4,nl,ns,triaxial_count;
   float Ebuf[record_length],Fbuf[record_length],Gbuf[record_length];
   float Rbuf[record_length];
   double latitude,longitude,t,triaxial[3];
   double coslon2,coslat2,sinlat2,sum_r;

/* vicar parameters */
   status=zveaction("AS","  ");
   status=zvparm("PATH",path_name,&count,&def,1,0);
   status=zvparm("PLANET",planet_name,&count,&def,1,0);
   status=zvparm("NL",&nl,&count,&def,1,0);
   status=zvparm("NS",&ns,&count,&def,1,0);
   status=zvparm("NLW",&nlw,&count,&def,1,0);
   status=zvparm("NSW",&nsw,&count,&def,1,0);
   status=zvparmd("TRIAXIAL",triaxial,&triaxial_count,&def,3,0);
   if(triaxial_count == 3){
     sprintf(msg,"Triaxial radii: %f %f %f",triaxial[0],
      triaxial[1],triaxial[2]);
     zvmessage(msg," ");
   }

/* create ascii model filename */
   strcpy(filename,path_name);
   strcat(filename,"/");
   strcat(filename,planet_name);
   strcat(filename,".model");
   zvmessage(filename," ");

   num_cols = 360.0/INTERVAL + 3;/*extends one more columun on both ends*/
   num_rows = 180.0/INTERVAL + 3;/*enxtend one more row on both end*/  
   sprintf(msg,"Model file: #rows= %d, #cols=%d",num_rows,num_cols);
   zvmessage(msg," ");

   lat = (double *)malloc((num_rows + 2)*sizeof(double)); 
   lng = (double *)malloc((num_cols + 2)*sizeof(double)); 
   radius = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   radius2 = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   r2lat = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   r2long = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   x = -95.0; 
   for(i = 1; i <= num_rows; ++i)
   {
      lat[i] = x;
      x += INTERVAL;
   }
   for(i = 1; i <= num_rows; ++i)
   {
      lat[i] = lat[i]*PI/180;
   }
   x = -5.0;
   for(i = 1; i <= num_cols; ++i)
   {
      lng[i] = x;
      x += INTERVAL;
   }
   for(i = 1; i <= num_cols; ++i)
   {
      lng[i] = lng[i]*PI/180;
   }
   if((IN = fopen(filename, "r")) == NULL){
     fprintf(stderr, "Error opening file %s\n", filename);
     zabend();
   }
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);

   for(i = 2; i < num_cols; ++i)       /*centric  east longitude */
   {
      for(j = 2; j < num_rows; ++j)    /* centric latitude */
      {
         if((fgets(msg, sizeof(msg), IN)) == NULL){
           fprintf(stderr, "Error reading file %s\n", filename);
           zabend();
         }
         sscanf(msg,"%f, %f, %f\n", &xtmp, &ytmp, &ztmp);
         xtmp = xtmp*PI/180.;
         ytmp = ytmp*PI/180.;
         if((fabs(lng[i]-xtmp) > .0001) || (fabs(lat[j]-ytmp) > .0001)){
           printf("%f %f \n",lng[i],lat[j]);
           printf("%f %f %f \n",xtmp,ytmp,ztmp);
           zvmessage("Incompatible model file"," ");
           zabend();
         }

         /*radius[j][i] = ztmp; if input were east longitude*/
         radius[j][num_cols-i+1] = ztmp; /* but input is west longitude*/
      }
   }

/* smooth the iso */
   if(nlw+nsw > 2){
     printf("nlw=%d nsw=%d\n",nlw,nsw);
     for(i = 2; i < num_cols; ++i){       /*centric  east longitude */
       for(j = 2; j < num_rows; ++j){    /* centric latitude */
         sum_r=0.;
         for(k = i-nsw/2; k <= i+nsw/2; k++){
           m=k;
           if(m < 2) m=num_cols-3+m;
           if(m >= num_cols) m=m-num_cols+3;
           for(l = j-nlw/2; l <= j+nlw/2; l++){
             n=l;
             if(n < 2) n=2;
             if(n > num_rows-1) n=num_rows-1;
             sum_r += radius[n][m];
           }
         }
         radius2[j][i]=sum_r/(nlw*nsw);
       }
     }
     for(i = 2; i < num_cols; ++i){       /*centric  east longitude */
       for(j = 2; j < num_rows; ++j){    /* centric latitude */
         radius[j][i]=radius2[j][i];
       }
     }
     printf("end of smoothing operation\n");
   }

/*add two columns on both ends*/
/*   for(i = 1; i < num_rows - 1; ++i)*/
   for(i = 2; i < num_rows ; ++i)
   {
      radius[i][1] = radius[i][num_cols-1];
      radius[i][num_cols] = radius[i][2];
   }
/*add two rows on both ends*/
   for(i = 1; i <= num_cols; ++i)
   {
      radius[1][i] = radius[2][i];
      radius[num_rows][i] = radius[num_rows - 1][i];
   }

/* override radius if TRIAXIAL keyword specified */
  if(triaxial_count == 3){
    for(j=1; j <= num_rows; j++){
     for(i=1; i <= num_cols; i++){
       coslon2=cos(lng[i])*cos(lng[i]);
       coslat2=cos(lat[j])*cos(lat[j]);
       sinlat2=sin(lat[j])*sin(lat[j]);
       radius[j][i] = 1.0/(
         coslat2*coslon2/triaxial[0]+
         coslat2*(1.0-coslon2)/triaxial[1]+
         sinlat2/triaxial[2]);
     }
    }
  }

  for(j=1; j <= num_rows; j++){
   for(i=1; i <= num_cols; i++){
     if((radius[j][i] < 0.1) || (radius[j][i] > 1500.)){
     printf("i,j %d %d \n",i,j);
     printf("lon,lat,radius %8.3e %8.3e %8.3e \n",lng[i],lat[j],radius[j][i]);
     zvmessage("Unreasonable radius value"," ");
     zabend();
     }
   }
  }

/* compute spline derivatives */
/*   splie2(lng, lat, radius, num_cols, num_rows, r2lat, r2long); */
   splie2(lng, lat, radius, num_cols, num_rows, r2long, r2lat); 


/* open vicar files */
 status=zvunit(&ounit1,"OUT",1,NULL);
 zvsignal(ounit1,status,1);
 status=zvopen(ounit1,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","REAL","O_FORMAT","REAL",NULL);
 zvsignal(ounit1,status,1);
 status=zvunit(&ounit2,"OUT",2,NULL);
 zvsignal(ounit2,status,1);
 status=zvopen(ounit2,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","REAL","O_FORMAT","REAL",NULL);
 zvsignal(ounit2,status,1);
 status=zvunit(&ounit3,"OUT",3,NULL);
 zvsignal(ounit3,status,1);
 status=zvopen(ounit3,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","REAL","O_FORMAT","REAL",NULL);
 zvsignal(ounit3,status,1);
 status=zvunit(&ounit4,"OUT",4,NULL);
 zvsignal(ounit4,status,1);
 status=zvopen(ounit4,"OP","WRITE","U_NL",nl,"U_NS",ns,"U_NB",1,
                      "U_FORMAT","REAL","O_FORMAT","REAL",NULL);
 zvsignal(ounit4,status,1);

/* process picture data */
 for(line=1; line <= nl; line++){

   for(sample=1; sample <= ns; sample++){

     /* compute lat & w. lon in the simple cylindrical output map */
     latitude=((line-1.0)*180./(1.0-nl))+90.0;
     t=(ns+1.0)/2.0;
     if(sample > t) longitude=(sample+t-2.0*ns)*180./(t-ns);
     else longitude=(sample-t)*180./(1.0-t);

     /* convert longitude to east for internal use */
     longitude=360.-longitude;

/*
      printf("   \n");
      printf("lat=%f lon=%f line=%d samp=%d \n",latitude,longitude,line,sample);
*/

     /*convert them to radian*/
     xr = longitude*PI/180.0;
     yr = latitude*PI/180.0;

     splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
          xr, yr, zx, zy, &zxy);
/*
      printf("zx,zy,zxy %8.3e %8.3e %8.3e \n",zx[1],zy[1],zxy);
*/
/*
     zx[1] is radius
     zx[2] is first order partial derivative on long.
     zx[3] is the second order partial derivative on long.
     zy[1] is radius
     zy[2] is first order partial derivative ON lat.
     zy[3] is the second order partial derivative ON lat.
*/
     calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);

     calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
/*
      printf("e,f,g %8.3e %8.3e %8.3e \n",e[1],f[1],g[1]);
*/
     Ebuf[sample-1]=e[1];
     Fbuf[sample-1]=f[1];
     Gbuf[sample-1]=g[1];
     Rbuf[sample-1]=zx[1];
   }

   status=zvwrit(ounit1,Ebuf,"LINE",line,NULL);
   zvsignal(ounit1,status,1);
   status=zvwrit(ounit2,Fbuf,"LINE",line,NULL);
   zvsignal(ounit2,status,1);
   status=zvwrit(ounit3,Gbuf,"LINE",line,NULL);
   zvsignal(ounit3,status,1);
   status=zvwrit(ounit4,Rbuf,"LINE",line,NULL);
   zvsignal(ounit4,status,1);
 }                      /* line loop */

}

/*********************************************************************/
double **dmatrix( nrl, nrh, ncl, nch)
long nrl, nrh, ncl, nch;
/*allocate a double matrix with subscript range m[nrl .. nrh][ncl..nch]*/
{
   long i, nrow=nrh-nrl+1, ncol = nch-ncl+1;
   double **m;
 
  /*allocate pointers of rows */
   m=(double **) malloc((size_t)((nrow+NR_END)*sizeof(double *)));
   if(!m) zvmessage("allocation failure 1 in matrix()"," ");
   m +=NR_END;
   m -= nrl;
 
  /*allocate rows and set pointers to them */
  m[nrl]=(double *)malloc((size_t)((nrow*ncol+NR_END)*sizeof(double)));
  if(!m[nrl]) zvmessage("allocation failure 2 in matrix()"," ");
   m[nrl] += NR_END;
   m[nrl] -=ncl;
 
   for(i = nrl + 1; i < nrh; i++)
      m[i] = m[i-1] + ncol;
 
   /* return pointer ro array of pointers to rows */
   return m;
}


/*********************************************************************/
void calc_derivatives(xr, yr, rx, ry,rxy, dx, dy, dz, d2x, d2y, d2z)
double xr, yr, rx[4], ry[4], rxy, dx[4], dy[4], dz[4], d2x[4], d2y[4], d2z[4];
{
    dx[1] = ry[2]*cos(yr)*cos(xr) - rx[1]* sin(yr)*cos(xr);    
    dx[2] = rx[2]*cos(yr)*cos(xr) - rx[1]* cos(yr)*sin(xr);
    dy[1] = ry[2]*cos(yr)*sin(xr) - rx[1]* sin(yr)*sin(xr);
    dy[2] = rx[2]*cos(yr)*sin(xr) + rx[1]* cos(yr)*cos(xr);
    dz[1] = ry[2]* sin(yr) + rx[1]*cos(yr);
    dz[2] = rx[2]* sin(yr);
    d2x[1] = ry[3]* cos(yr) * cos(xr) - 2* ry[2]*sin(yr)*cos(xr) - 
      rx[1]*cos(yr)* cos(xr);
    d2y[1] = ry[3]* cos(yr) * sin(xr) - 2* ry[2]*sin(yr)*sin(xr) - 
      rx[1]*cos(yr)* sin(xr);
    d2z[1] = ry[3]* sin(yr)  + 2* ry[2]*cos(yr) - rx[1]*sin(yr);
 
    d2x[3] = rx[3]* cos(yr) * cos(xr) - 2* rx[2]*cos(yr)*sin(xr) - 
      ry[1]*cos(yr)* cos(xr);
    d2y[3] = rx[3]* cos(yr) * sin(xr) - 2* rx[2]*cos(yr)*cos(xr) - 
      ry[1]*cos(yr)* sin(xr);
    d2z[3] = rx[3]* sin(yr);
 
    d2x[2] = rxy* cos(yr) * cos(xr) -  ry[2]*cos(yr)*sin(xr)
             - rx[2]*sin(yr)*cos(xr) + rx[1]*sin(yr)* sin(xr);
    d2y[2] = rxy* cos(yr) * sin(xr) -  ry[2]*cos(yr)*cos(xr)
             - rx[2]*sin(yr)*sin(xr) - rx[1]*sin(yr)* cos(xr);
    d2z[2] = rxy* sin(yr)  +  rx[2]*cos(yr) ;
}
 
/*********************************************************************/
void calc_fund_forms(dx, dy, dz, d2x, d2y, d2z, e,f, g)
double dx[4], dy[4], dz[4], d2x[4], d2y[4], d2z[4], e[4], f[4], g[4];
{ 
   e[1] = dx[1]*dx[1] + dy[1]*dy[1] + dz[1]*dz[1];
   f[1] = dx[1]*dx[2] + dy[1]*dy[2] + dz[1]*dz[2];
   g[1] = dx[2]*dx[2] + dy[2]*dy[2] + dz[2]*dz[2];
 
   e[2] = 2*dx[1]*d2x[1] + 2*dy[1]*d2y[1] + 2*dz[1]*d2z[1];
   f[2] = d2x[1]*dx[2] +dx[1]*d2x[2] +
          d2y[1]*dy[2] +dy[1]*d2y[2] +
          d2z[1]*dz[2] +dz[1]*d2z[2];
   g[2] = 2*dx[2]*d2x[2] + 2*dy[2]*d2y[2] + 2*dz[2]*d2z[2];
 
   e[3] = 2*dx[1]*d2x[2] + 2*dy[1]*d2y[2] + 2*dz[1]*d2z[2];
   f[3] = d2x[3]*dx[1] +dx[2]*d2x[2] +
          d2y[3]*dy[1] +dy[2]*d2y[2] +
          d2z[3]*dz[1] +dz[2]*d2z[2];
   g[3] = 2*dx[2]*d2x[3] + 2*dy[2]*d2y[3] + 2*dz[2]*d2z[3];
}

/*********************************************************************/
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

/*********************************************************************/
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
 
/*********************************************************************/
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

/*********************************************************************/
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

/*********************************************************************/
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
