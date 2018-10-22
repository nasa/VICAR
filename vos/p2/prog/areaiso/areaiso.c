/* areaiso */
/************************************************************************/
/*Program Name:	iso_equalarea_jpl.c                                     */
/*Purpose:	Develop two equal area map projections for both Northern*/
/*		and Southern Hemisheres of ISO                          */
/*Auther:  	Yang Cheng						*/
/*Affiliation:	Oak Ridge National Laboratory				*/
/*Date:		December, 1997						*/
/************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "util.h"
#include "vicmain_c"

#define INTERVAL 5.0
#define STEP_LEGHTH 5.0
#define PI 3.14159265359
#define SWAP(a,b) {temp =(a); (a) = (b); (b)=temp;}
 
void gaussj();
void sor_test();
void calc_derivatives();
void calc_fund_forms();
void calc_gam();
void calc_curvature();
void calc_equator();
void calc_theFirst_Two_Parallel_Northern();
void calc_theFirst_Two_Parallel_Southern();
void calc_Projection_Northern();
void calc_Projection_Southern();
void calc_efg();
void calc_fvec();
void calc_fjac();
void calc_area();

void main44(void)
{

/* for vicar */
   char planet_name[30],path_name[80],filename[80];
   char msg[80];
   int status,count,def,line,sample,north,south;
   int ounit[7],nl,ns,l,triaxial_count;
   float lat_buf[100],lon_buf[100],rescale_n,rescale_s,buf1[100],buf2[100];
   float dlat_buf[100],dlon_buf[100],area_buf[100],angle_buf[100];
   float lat_grid[100],lon_grid[100],buf3[100],buf4[100];
   double triaxial[3],coslon2,coslat2,sinlat2;

   FILE *IN;
   int i, j;
   double x, y, zx[4], zy[4];
   double e[4], f[4], g[4];
   double xr, yr;
   double dx[4], dy[4], dz[4];
   double d2x[4], d2y[4], d2z[4];
   double dxt[4], dyt[4], dzt[4], r;
   double zxy;
/*
   double xm[183][363], ym[183][363]; 
   double ar[183][363], ae[183][363];
*/
/*map coordinate of northern heimsphere*/
   double **xm_n, **ym_n; /*Map projection coordinate in polar coordinate system*/
/*xm_n stores the raduii and ym_n stores the polar angle*/
   double **ar_n, **ae_n; /*ar is area ratio ae_n is angular error*/

/*map coordinate of southern heimsphere*/
   double **xm_s, **ym_s; /*Map projection coordinate in polar coordinate system*/
   double **ar_s, **ae_s; /*ar_s is area ratio ae_s is angular error*/

   double rst[5];
   double *lat, *lng, **radius, **r2long, **r2lat;
   int num_cols, num_rows;
   int map_num_cols, map_num_rows;
   float xtmp, ytmp, ztmp;
   double gam[3][4];
   double area;
   int k;

/* vicar parameters */
   status=zveaction("AS","  ");
   status=zvparm("PATH",path_name,&count,&def,1,0);
   status=zvparm("PLANET",planet_name,&count,&def,1,0);
   north=zvptst("NORTH");
   south=zvptst("SOUTH");
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

   if((IN = fopen(filename, "r")) == NULL){
     fprintf(stderr, "Error opening file %s\n", filename);
     zabend();
   }
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);
   fscanf(IN,"%s \n",msg);

   num_cols = 360.0/INTERVAL + 3;/*extends one more columun on both ends*/
   num_rows = 180.0/INTERVAL + 3;/*enxtend one more row on both end*/  
  /* the map projection coordinates grid size*/
   map_num_cols = 360.0/STEP_LEGHTH;
   map_num_rows = 180.0/STEP_LEGHTH;  
   lat = (double *)malloc((num_rows + 2)*sizeof(double)); 
   lng = (double *)malloc((num_cols + 2)*sizeof(double)); 
   radius = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   r2lat = dmatrix(0, num_rows + 1, 0, num_cols + 1);
   r2long = dmatrix(0, num_rows + 1, 0, num_cols + 1);

   xm_n = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);
   ym_n = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);

   xm_s = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);
   ym_s = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);

   ar_n = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);
   ae_n = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);

   ar_s = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);
   ae_s = dmatrix(0, map_num_rows+ 1, 0, map_num_cols + 1);

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
   for(i = 2; i < num_cols; ++i)
   {
      for(j = 2; j < num_rows; ++j)
      {
        if((fgets(msg, sizeof(msg), IN)) == NULL){
           fprintf(stderr, "Error reading file %s\n", filename);
           zabend();
        }
        sscanf(msg,"%f, %f, %f\n", &xtmp, &ytmp, &ztmp);
         radius[j][i] = ztmp;
      }
   }
   fclose(IN);
   /*add two raws on both ends*/
   for(i = 1; i <= num_rows; ++i)
   {
      radius[i][1] = radius[i][num_cols-1];
      radius[i][num_cols] = radius[i][2];
   }
   /*add two columes on both ends*/
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

/*2-D Cubic Spline Interpolation*/
   splie2(lng, lat, radius, num_cols, num_rows, r2long, r2lat); 



 if(north){
  zvmessage("North projection only"," ");
/*calculate the first and second Parallel lines, at this case, its
  latitude = 85  and 80 degree, which is stored xm_n[1][*], ym_n[1][*]
  and xm_n[2][*], ym_n[2][*], xm_n[1][1] and ym_n[1][1] is the coordinate
  at lat = 85 and long = 0 and so on*/

   calc_theFirst_Two_Parallel_Northern(STEP_LEGHTH*PI/180, map_num_cols, 
      lng, lat, radius, r2long, r2lat, num_cols, num_rows, xm_n, ym_n, area);

/*calculate map projection of northern hemisphere*/
   calc_Projection_Northern(STEP_LEGHTH*PI/180, xr, map_num_cols, 
      lng, lat, radius, r2long, r2lat, num_cols, num_rows, xm_n, ym_n, ar_n, ae_n);
 }

 if(south){
  zvmessage("South projection only"," ");
/*calculate the first and second Parallel lines, at this case, its
  latitude = -85  and -80 degree, which are stored in xm_s[1][*], ym_s[1][*]
  and xm_s[2][*], ym_s[2][*]*/

   calc_theFirst_Two_Parallel_Southern(STEP_LEGHTH*PI/180, map_num_cols, 
      lng, lat, radius, r2long, r2lat, num_cols, num_rows, xm_s, ym_s, area);

/*calculate map projection of southern hemisphere*/
   calc_Projection_Southern(STEP_LEGHTH*PI/180, xr, map_num_cols, 
      lng, lat, radius, r2long, r2lat, num_cols, num_rows, xm_s, ym_s, ar_s, ae_s);
 }


 nl=map_num_rows + 1;
 ns=map_num_cols;

/* open vicar files */
 for(l=1; l <= 6; l++){
   status=zvunit(&ounit[l],"OUT",l,NULL);
   zvsignal(ounit[l],status,1);
   status=zvopen(ounit[l],"OP","WRITE","U_NL",nl,"U_NS",ns+1,"U_NB",1,
                        "U_FORMAT","REAL","O_FORMAT","REAL",NULL);
   zvsignal(ounit[l],status,1);
 }

/* rescale converts radius into degrees latitude */
 rescale_n=0.;
 l=nl/2;
 for(sample=0; sample < ns; sample++){
   rescale_n += xm_n[l][sample+1];
 }
 rescale_n = 90.0*ns/rescale_n;
 rescale_s=0.;
 l=nl/2;
 for(sample=0; sample < ns; sample++){
   rescale_s += xm_s[l][sample+1];
 }
 rescale_s = 90.0*ns/rescale_s;

/* compute picture lat & lon grid coordinates */
 for(sample=0; sample <= ns; sample++){
     buf1[sample]=sample*5.;
 }
 for(sample=0; sample <= ns/2; sample++){
   lon_grid[sample]=buf1[ns/2-sample];
 }
 for(sample=ns/2+1; sample < ns; sample++){
   lon_grid[sample]=buf1[ns-sample+ns/2];
 }
 lon_grid[ns]=lon_grid[0];
 for(line=1; line <= nl; line++){
   lat_grid[line]=90.-(line-1)*5.;
 }

/* process picture data */
 for(line=1; line <= nl; line++){

  if(line == 1){
   for(sample=0; sample < ns; sample++){
     lat_buf[sample]=90.;
     lon_buf[sample]=sample*5.;
     area_buf[sample]=1.0;
     angle_buf[sample]=0.0;
   }
  }

  /*if((line >= 2) && (line <= nl/2+1)){*/
  if((line >= 2) && (line <= nl-1) && (north)){
   l=line-1;
   for(sample=0; sample < ns; sample++){
     lat_buf[sample]=90. - xm_n[l][sample+1]*rescale_n;
     lon_buf[sample]=ym_n[l][sample+1]*180./PI;
     area_buf[sample]=ar_n[l][sample+1];
     angle_buf[sample]=ae_n[l][sample+1]*180/PI;
   }
  }

  /*if((line >= nl/2+2) && (line <= nl-1)){*/
  if((line >= 2) && (line <= nl-1) && (south)){
   l=map_num_rows/2+nl/2+1-line;
   for(sample=0; sample < ns; sample++){
     lat_buf[sample]=xm_s[l][sample+1]*rescale_s - 90.;
     lon_buf[sample]=ym_s[l][sample+1]*180./PI;
     area_buf[sample]=ar_s[l][sample+1];
     angle_buf[sample]=ae_s[l][sample+1]*180/PI;
   }
  }

  if(line == nl){
   for(sample=0; sample < ns; sample++){
     lat_buf[sample]=-90.;
     lon_buf[sample]=sample*5.;
     area_buf[sample]=1.0;
     angle_buf[sample]=0.0;
   }
  }

  for(sample=0; sample < ns; sample++){
   buf1[sample]=lat_buf[sample];
   buf2[sample]=lon_buf[sample];
   buf3[sample]=area_buf[sample];
   buf4[sample]=angle_buf[sample];
  }
  for(sample=0; sample <= ns/2; sample++){  /* rearrange with 0 longitude in center */
   lat_buf[sample]=buf1[ns/2-sample];
   lon_buf[sample]=buf2[ns/2-sample];
   area_buf[sample]=buf3[ns/2-sample];
   angle_buf[sample]=buf4[ns/2-sample];
  }
  for(sample=ns/2+1; sample < ns; sample++){
   lat_buf[sample]=buf1[ns-sample+ns/2];
   lon_buf[sample]=buf2[ns-sample+ns/2];
   area_buf[sample]=buf3[ns-sample+ns/2];
   angle_buf[sample]=buf4[ns-sample+ns/2];
  }
  lat_buf[ns]=lat_buf[0];
  lon_buf[ns]=lon_buf[0];
  area_buf[ns]=area_buf[0];
  angle_buf[ns]=angle_buf[0];
  for(sample=0; sample <= ns; sample++){ /* convert To to FROM */
   lat_buf[sample] -= 2.0*(lat_buf[sample]-lat_grid[line]);
   lon_buf[sample] -= 2.0*(lon_buf[sample]-lon_grid[sample]);
   if(lat_buf[sample] >  90.)lat_buf[sample]= 90.;
   if(lat_buf[sample] < -90.)lat_buf[sample]=-90.;
   if(lon_buf[sample] > 360.)lon_buf[sample] -= 360.;
   if(lon_buf[sample] <   0.)lon_buf[sample] += 360.;
  }
  for(sample=0; sample <= ns; sample++){  /* compute dlat dlon */
   dlat_buf[sample]=lat_buf[sample]-lat_grid[line];
   dlon_buf[sample]=lon_buf[sample]-lon_grid[sample];
   if(dlon_buf[sample] < -180.) dlon_buf[sample] += 360.;
   if(dlon_buf[sample] >  180.) dlon_buf[sample] -= 360.;
  }

  status=zvwrit(ounit[1],lat_buf,"LINE",line,NULL);
  zvsignal(ounit[1],status,1);
  status=zvwrit(ounit[2],lon_buf,"LINE",line,NULL);
  zvsignal(ounit[2],status,1);
  status=zvwrit(ounit[3],area_buf,"LINE",line,NULL);
  zvsignal(ounit[3],status,1);
  status=zvwrit(ounit[4],angle_buf,"LINE",line,NULL);
  zvsignal(ounit[4],status,1);
  status=zvwrit(ounit[5],dlat_buf,"LINE",line,NULL);
  zvsignal(ounit[5],status,1);
  status=zvwrit(ounit[6],dlon_buf,"LINE",line,NULL);
  zvsignal(ounit[6],status,1);
 }
}


void calc_derivatives(xr, yr, rx, ry,rxy, dx, dy, dz, d2x, d2y, d2z)
double xr, yr, rx[4], ry[4], rxy, dx[4], dy[4], dz[4], d2x[4], d2y[4], d2z[4];
{
    dx[1] = ry[2]*cos(yr)*cos(xr) - rx[1]* sin(yr)*cos(xr);    
    dx[2] = rx[2]*cos(yr)*cos(xr) - rx[1]* cos(yr)*sin(xr);
    dy[1] = ry[2]*cos(yr)*sin(xr) - rx[1]* sin(yr)*sin(xr);
    dy[2] = rx[2]*cos(yr)*sin(xr) + rx[1]* cos(yr)*cos(xr);
    dz[1] = ry[2]* sin(yr) + rx[1]*cos(yr);
    dz[2] = rx[2]* sin(yr);
    d2x[1] = ry[3]* cos(yr) * cos(xr) - 2* ry[2]*sin(yr)*cos(xr) - rx[1]*cos(yr)* cos(xr);
    d2y[1] = ry[3]* cos(yr) * sin(xr) - 2* ry[2]*sin(yr)*sin(xr) - rx[1]*cos(yr)* sin(xr);
    d2z[1] = ry[3]* sin(yr)  + 2* ry[2]*cos(yr) - rx[1]*sin(yr);

    d2x[3] = rx[3]* cos(yr) * cos(xr) - 2* rx[2]*cos(yr)*sin(xr) - ry[1]*cos(yr)* cos(xr);
    d2y[3] = rx[3]* cos(yr) * sin(xr) - 2* rx[2]*cos(yr)*cos(xr) - ry[1]*cos(yr)* sin(xr);
    d2z[3] = rx[3]* sin(yr);

    d2x[2] = rxy* cos(yr) * cos(xr) -  ry[2]*cos(yr)*sin(xr)
             - rx[2]*sin(yr)*cos(xr) + rx[1]*sin(yr)* sin(xr);
    d2y[2] = rxy* cos(yr) * sin(xr) -  ry[2]*cos(yr)*cos(xr)
             - rx[2]*sin(yr)*sin(xr) - rx[1]*sin(yr)* cos(xr);
    d2z[2] = rxy* sin(yr)  +  rx[2]*cos(yr) ;
}

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

void calc_gam(e, f, g, gam)
double e[4], f[4], g[4], gam[3][4];
{
  gam[1][1] = (g[1]*e[2] - 2.0*f[1]*f[2] + f[1]*e[3])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));
  gam[1][2] = (g[1]*e[3] - f[1]*g[2])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));
  gam[1][3] = (2.0*g[1]*f[3] - g[1]*g[2] - f[1]*g[3])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));

  gam[2][1] = (2.0*e[1]*f[2] - e[1]*e[3] - f[1]*e[2])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));
  gam[2][2] = (e[1]*g[2] - f[1]*e[3])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));
  gam[2][3] = (e[1]*g[3] - 2.0*f[1]*f[3] + f[1]*g[2])/
              (2.0*(e[1]*g[1] - f[1]*f[1]));

}

/*checked once, no error was found */

void calc_curvature(e, f, g, gam,dx, dy, r)
double e[4], f[4], g[4], gam[3][4], dx[4], dy[4], *r;
{
  double a, b;
double c;
  a = gam[1][1]*dy[2]*dy[2] + 2.0*gam[1][2]*dy[2]*dx[2] + gam[1][3]*dx[2]*dx[2];
  b = gam[2][1]*dy[2]*dy[2] + 2.0*gam[2][2]*dy[2]*dx[2] + gam[2][3]*dx[2]*dx[2];
  c = sqrt(e[1]*g[1] - f[1]*f[1])*(dy[3]*dx[2] - dx[3]*dy[2] + a*dx[2] - b*dy[2]);
  
  a = e[1]*dy[2]*dy[2] + 2.0*f[1]*dx[2]*dy[2] + g[1]*dx[2]*dx[2];
  a = a*sqrt(a);
  c = c/a;
  *r = c;
}
/*
void calc_equator( h,  num_steps,  lng, lat,
      radius, r2long, r2lat, num_cols, num_rows, xm, ym)
double h;
int num_steps;
double *lng, *lat, **radius, **r2long, **r2lat;
int num_cols, num_rows;
double xm[183][363], ym[183][363];
{
   int i, j;
   int mrow;
   double x0, x1, a[5];
   double r, am, ds, ar;
   double zx[4], zy[4];
   double e[4], f[4], g[4];
   double gam[3][4];
   double xr, yr;
   double dx[4], dy[4], dz[4];
   double d2x[4], d2y[4], d2z[4];
   double dxt[4], dyt[4], dzt[4];
   double zxy;
   double akg;

   mrow = 91;
   x0 = 0.0;
   xm[mrow][1] = 0.0;
   ym[mrow][1] = 0.0;
   for( j = 1; j <= num_steps; ++j)
   {
      for(i = 1; i <= 4; ++i)
      {
         xr =  (j - 1) * h + i*h/5;
         yr = 0.0;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows, xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
         calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
         a[i] = sqrt(g[1]);
      }
      x1 = x0 + h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] + 5.0/24.0*a[3] + 55/24.0*a[4]);
      x0 = x1;
      xm[mrow][j] = x1;
      ym[mrow][j] = 0;
   }
}
*/
void calc_theFirst_Two_Parallel_Northern( h, num_steps,  lng, lat,
      radius, r2long, r2lat, num_cols, num_rows, xm, ym, area)
double h;
int num_steps;
double *lng, *lat, **radius, **r2long, **r2lat;
int num_cols, num_rows;
double **xm, **ym;
double area;
{
   int i, j;
   int mrow;
   double x0, x1, a[5];
   double r, am, ds, ar;
   double zx[4], zy[4];
   double e[4], f[4], g[4];
   double gam[3][4];
   double xr, yr;
   double dx[4], dy[4], dz[4];
   double d2x[4], d2y[4], d2z[4];
   double dxt[4], dyt[4], dzt[4];
   double zxy;
   double  area1;
   double xi0, xi1, xi2;
   double c;
   int q;
   area1 = 0;
   mrow = 90/STEP_LEGHTH + 1;/*it has to been changed*/
   x0 = 0.0;
   for( j = 1; j <= num_steps; ++j)
   { 
      xm[0][j] = 0.0;
      ym[0][j] = (float)(j-1)*h;
   }
   for(i = 1; i <= num_steps; ++i )
   {
       xr = (double)(i-1) * h;
       yr =  PI/2.0 -h/2;
       splin2(lng, lat, radius, r2long, r2lat, num_cols,
              num_rows, xr, yr, zx, zy, &zxy);
       calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
       calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
   }
   for(i = 1; i <= num_steps; ++i )
   {
      c = 0;
      xr = (double)(i-1) * h;
      for(q = 1; q <= 4; ++q)
      {
         xr = (double)(i-1) * h;
         yr =  PI/2.0 - (double)q*h/5.0;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
         calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
         a[q] = 2*sqrt(e[1]*g[1] - f[1] * f[1]);
      }

      c = h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
          5.0/24.0*a[3] + 55/24.0*a[4]);
/*the first parallel line*/
      xm[1][i] = sqrt(c);
      ym[1][i] = xr;

      for(q = 1; q <= 4; ++q)
      {
         xr = (double)(i-1) * h;
         yr =  PI/2.0 - PI/36.0 - (double)q*h/5.0;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
         calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
         a[q] = 2*sqrt(e[1]*g[1] - f[1] * f[1]);
      }

      c = c+ h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
          5.0/24.0*a[3] + 55/24.0*a[4]);
/*the second parallel line*/
      xm[2][i] = sqrt(c);
      ym[2][i] = xr;
   }
}    

void calc_theFirst_Two_Parallel_Southern( h, num_steps,  lng, lat,
      radius, r2long, r2lat, num_cols, num_rows, xm, ym, area)
double h;
int num_steps;
double *lng, *lat, **radius, **r2long, **r2lat;
int num_cols, num_rows;
double **xm, **ym;
double area;
{
   int i, j;
   int mrow;
   double x0, x1, a[5];
   double r, am, ds, ar;
   double zx[4], zy[4];
   double e[4], f[4], g[4];
   double gam[3][4];
   double xr, yr;
   double dx[4], dy[4], dz[4];
   double d2x[4], d2y[4], d2z[4];
   double dxt[4], dyt[4], dzt[4];
   double zxy;
   double  area1;
   double xi0, xi1, xi2;
   double c;
   int q;
   area1 = 0;
   mrow = 90/STEP_LEGHTH + 1;/*it has to been changed*/
   x0 = 0.0;
   for( j = 1; j <= num_steps; ++j)
   { 
      xm[0][j] = 0.0;
      ym[0][j] = (float)(j-1)*h;
   }
/*
   for(i = 1; i <= num_steps; ++i )
   {
       xr = (double)(i-1) * h;
       yr =  -PI/2.0 +h/2;
       splin2(lng, lat, radius, r2long, r2lat, num_cols,
              num_rows, xr, yr, zx, zy, &zxy);
       calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
       calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
   }
*/
   for(i = 1; i <= num_steps; ++i )
   {
      c = 0;
      xr = (double)(i-1) * h;
      for(q = 1; q <= 4; ++q)
      {
         xr = (double)(i-1) * h;
         yr =  -PI/2.0 + (double)q*h/5.0;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
         calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
         a[q] = 2*sqrt(e[1]*g[1] - f[1] * f[1]);
      }

      c = h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
          5.0/24.0*a[3] + 55/24.0*a[4]);
/*the first parallel line*/
      xm[1][i] = sqrt(c);
      ym[1][i] = xr;

      for(q = 1; q <= 4; ++q)
      {
         xr = (double)(i-1) * h;
         yr =  -PI/2.0 + PI/36.0 + (double)q*h/5.0;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx, dy, dz, d2x, d2y, d2z);
         calc_fund_forms(dx, dy, dz,d2x, d2y, d2z, e,f, g);
         a[q] = 2*sqrt(e[1]*g[1] - f[1] * f[1]);
      }

      c = c+ h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
          5.0/24.0*a[3] + 55/24.0*a[4]);
/*the second parallel line*/
      xm[2][i] = sqrt(c);
      ym[2][i] = xr;
   }
}    

void calc_Projection_Northern( h, t0, num_steps,  lng, lat,
      radius, r2long, r2lat, num_cols, num_rows, xm, ym, ar, ae, area)
double h;
double t0;
int num_steps;
double *lng, *lat, **radius, **r2long, **r2lat;
int num_cols, num_rows;
double **xm, **ym;
double **ar, **ae;
double area;
{
   int i, j, q;
   int mrow;
   double r, am, ds;
   double zx[4], zy[4];
   double e1[4], f1[4], g1[4];
   double gam[3][4];
   double a[5];
   double xr, yr;
   double dx1[4], dy1[4], dz1[4];
   double d2x1[4], d2y1[4], d2z1[4];
   double dxt[4], dyt[4], dzt[4];
   double zxy;
   double akg;
   double *E, *F, *G;
   double *e, *f, *g;
   double *x_lat, *y_lat, *x_long, *y_long;
   double **fjac;
   double *fvec;
   double *x1, *x2, *y1, *y2, *x0, *y0, *eg;
   double c;
   double er, er1;
   double length;
   double *g2, *p, *xold, *xnew;
   double *ar1, *ae1;
   double error, error1;
   int *indx;
   int check;
 
   x0 = (double *)malloc(sizeof(double)*(num_steps+2));
   y0 = (double *)malloc(sizeof(double)*(num_steps+2));
   x1 = (double *)malloc(sizeof(double)*(num_steps+2));
   x2 = (double *)malloc(sizeof(double)*(num_steps+2));
   y1 = (double *)malloc(sizeof(double)*(num_steps+2));
   y2 = (double *)malloc(sizeof(double)*(num_steps+2));
   eg = (double *)malloc(sizeof(double)*(num_steps+2));
   E = (double *)malloc(sizeof(double)*(num_steps+2));
   F = (double *)malloc(sizeof(double)*(num_steps+2));
   G = (double *)malloc(sizeof(double)*(num_steps+2));
   e = (double *)malloc(sizeof(double)*(num_steps+2));
   f = (double *)malloc(sizeof(double)*(num_steps+2));
   g = (double *)malloc(sizeof(double)*(num_steps+2));
   x_lat = (double *)malloc(sizeof(double)*(num_steps+2));
   y_lat = (double *)malloc(sizeof(double)*(num_steps+2));
   x_long = (double *)malloc(sizeof(double)*(num_steps+2));
   y_long = (double *)malloc(sizeof(double)*(num_steps+2));
   fvec = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   g2 = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   p = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   xold = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   xnew = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   indx = (int *)malloc(sizeof(int)*(num_steps*2 + 2));
   fjac = dmatrix(0, num_steps*2 + 2, 0, num_steps *2);
   mrow = 90/STEP_LEGHTH + 1;
/*
   length = 0.9;
   length = 1.0*3.1415926/180.0;
*/
   length = h;
/*the length (primeter of the cylinder*/
      i = 1;
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = PI/2.0 - (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x2[j] = xm[ 1][j];
         y2[j] = ym[ 1][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }   
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]); 
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }
      i = 2;
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = PI/2.0 - (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x2[j] = xm[ i][j];
         y2[j] = ym[ i][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]);
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }

/*   for(i = 3; i <= 90/STEP_LEGHTH; ++i ) */
   for(i = 3; i <= 175/STEP_LEGHTH; ++i )
   {
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = PI/2.0 - (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows, xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x1[j] = xm[ i -1][j];
         y1[j] = ym[ i -1][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
      for( j = 1; j <= num_steps; ++j)
      {
         for(q = 1; q <5; ++q)
         {
            xr = (double)(j-1) * h;
            yr =  PI/2.0 -(double)h*(i-1)- (double)q*h/5.0;
            splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
            calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
            calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
            a[q] = 2*sqrt(e1[1]*g1[1] - f1[1] * f1[1]);
         }
         c = h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
             5.0/24.0*a[3] + 55/24.0*a[4]);
/*the first parallel line*/
/*
         x2[j] =2*x1[j]- xm[i-2][j];
*/
         x2[j] =sqrt(xm[i-1][j]*xm[i-1][j] + c);
         y2[j] =y1[j] ;
      }
/*solve the non-linear equation by new newtow interation method*/
/*printf("i = %d x0 = %f x2 = %f y0 = %f y2 = %f \n", i, x0[3], x2[3], y0[3], y2[3]);*/
      j =newt(num_steps,&check, length, x2, x0, y2, y0, E, F, G, indx, x_lat, x_long,
y_lat, y_long, e, f, g, g2, p, xold, xnew, fjac, fvec);
      if(j == 0 || check == 1)
      {
        printf("failed in subroutine newt exit...\n");
        exit(1);
      }
      for(j = 1; j <= num_steps; ++j)
      {
         xm[i][j] = x2[j];
         ym[i][j] = y2[j];
         x0[j] = xm[i-1][j];
         y0[j] = ym[i-1][j];
      }
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]); 
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }
/*
      printf("%f, %f\n", x2[1], y2[1]);
       printf("end\n");
*/
/*
      for(j = 1; j <= num_steps; ++j)
      {
         printf("4\n");
         printf("%f, %f\n", xm[mrow][j], ym[mrow][j]);
         printf("%f, %f\n", xm[mrow+1][j], ym[mrow+1][j]);
         printf("end\n");
      }
*/
   }
}

void calc_Projection_Southern( h, t0, num_steps,  lng, lat,
      radius, r2long, r2lat, num_cols, num_rows, xm, ym, ar, ae, area)
double h;
double t0;
int num_steps;
double *lng, *lat, **radius, **r2long, **r2lat;
int num_cols, num_rows;
double **xm, **ym;
double **ar, **ae;
double area;
{
   int i, j, q;
   int mrow;
   double r, am, ds;
   double zx[4], zy[4];
   double e1[4], f1[4], g1[4];
   double gam[3][4];
   double a[5];
   double xr, yr;
   double dx1[4], dy1[4], dz1[4];
   double d2x1[4], d2y1[4], d2z1[4];
   double dxt[4], dyt[4], dzt[4];
   double zxy;
   double akg;
   double *E, *F, *G;
   double *e, *f, *g;
   double *x_lat, *y_lat, *x_long, *y_long;
   double **fjac;
   double *fvec;
   double *x1, *x2, *y1, *y2, *x0, *y0, *eg;
   double c;
   double er, er1;
   double length;
   double *g2, *p, *xold, *xnew;
   double *ar1, *ae1;
   double error, error1;
   int *indx;
   int check;
 
   x0 = (double *)malloc(sizeof(double)*(num_steps+2));
   y0 = (double *)malloc(sizeof(double)*(num_steps+2));
   x1 = (double *)malloc(sizeof(double)*(num_steps+2));
   x2 = (double *)malloc(sizeof(double)*(num_steps+2));
   y1 = (double *)malloc(sizeof(double)*(num_steps+2));
   y2 = (double *)malloc(sizeof(double)*(num_steps+2));
   eg = (double *)malloc(sizeof(double)*(num_steps+2));
   E = (double *)malloc(sizeof(double)*(num_steps+2));
   F = (double *)malloc(sizeof(double)*(num_steps+2));
   G = (double *)malloc(sizeof(double)*(num_steps+2));
   e = (double *)malloc(sizeof(double)*(num_steps+2));
   f = (double *)malloc(sizeof(double)*(num_steps+2));
   g = (double *)malloc(sizeof(double)*(num_steps+2));
   x_lat = (double *)malloc(sizeof(double)*(num_steps+2));
   y_lat = (double *)malloc(sizeof(double)*(num_steps+2));
   x_long = (double *)malloc(sizeof(double)*(num_steps+2));
   y_long = (double *)malloc(sizeof(double)*(num_steps+2));
   fvec = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   g2 = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   p = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   xold = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   xnew = (double *)malloc(sizeof(double)*(num_steps*2 + 2));
   indx = (int *)malloc(sizeof(int)*(num_steps*2 + 2));
   fjac = dmatrix(0, num_steps*2 + 2, 0, num_steps *2);
   mrow = 90/STEP_LEGHTH + 1;
/*
   length = 0.9;
   length = 1.0*3.1415926/180.0;
*/
   length = h;
   /*printf("mrow = %d num_rows = %d, numsteps = %d\n", mrow, num_rows, num_steps);*/
/*the length (primeter of the cylinder*/
   /*printf("h = %f length = %f\n", h, length);*/
     i = 1;
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = -PI/2.0 + (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x2[j] = xm[ i][j];
         y2[j] = ym[ i][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]);
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }
     i = 2;
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = -PI/2.0 + (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x2[j] = xm[ i][j];
         y2[j] = ym[ i][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]);
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }
   /*for(i = 3; i <= 90/STEP_LEGHTH; ++i )*/
   for(i = 3; i <= 175/STEP_LEGHTH; ++i )
   {
      for( j = 1; j <= num_steps; ++j)
      {
         xr = (double)(j-1) * h;
         yr = -PI/2.0 + (double)i * h;
         splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows, xr, yr, zx, zy, &zxy);
         calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
         calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
         E[j] = e1[1];
         F[j] = f1[1];
         G[j] = g1[1];
         eg[j] = sqrt(e1[1]/g1[1]);
         x1[j] = xm[ i -1][j];
         y1[j] = ym[ i -1][j];
         x0[j] = xm[ i -1][j];
         y0[j] = ym[ i -1][j];
      }
      E[num_steps] = E[1];
      F[num_steps] = F[1];
      G[num_steps] = G[1];
      eg[num_steps] = eg[1];
      for( j = 1; j <= num_steps; ++j)
      {
         for(q = 1; q <= 4; ++q)
         {
            xr = (double)(j-1) * h;
            yr =  -PI/2.0 +(double)h*(i-1)+ (double)q*h/5.0;
            splin2(lng, lat, radius, r2long, r2lat, num_cols, num_rows,
                xr, yr, zx, zy, &zxy);
            calc_derivatives(xr, yr, zx, zy,zxy, dx1, dy1, dz1, d2x1, d2y1, d2z1);
            calc_fund_forms(dx1, dy1, dz1,d2x1, d2y1, d2z1, e1,f1, g1);
            a[q] = 2*sqrt(e1[1]*g1[1] - f1[1] * f1[1]);
         }
 
      c = h/5.0*(55.0/24.0*a[1] + 5.0/24.0*a[2] +
          5.0/24.0*a[3] + 55/24.0*a[4]);
/*the first parallel line*/
/*
         x2[j] =2*x1[j]- xm[i-2][j]; 
*/
         x2[j] =sqrt(xm[i-1][j]*xm[i-1][j] + c); 
         y2[j] =y1[j] ; 
      }
/*solve the non-linear equation by new newtow interation method*/
      j =newt(num_steps,&check, length, x2, x0, y2, y0, E, F, G, indx, x_lat, x_long,
y_lat, y_long, e, f, g, g2, p, xold, xnew, fjac, fvec);
      if(j == 0 || check == 1)
      {
        printf("failed in subroutine newt exit...\n");
        exit(1);
      }
      /*printf("2\n");*/
      for(j = 1; j <= num_steps; ++j)
      {
         xm[i][j] = x2[j];
         ym[i][j] = y2[j];
         x0[j] = xm[i-1][j];
         y0[j] = ym[i-1][j];
      }
calc_efg(num_steps,length, x2,x0,y2, y0, e, f, g, x_lat, x_long, y_lat, y_long);
      for(j = 1; j <= num_steps; ++j)
      {
         error = sqrt(e[j]*g[j] - f[j]*f[j])/sqrt(E[j]*G[j] - F[j]*F[j]); 
         ar[i][j] = error;
         error = atan(f[j]/sqrt(e[j]*g[j]));
         error1 = atan(F[j]/sqrt(E[j]*G[j]));
         ae[i][j] = (error - error1)*180/PI;
      }
      /*printf("%f, %f\n", x2[1], y2[1]);*/
       /*printf("end\n");*/
/*
      for(j = 1; j <= num_steps; ++j)
      {
         printf("4\n");
         printf("%f, %f\n", xm[mrow][j], ym[mrow][j]);
         printf("%f, %f\n", xm[mrow+1][j], ym[mrow+1][j]);
         printf("end\n");
      }
*/
   }
}
void gaussj(a, b, n)
double **a, *b;
int n;
{
        int *indxc, *indxr, *ipiv;
        int i, icol, irow, j, k, l, ll;
        double big, dum, pivinv, temp;
        int nn;
        indxc = ivector(1, n);
        indxr = ivector(1, n);
        ipiv = ivector(1,n);
         nn = n;
        for(j = 1; j<=n; j++) ipiv[j] = 0;
        for(i=1; i<=n; i++){
           big = 0.0;
              for( j = 1; j <= n; j++)
                 if(ipiv[j] != 1)
                    for(k = 1; k<=n; k++)
                    {
                       if(ipiv[k] == 0)
                       {
                          if(fabs(a[j][k]) >= big)
                          {
                             big=fabs(a[j][k]);
                             irow = j;
                             icol = k;
                          }
                       }   
                       else if(ipiv[k] > 1)
                       {
                          printf("gaussj: Singular Matrix-1\n");
                          return ;
                       }
                    }
                     ++(ipiv[icol]);
                    if(irow != icol)
                    {
                    for( l=1; l<=n; l++) SWAP(a[irow][l], a[icol][l]);
                    SWAP(b[irow], b[icol]);
                    }
        indxr[i] = irow;
        indxc[i] = icol;
        if(a[icol][icol] == 0.0){
   printf("gaussj :Singular Matrix-2");
   return ;
   } 
        pivinv=1.0/a[icol][icol];
        a[icol][icol]=1.0;
        for(l= 1; l<=n; l++) a[icol][l] *= pivinv;
         b[icol] *=pivinv;
        for( ll = 1; ll <= n; ll++)
           if(ll != icol){
             dum = a[ll][icol];
             a[ll][icol] = 0.0;
             for(l = 1; l <= n; l++) a[ll][l] -= a[icol][l] *dum;
             b[ll] -=b[icol]*dum;
             }
         }
 
         for(l= n; l >= 1; l--) {
            if(indxr[l] != indxc[l])
                for( k=1; k <= n; k++)
                    SWAP(a[k][indxr[l]], a[k][indxc[l]]);
         }
       free_ivector(ipiv, 1, n);
       free_ivector(indxr, 1, n);
       free_ivector(indxc, 1, n);
}

void calc_efg(n,length, p,p0,o, o0, e, f, g, p_lat, p_long, o_lat, o_long)
int n;
double length;
double *p, *p0, *o, *o0;
double *e, *f, *g;
double *p_lat, *p_long, *o_lat, *o_long;
{
   int i, j, num_steps; 
   double t;
 
   num_steps = n; 
   for(j = 1; j <= num_steps; ++j)
   {
      if(j == 1 )
      {
          p_lat[j] = (p[j] - p0[j])/length;
          p_long[j] = (p[2] - p[num_steps])/2/length;
          o_lat[j] = (o[j] - o0[j])/length;
          t = o[2] - o[num_steps];
          if(t < 0) t = 2.0*PI + t;
          o_long[j] = t/2/length;
      }
      else if( j == num_steps)
      {
          p_lat[j] = (p[j] - p0[j])/length;
          p_long[j] = (p[1] - p[num_steps -1])/2/length;
          o_lat[j] = (o[j] - o0[j])/length;
          t = o[1] - o[num_steps - 1];
          if(t < 0) t = 2.0*PI + t;
          o_long[j] = t/2.0/length;
      }
      else
      {   
          p_lat[j] = (p[j] - p0[j])/length;
          p_long[j] = (p[j+1] - p[j-1])/length/2;
          o_lat[j] = (o[j] - o0[j])/length;
          o_long[j] = (o[j+1] - o[j-1])/length/2;   
      }
   }  
   for(j = 1; j <= num_steps; ++j)
   {
      e[j] = p_lat[j]*p_lat[j] + o_lat[j]*o_lat[j]*p[j]*p[j];
      f[j] = p_lat[j]*p_long[j] + o_lat[j]*o_long[j]*p[j]*p[j];
      g[j] = p_long[j]*p_long[j] + o_long[j]*o_long[j]*p[j]*p[j];
/*
printf("efg %f %f %f\n", e[j], f[j], g[j]);
printf("x y %f %f %f %f\n", x[j], x0[j], y[j], y0[j]);
*/
   }
} 

void calc_fvec(n, E, F, G,e, f, g, fvec)
int n;
double *E, *F, *G;
double *e, *f, *g;
double *fvec;
{
   int i, j, num_steps;
   num_steps = n;
   for(j = 1; j <= num_steps; ++j)
   {
      fvec[j] = e[j]*g[j] - f[j]*f[j] - E[j]*G[j] + F[j]*F[j];
      fvec[j+num_steps] = f[j]*sqrt(E[j]*G[j]) - F[j]*sqrt(e[j]*g[j]);
   }
/*
printf("efg = %f %f %f EGF = %f %f %f\n", e[j], f[j], g[j], E[j], F[j], G[j]);
printf("first = %f second = %f\n", fvec[j], fvec[j+num_steps]);
j = 1;
printf("efg = %f %f %f EGF = %f %f %f\n", e[j], f[j], g[j], E[j], F[j], G[j]);
printf("first = %f second = %f\n", fvec[j], fvec[j+num_steps]);
*/
}

void calc_fjac(n,length, E, F, G, e, f, g, p, p_lat, p_long, o_lat, o_long, a)
int n;
double length;
double *E, *F, *G, *e, *f, *g, *p, *p_lat, *p_long, *o_lat, *o_long;
double **a;
{
int  i, j;
int num_steps;
   num_steps = n;
   for(i = 1; i<= num_steps*2; ++i)
   {
      for(j = 1; j <=num_steps*2; ++j)
      {
        a[i][j] = 0;
      }
   }
/*
printf("e f g= %f %f %f p = %f\n", e[12], f[12], g[12], p[12]);
printf("E F G= %f %f %f p = %f\n", E[12], F[12], G[12], p[2]);
printf("p %f %f o %f %f\n", p_lat[12], p_long[12], o_lat[12], o_long[12]);
*/
   for(j = 2; j < num_steps; ++j)
   {
       a[j][j-1] = -(p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] +
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j]; 
       a[j][j] = (2.0*p_lat[j]/length + 2.0*p[j]*o_lat[j]*o_lat[j])*g[j]
                 -2.0*f[j]*(p_long[j]/length + 2.0*p[j]*o_lat[j]*o_long[j]);
       a[j][j+1] = (p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] -
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j]; 
       a[j][j -1 + num_steps] = -p[j]*p[j]*e[j]*o_lat[j]/length +
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j][j+num_steps] = 2.0*p[j]*p[j]*g[j]*o_lat[j]/length -
                   2.0*f[j]*p[j]*p[j]*o_long[j]/length;
       a[j][j +1 + num_steps] = p[j]*p[j]*e[j]*o_lat[j]/length -
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][j] = sqrt(E[j]*G[j])*(p_long[j]/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(g[j]/e[j])*F[j]*
                             (p_lat[j]/length + p[j]*o_lat[j]*o_lat[j]);
       a[j+num_steps][j-1] = -sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) + sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][j+1] = sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][j-1+num_steps] = -sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               + F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
       a[j+num_steps][j+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_long[j]/length
               - F[j]*sqrt(g[j]/e[j])*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][j+1+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               - F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
   }
   j = 1;
       a[j][num_steps] = -(p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] +
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j];
       a[j][j] = (2.0*p_lat[j]/length + 2.0*p[j]*o_lat[j]*o_lat[j])*g[j]
                 -2.0*f[j]*(p_long[j]/length + 2.0*p[j]*o_lat[j]*o_long[j]);
       a[j][j+1] = (p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] -
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j];
       a[j][2*num_steps] = -p[j]*p[j]*e[j]*o_lat[j]/length +
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j][j+num_steps] = 2.0*p[j]*p[j]*g[j]*o_lat[j]/length -
                   2.0*f[j]*p[j]*p[j]*o_long[j]/length; 
       a[j][j +1 + num_steps] = p[j]*p[j]*e[j]*o_lat[j]/length -
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][j] = sqrt(E[j]*G[j])*(p_long[j]/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(g[j]/e[j])*F[j]*
                             (p_lat[j]/length + p[j]*o_lat[j]*o_lat[j]);
       a[j+num_steps][num_steps] = -sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) + sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][j+1] = sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][2*num_steps] = -sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               + F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
       a[j+num_steps][j+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_long[j]/length
               - F[j]*sqrt(g[j]/e[j])*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][j+1+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               - F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
/*
   a[j][num_steps] = - (y_long[j]*e[j] - x_lat[j]*f[j])/length;
   a[j][j] = (2.0*x_lat[j]*g[j] - 2.0*f[j]*x_long[j])/length;
   a[j][j+1] = (y_long[j]*e[j] - x_lat[j]*f[j])/length;
   a[j][2*num_steps] = -(x_long[j]*e[j] - y_lat[j]*f[j])/length;
   a[j][j+num_steps] = (2.0*y_lat[j]*g[j] - 2.0*f[j]*y_long[j])/length;
   a[j][j+1+num_steps] = (x_long[j]*e[j] - y_lat[j]*f[j])/length;
   a[j+num_steps][num_steps] = -sqrt(E[j]*G[j])*x_lat[j]/2/length
           + F[j]*sqrt(e[j]/g[j])*x_long[j]/2/length;
   a[j+num_steps][j] = sqrt(E[j]*G[j])*x_long[j]/length
           - F[j]*sqrt(g[j]/e[j])*x_lat[j]/length;
   a[j+num_steps][j+1] = sqrt(E[j]*G[j])*x_lat[j]/2/length
          - F[j]*sqrt(e[j]/g[j])*x_long[j]/2/length;
   a[j+num_steps][2*num_steps] = -sqrt(E[j]*G[j])*y_lat[j]/2/length
           + F[j]*sqrt(e[j]/g[j])*y_long[j]/2/length;
   a[j+num_steps][j+num_steps] = sqrt(E[j]*G[j])*y_long[j]/length
           - F[j]*sqrt(g[j]/e[j])*y_lat[j]/length;
   a[j+num_steps][j+1+num_steps] = sqrt(E[j]*G[j])*y_lat[j]/2/length
           - F[j]*sqrt(e[j]/g[j])*y_long[j]/2/length;
*/
   j = num_steps;
       a[j][j-1] = -(p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] +
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j];
       a[j][j] = (2.0*p_lat[j]/length + 2.0*p[j]*o_lat[j]*o_lat[j])*g[j]
                 -2.0*f[j]*(p_long[j]/length + 2.0*p[j]*o_lat[j]*o_long[j]);
       a[j][1] = (p_long[j]/length + 2*p[j]*o_long[j]*o_long[j])*e[j] -
                   (p_lat[j]/2/length + 2.0*p[j]*o_long[j]*o_lat[j])*2.0*f[j];
       a[j][j -1 + num_steps] = -p[j]*p[j]*e[j]*o_lat[j]/length +
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j][j+num_steps] = 2.0*p[j]*p[j]*g[j]*o_lat[j]/length -
                   2.0*f[j]*p[j]*p[j]*o_long[j]/length; 
       a[j][1 + num_steps] = p[j]*p[j]*e[j]*o_lat[j]/length -
                   f[j]*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][j] = sqrt(E[j]*G[j])*(p_long[j]/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(g[j]/e[j])*F[j]*
                             (p_lat[j]/length + p[j]*o_lat[j]*o_lat[j]);
       a[j+num_steps][j-1] = -sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) + sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][1] = sqrt(E[j]*G[j])*(p_lat[j]/2/length + 2.0*p[j]*
                             o_lat[j]*o_long[j]) - sqrt(e[j]/g[j])*F[j]*
                             (p_long[j]/length + 2.0*p[j]*o_long[j]*o_long[j])/2.0;
       a[j+num_steps][j-1+num_steps] = -sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               + F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
       a[j+num_steps][j+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_long[j]/length
               - F[j]*sqrt(g[j]/e[j])*p[j]*p[j]*o_lat[j]/length;
       a[j+num_steps][1+num_steps] = sqrt(E[j]*G[j])*p[j]*p[j]*o_lat[j]/2/length
               - F[j]*sqrt(e[j]/g[j])*p[j]*p[j]*o_lat[j]/2/length;
/*
    a[j][j-1] = - (y_long[j]*e[j] - x_lat[j]*f[j])/length;
    a[j][j] = (2.0*x_lat[j]*g[j] - 2.0*f[j]*x_long[j])/length;
    a[j][1] = (y_long[j]*e[j] - x_lat[j]*f[j])/length;
    a[j][j -1 + num_steps] = -(x_long[j]*e[j] - y_lat[j]*f[j])/length;
    a[j][j+num_steps] = (2.0*y_lat[j]*g[j] - 2.0*f[j]*y_long[j])/length;
    a[j][1+num_steps] = (x_long[j]*e[j] - y_lat[j]*f[j])/length;
   a[j+num_steps][j-1] = -sqrt(E[j]*G[j])*x_lat[j]/2/length
           + F[j]*sqrt(e[j]/g[j])*x_long[j]/2/length;
   a[j+num_steps][j] = sqrt(E[j]*G[j])*x_long[j]/length
           - F[j]*sqrt(g[j]/e[j])*x_lat[j]/length;
   a[j+num_steps][1] = sqrt(E[j]*G[j])*x_lat[j]/2/length
           - F[j]*sqrt(e[j]/g[j])*x_long[j]/2/length;
   a[j+num_steps][j-1+num_steps] = -sqrt(E[j]*G[j])*y_lat[j]/2/length
           + F[j]*sqrt(e[j]/g[j])*y_long[j]/2/length;
   a[j+num_steps][j+num_steps] = sqrt(E[j]*G[j])*y_long[j]/length
           - F[j]*sqrt(g[j]/e[j])*y_lat[j]/length;
   a[j+num_steps][1+num_steps] = sqrt(E[j]*G[j])*y_lat[j]/2/length
           - F[j]*sqrt(e[j]/g[j])*y_long[j]/2/length;
for(i = 1; i <= 2*num_steps; ++i)
{
for(j = 1; j <= 2*num_steps; ++j)
{
   if(a[i][j] != 0)
   printf("i = %d j = %d a = %f\n", i, j, a[i][j]);
}
printf("\n");
}
*/
}

void sor_test(a, b, n)
double **a, *b;
int n;
{
   int i, icol, irow, j, k, l, ll;
   double *b1, *b2;
   double omg = 0.8;
   double e, e1;
   b1 = (double *)malloc(sizeof(double)*(n+2));
   b2 = (double *)malloc(sizeof(double)*(n+2));
   for(i= 1; i<=n; ++i)
   { 
      b1[i] = 0;
      b2[i] = 0;
   } 
/*
   for(i = 1; i <= n/2; ++i)
   {
      e = 0;
      e1 = 0;
      for(j = 1; j <= n; ++j)
      {
         if(e <fabs(a[i][j])){
          l = j;
           e = fabs(a[i][j]);
         }
         if(e1 <fabs(a[i + n/2][j])){
            ll = j;
             e1 = fabs(a[i + n/2][j]);
           }
      }
         if( abs(l - ll) != n/2){
           printf(" hi hi hi wrong\n");
           exit(1);
         }
         if( l > ll)
         {
            for( j = 1; j <= n; ++j)
            {
               e = a[l][j];
               a[l][j] = a[ll][j];
               a[ll][j] = e;
            } 
               e = b[l];
               b[l] = b[ll];
               b[ll] = e;
          }
   }
*/
   for(ll = 1; ll < 100; ++ll)
   { 
      for(i = 1; i <= n; ++i)
      {
         b2[i] = b[i];
         for(j = 1; j <= i -1; ++j)
         {
           b2[i] -= a[i][j]*b2[j];
         }
         for(j = i+1; j <= n; ++j)
         {
           b2[i] -= a[i][j]*b1[j];
         }
         b2[i] = (1.0- omg)*b1[i] + omg*b2[i]/a[i][i];
      }
      e = 0.0;
      for(i = 1; i <= n; ++i)
      {
           e += fabs(b1[i] - b2[i]);
           b1[i] = b2[i];
      }
      if(e <= 0.00001)
      {
          for(i = 1; i <= n; ++i)
            b[i] = b1[i];
          printf("Prodedure completed successfully\n");
          return;
      }
   }
    printf("Prodedure completed unsuccessfully\n");
    exit(1);
}

