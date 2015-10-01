$!****************************************************************************
$!
$! Build proc for MIPL module areaiso
$! VPACK Version 1.9, Monday, December 07, 2009, 16:00:17
$!
$! Execute by entering:		$ @areaiso
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module areaiso ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to areaiso.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("areaiso.imake") .nes. ""
$   then
$      vimake areaiso
$      purge areaiso.bld
$   else
$      if F$SEARCH("areaiso.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake areaiso
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @areaiso.bld "STD"
$   else
$      @areaiso.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create areaiso.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack areaiso.com -mixed -
	-s areaiso.c util.h ludcmp.c newt.c spline.c util.c -
	-i areaiso.imake -
	-p areaiso.pdf -
	-t tstareaiso.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create areaiso.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util.h
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ludcmp.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include "util.h"
#include <stdio.h>
#include <stdlib.h>

#define TINY 1.0e-20
 
void ludcmp(a, n, indx, d)
double **a;
int n;
int *indx;
double *d;
{
   int i, imax, j, k;
   double big, dum, sum, temp;
   double *vv;
 
   vv=dvector(1, n);
   *d = 1.0;
   for(i=1; i <=n; i++)
   {
      big = 0.0;
      for(j = 1; j<=n; j++)
         if((temp = fabs(a[i][j])) > big) big = temp;
      if(big == 0.0)
      {
          printf("singluar matrix in routine ludcmp\n");
          exit(1);
      }
      vv[i] = 1.0/big;
   }
   for(j = 1; j<=n; j++)
   {
      for(i = 1; i<j; i++)
      {
         sum= a[i][j];
         for(k = 1; k<i; k++)  sum -=a[i][k]*a[k][j];
         a[i][j] =sum;
      }
      big = 0.0;
      for(i = j; i <=n; i++)
      {
          sum = a[i][j];
          for(k = 1; k<j; k++)
             sum -= a[i][k]*a[k][j];
          a[i][j]=sum;
          if (( dum=vv[i]*fabs(sum)) >= big)
          {
             big=dum;
             imax=i;
          }
      }
      if(j != imax){
          for(k = 1; k<=n; k++) {
             dum = a[imax][k];
             a[imax][k] = a[j][k];
             a[j][k]=dum;
          }
          *d= -(*d);
          vv[imax]=vv[j];
      }
      indx[j] = imax;
      if(a[j][j] == 0.0) a[j][j] = TINY;
      if(j != n)
      {
         dum= 1.0/(a[j][j]);
         for(i = j+1; i<=n; i++)  a[i][j] *= dum;
      }
   }
   free_dvector(vv, 1, n);
}
 
void lubksb(a, n, indx, b)
double **a;
int n;
int *indx;
double *b;
{
    int i, ii=0, ip, j;
    double sum;
                             
   for(i = 1; i<=n; i++)
   {
      ip= indx[i];
      sum= b[ip];
      b[ip] = b[i];
      if(ii)
         for(j =ii; j<=i-1; ++j)  sum -= a[i][j]*b[j];
      else if(sum) ii = i;
      b[i] = sum;
   }
   for(i = n; i >= 1; i--)
   {
      sum=b[i];
      for(j = i+1; j <= n; j++)  sum -=a[i][j]*b[j];
      b[i]=sum/a[i][i];
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create newt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create spline.c
$ DECK/DOLLARS="$ VOKAGLEVE"

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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create util.c
$ DECK/DOLLARS="$ VOKAGLEVE"

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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create areaiso.imake
#define  PROGRAM   areaiso

#define MODULE_LIST areaiso.c ludcmp.c newt.c spline.c util.c
#define INCLUDE_LIST util.h
#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create areaiso.pdf
process help=*
PARM OUT TYPE=STRING COUNT=6
PARM PLANET     TYPE=(STRING,12) COUNT=1 +
 VALID=("phobos","deimos","gaspra") DEFAULT="phobos"
PARM PATH    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/test_work/testdata/general
PARM MODE TYPE=KEYWORD COUNT=1 VALID=(north,south)
PARM TRIAXIAL TYPE=REAL COUNT=(0,3) DEFAULT=--
END-PROC

.TITLE
VICAR program AREAISO

.HELP
PURPOSE:
To create maps of auxiliary latitude & longitude for equal area projections
of irregularly shaped objects. 
Only good for polar coordinates.
The planet models must conform to a standard grid and reside in a directory 
pointed to by the PATH keyword.
Based upon Dr Yang Cheng's solution.

EXECUTION:
areaiso out=(lat,lon,area,angle,dlat,dlon) planet=phobos 'north

.PAGE
METHOD:
First the program reads the planet model file located in PATH.
This is an ascii table with west longitude, latitude, radius each 5 degrees.
All angles are planetocentric.

The program then solves the Cauchy equations for an equal area projection
of the ISO on a polar coordinate grid, either 'north or ' south.

The auxiliary coordinates are only useable for a polar coordinate grid.

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output images
latitude,
longitude,
area,
angle,
delta latitude,
delta longitude

.VARI PLANET
Planet name

.VARI PATH
Directory path for
planet models

.VARI MODE
Specify hemisphere.
mode=north or
mode=south

.VARI TRIAXIAL
Three radii: a,b,c
Overrides planet
model.

.LEVEL2

.VARI OUT
Output images.
1. latitude which comes from the initial grid latitude in degrees.
2. longitude which comes from the initial grid longitude in degrees.
3. The area constraint ratio.
4. The angle deformation in degrees.
5. latitude shift (latitude - grid location) in degrees.
6. longitude shift (longitude - grid location) in degrees.

All outputs are in REAL format.

.VARI PLANET
The planet or object name for whom auxiliary maps are to be generated..

.VARI PATH
The directory name where the planet models reside.

.VARI MODE
Specify the hemisphere.
mode=north for a north polar projection.
mode=south for a south polar projection.
NOTE: Mapaux must have the same keyword.

.VARI TRIAXIAL
Three triaxial ellipsoid radii: a, b, and c. a > b > c.
This optional keyword will cause the radii in the planet model to be
replaced by a triaxial ellipsoid of radii a b c (in km). Use the program
as one would ordinarily.

$ Return
$!#############################################################################
$Test_File:
$ create tstareaiso.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! for phobos
areaiso out= +
 (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
 del_area_lon.img) +
 planet=phobos 'north
!concat inp=(area_lat.img,area_lon.img,area_map.img,angle_map.img, +
! del_area_lat.img,del_area_lon.img) out=auth_mos_cheng.img ns=146
!size inp=auth_mos_cheng.img out=a.img zoom=4
!xvd a.img
!barne_r inp=a.img miplbox=21 primary=42057
!areaiso out= +
! (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
! del_area_lon.img) +
! planet=phobos 'south
!concat inp=(area_lat.img,area_lon.img,area_map.img,angle_map.img, +
! del_area_lat.img,del_area_lon.img) out=auth_mos_cheng.img ns=146
!size inp=auth_mos_cheng.img out=a.img zoom=4
!xvd a.img
!barne_r inp=a.img miplbox=21 primary=42057
!
! for triaxial
!areaiso out= +
! (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
! del_area_lon.img) +
! planet=phobos triaxial=(13.1,11.2,9.2) 'north
!concat inp=(area_lat.img,area_lon.img,area_map.img,angle_map.img, +
! del_area_lat.img,del_area_lon.img) out=auth_mos_cheng.img ns=146
!size inp=auth_mos_cheng.img out=a.img zoom=4
!xvd a.img
!barne_r inp=a.img miplbox=21 primary=42057
!areaiso out= +
! (area_lat.img,area_lon.img,area_map.img,angle_map.img,del_area_lat.img, +
! del_area_lon.img) +
! planet=phobos triaxial=(13.1,11.2,9.2) 'south
!concat inp=(area_lat.img,area_lon.img,area_map.img,angle_map.img, +
! del_area_lat.img,del_area_lon.img) out=auth_mos_cheng.img ns=146
!size inp=auth_mos_cheng.img out=a.img zoom=4
!xvd a.img
!barne_r inp=a.img miplbox=21 primary=42057
!
end-proc
$ Return
$!#############################################################################
