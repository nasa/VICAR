$!****************************************************************************
$!
$! Build proc for MIPL module efgiso
$! VPACK Version 1.9, Monday, December 07, 2009, 16:06:50
$!
$! Execute by entering:		$ @efgiso
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
$ write sys$output "*** module efgiso ***"
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
$ write sys$output "Invalid argument given to efgiso.com file -- ", primary
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
$   if F$SEARCH("efgiso.imake") .nes. ""
$   then
$      vimake efgiso
$      purge efgiso.bld
$   else
$      if F$SEARCH("efgiso.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake efgiso
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @efgiso.bld "STD"
$   else
$      @efgiso.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create efgiso.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack efgiso.com -mixed -
	-s efgiso.c -
	-i efgiso.imake -
	-p efgiso.pdf -
	-t tstefgiso.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create efgiso.c
$ DECK/DOLLARS="$ VOKAGLEVE"
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
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create efgiso.imake
#define  PROGRAM   efgiso

#define MODULE_LIST efgiso.c

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_RTL
#define LIB_P2SUB
#define LIB_TAE
$ Return
$!#############################################################################
$PDF_File:
$ create efgiso.pdf
process help=*
PARM OUT TYPE=STRING COUNT=4
PARM PLANET     TYPE=(STRING,12) COUNT=1 +
 VALID=("phobos","deimos","gaspra") DEFAULT="phobos"
PARM PATH    TYPE=(STRING,80) COUNT=(0:1) +
   DEFAULT=/project/test_work/testdata/general
PARM NL TYPE=INTEGER COUNT=1 VALID=(2:2000) DEFAULT=100
PARM NS TYPE=INTEGER COUNT=1 VALID=(2:2000) DEFAULT=100
PARM NLW TYPE=INTEGER COUNT=(0,1) VALID=(1:37) DEFAULT=1
PARM NSW TYPE=INTEGER COUNT=(0,1) VALID=(1:73) DEFAULT=1
PARM TRIAXIAL TYPE=REAL COUNT=(0,3) DEFAULT=--
END-PROC

.TITLE
VICAR program EFGISO

.HELP
PURPOSE:
To create images of E F and G for the ISO (irregularly shaped object),
and an image of the object radius.
E F and G are used to compute the validity of conformal and authalic
map projections.
The planet models must conform to a standard grid and reside in a directory 
pointed to by the PATH keyword.

EXECUTION:
mapiso out=(E,F.G) planet=phobos nl=180 ns=360 

.PAGE
METHOD:
First the program reads the planet model file located in PATH.
This is an ascii table with west longitude, latitude, radius each 5 degrees.
All angles are planetocentric.

1> Construct a radius surface, which is a function of lat. and long.
   Two dimentional cubic spline interpolation is used here.
 
2> With the radius surface, the ISO can be constructed as
 
   X = r(lat, long) cos(lat)cos (long)
   Y = r(lat, long) cos(lat)sin (long)
   Z = r(lat, long) sin(lat)
 
3> From the cubic spline and the equations in (2), the partial derivatives 
can be calculated.
 
  X_lat, X_long, Y_lat, Y_long, Z_lat and Z_long.
 
4>
   E = (X_lat)(X_lat) + (Y_lat)(Y_lat) + (Z_lat)(Z_lat)
   F = (X_lat)(X_long) + (Y_lat)(Y_long) + (Z_lat)(Z_long)
   G = (X_long)(X_long) + (Y_long)(Y_long) + (Z_long)(Z_long)
 
To verify that the map projection is truly conformal or authalic the 
following constraint must be true, where:
EFG are measured on the ISO.
efg are mesured on the map projection of the ISO.

Conformal case:
E/e=G/g  and F/sqrt(EG)=f/sqrt(eg)
(note: f/sqrt(eg) is the cosine of the angle between meridians and latitudes)

Authalic case:
EG-FF=eg-ff and F/sqrt(EG)=f/sqrt(eg)
(note: ff is f*f)

HISTORY:
9-1-98  J Lorre. 
COGNIZANT PROGRAMMER:  Jean Lorre

.LEVEL1

.VARI OUT
Output images
E, F, G, Radius

.VARI NL
number lines in
output images.

.VARI NS
number samples in
output images.

.VARI PLANET
Planet name

.VARI PATH
Directory path for
planet models

.VARI NLW
Size of a smoothing filter.
Filter height in n-s direction.
Default is 1.

.VARI NSW
Size of a smoothing filter.
Filter  width in e-w direction.
Default is 1.

.VARI TRIAXIAL
Three radii: a,b,c
Overrides planet
model.

.LEVEL2

.VARI OUT
Output images. These represent the fundamental forms:
1. E
2. F
3. G
and radius:
4. Radius in km.

All outputs are in a rectangular projection with zero lat & lon at the center,
in west planetocentric longitude, north at the top, and covering the entire
planet.
All outputs are in REAL format.

.VARI NL
number lines in
output images.

.VARI NS
number samples in
output images.

.VARI PLANET
The planet or object name for whom E F G & R maps are to be generated..

.VARI PATH
The directory name where the planet models reside.

.VARI NLW
Size of a smoothing filter.
Filter height in n-s direction.
Each count is 5 degrees.
Largest value is 37 which spans +90 to -90.
Default is 1.
Must be odd.

.VARI NSW
Size of a smoothing filter.
Filter  width in e-w direction.
Each count is 5 degrees.
Largest value is 73 which spans 0 to 360.
Default is 1.
Must be odd.

.VARI TRIAXIAL
Three triaxial ellipsoid radii: a, b, and c. a > b > c.
This optional keyword will cause the radii in the planet model to be 
replaced by a triaxial ellipsoid of radii a b c (in km). Use the program
as one would ordinarily.
$ Return
$!#############################################################################
$Test_File:
$ create tstefgiso.pdf
procedure
refgbl $echo
body
let $echo="yes"
!
! for phobos
efgiso out=(E.img,F.img,G.img,R.img) planet=phobos nl=180 ns=360
!xvd E.img
!xvd F.img
!xvd G.img
!xvd R.img
!efgiso out=(E6.img,F6.img,G6.img,R6.img) planet=phobos nl=180 ns=360 +
! nlw=19 nsw=37
!efgiso out=(E5.img,F5.img,G5.img,R5.img) planet=phobos nl=180 ns=360 +
! nlw=11 nsw=19
!efgiso out=(E4.img,F4.img,G4.img,R4.img) planet=phobos nl=180 ns=360 +
! nlw=7 nsw=9
!efgiso out=(E3.img,F3.img,G3.img,R3.img) planet=phobos nl=180 ns=360 +
! nlw=5 nsw=5
!efgiso out=(E2.img,F2.img,G2.img,R2.img) planet=phobos nl=180 ns=360 +
! nlw=3 nsw=3
!xvd E3.img
!xvd F3.img
!xvd G3.img
!xvd R3.img
!
! for triaxial
!efgiso out=(TE.img,TF.img,TG.img,TR.img) planet=phobos nl=180 ns=360 +
!  triaxial=(13.1,11.2,9.2)
!xvd TE.img
!xvd TF.img
!xvd TG.img
!xvd TR.img
!
! for sphere
!efgiso out=(SE.img,SF.img,SG.img,SR.img) planet=phobos nl=180 ns=360 +
!  triaxial=(11.5,11.5,11.5)
!! 11.7 matches Phobos well. 11.5 matches the triaxial above.
!xvd SE.img
!xvd SF.img
!xvd SG.img
!xvd SR.img
!
end-proc
$ Return
$!#############################################################################
