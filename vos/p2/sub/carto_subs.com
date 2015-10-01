$!****************************************************************************
$!
$! Build proc for MIPL module carto_subs
$! VPACK Version 1.9, Wednesday, December 21, 2011, 13:57:50
$!
$! Execute by entering:		$ @carto_subs
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
$ write sys$output "*** module carto_subs ***"
$!
$ Create_Source = ""
$ Create_Repack =""
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
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to carto_subs.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
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
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("carto_subs.imake") .nes. ""
$   then
$      vimake carto_subs
$      purge carto_subs.bld
$   else
$      if F$SEARCH("carto_subs.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake carto_subs
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @carto_subs.bld "STD"
$   else
$      @carto_subs.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create carto_subs.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack carto_subs.com -mixed -
	-s cartoGridUtils.c cartoGtUtils.c cartoLsqUtils.c cartoMatUtils.c -
	   cartoMemUtils.c cartoSortUtils.c cartoStrUtils.c -
	   cartoVicarProtos.h -
	-i carto_subs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cartoGridUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>

#include "cartoGridUtils.h"
#include "cartoMemUtils.h"
#include "cartoVicarProtos.h"

double gridtol;

/*  subroutine cartogetline:
finds the longest row or column of a grid as predicted by successive
neighbors; returns the length of the row -1 (number of areas); returns
0 for any failures, though there are none since you can always have a
row of two points.  the null9 case is for grids that have (-999,-999)
in them as don't cares */

void cartogetline(double *x,double*y,
	     int istart, int inc, int nmax, int bign, int* nfound, int* null9)
{
   int i,ix;
   double x1,y1,x2,y2,x3,y3,xp,yp,d1,d2,dbar;
   
   *nfound = 1;
   *null9 = 0;
   dbar = 0.0;
   x2 = x[istart];
   y2 = y[istart];
   x3 = x[istart+inc];
   y3 = y[istart+inc];
   if (x2<-998.99&&y2<-998.99&&x2>-999.01&&y2>-999.01) { *null9 = 1; return; }
   if (x3<-998.99&&y3<-998.99&&x3>-999.01&&y3>-999.01) { *null9 = 1; return; }
   for (i=0;i<nmax-1;i++)
      {
      x1 = x2; y1 = y2; x2 = x3; y2 = y3;
      ix = istart+(i+2)*inc;
      if (ix>=bign) break;
      x3 = x[ix];
      y3 = y[ix];
      if (x3<-998.99&&y3<-998.99&&x3>-999.01&&y3>-999.01) { *null9 = 1; return; }
      xp = 2*x2-x1;
      yp = 2*y2-y1;
      d1 = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
      d2 = sqrt((xp-x3)*(xp-x3)+(yp-y3)*(yp-y3));
      dbar = (dbar*(double)i+d1)/(double)(i+1);
      if (d2>gridtol*dbar) break;
      (*nfound)++;
      }
   return;
}

/*  subroutine getline2:
the case is for grids that have (-999,-999).  this routine tries to find
the first or last grid row that might end with -999 but finds the first
non-999 on the second row */

void getline2(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound)
{
   int i,ix,stage;
   double x1,y1,x2,y2;
   
   x2 = x[istart];
   y2 = y[istart];
   if (x2<-998.99&&y2<-998.99&&x2>-999.01&&y2>-999.01) 
      {
      *nfound = 0;
      return;
      }
   *nfound = 1;
   stage = 0;
   for (i=0;i<nmax;i++)
      {
      x1 = x2; y1 = y2;
      ix = istart+(i+1)*inc;
      if (ix>=bign) break;
      x2 = x[ix];
      y2 = y[ix];
      if (x2<-998.99&&y2<-998.99&&x2>-999.01&&y2>-999.01) stage = 1;
         else if (stage==1) { (*nfound)--; return;}
      (*nfound)++;
      }
   *nfound = 0;
   return;
}

/*  subroutine getline3:
the case is for grids that have (-999,-999).  this routine tries to find
the first or last grid row that might end with -999 but finds the first
non-999 on the second row */

void getline3(double *x,double *y, int istart, int inc, int nmax, int bign, int *nfound)
{
   int i,ix,stage;
   double x1,y1,x2,y2;
   
   x2 = x[istart];
   y2 = y[istart];
   if (x2>-998.99||y2>-998.99||x2<-999.01||y2<-999.01) 
      {
      *nfound = 0;
      return;
      }
   *nfound = 1;
   stage = 0;
   for (i=0;i<nmax;i++)
      {
      x1 = x2; y1 = y2;
      ix = istart+(i+1)*inc;
      if (ix>=bign) break;
      x2 = x[ix];
      y2 = y[ix];
      if (x2>-998.99||y2>-998.99||x2<-999.01||y2<-999.01) stage = 1;
         else if (stage==1) { (*nfound)--; return;}
      (*nfound)++;
      }
   *nfound = 0;
   return;
}

/*  subroutine gridfill:
this routine assumes that the grid dimensions have been found and it is desired to
replace the (-999,-999) values with unique grid values avoiding inside-out triangles
the call is for a single line in one direction.  This routine is probably not needed
but is better in case of a plot.  You cannot be inside an inside-out triangle */

void gridfill(double *x,double *y, int istart, int inc, int nmax)
{
   int i;
   double x1,y1,x2,y2,x3,y3;
   
   x2 = x[istart];
   y2 = y[istart];
   x3 = x[istart+inc];
   y3 = y[istart+inc];
   for (i=0;i<nmax-1;i++)
      {
      x1 = x2; y1 = y2; x2 = x3; y2 = y3;
      x3 = x[istart+(i+2)*inc];
      y3 = y[istart+(i+2)*inc];
      if (x3<-998.99&&y3<-998.99&&x3>-999.01&&y3>-999.01&&
         (x2>-998.99||y2>-998.99||x2<-999.01||y2<-999.01)&&
         (x1>-998.99||y1>-998.99||x1<-999.01||y1<-999.01))
         {
         x3 = 2*x2-x1;
         y3 = 2*y2-y1;
         x[istart+(i+2)*inc] = x3;
         y[istart+(i+2)*inc] = y3;
         }
      }
   return;
}

/*  subroutine tgrid:
triangulates a grid, and the four extrapolated points in the last four
positions as generated by polygeov main */

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
	   int zgeom)
{
   int i,j,nah,nav,nahp,navp,ptr,n,opoint;
   double ctrx,ctry,x,y,fac;
   
   n = npoints-4;
   nah = gridnah;
   nahp = nah+1;
   navp = n/nahp;
   nav = navp-1;
   if (nahp*navp!=n) zmabend("grid has wrong number of points");
   *ntriang = nah*nav*2+(nah+nav)*2+4;
   *nlinret = (*ntriang*3)/2+2;
           
   mz_alloc1((unsigned char **)tcon1,*ntriang,4);
   mz_alloc1((unsigned char **)tcon2,*ntriang,4);
   mz_alloc1((unsigned char **)tcon3,*ntriang,4);
   
   /* 4 far points may be unsuitable, substitute these */
  
   ctrx = (ptx[0]+ptx[nah]+ptx[n-nah-1]+ptx[n-1])*0.25; 
   ctry = (pty[0]+pty[nah]+pty[n-nah-1]+pty[n-1])*0.25; 
   ptx[n] = ctrx+(ptx[0]+ptx[nah])*10.0;
   pty[n] = ctry+(pty[0]+pty[nah])*10.0;
   ptx[n+3] = ctrx-(ptx[0]+ptx[nah])*10.0;
   pty[n+3] = ctry-(pty[0]+pty[nah])*10.0;
   ptx[n+1] = ctrx+(ptx[nah]+ptx[n-1])*10.0;
   pty[n+1] = ctry+(pty[nah]+pty[n-1])*10.0;
   ptx[n+2] = ctrx-(ptx[nah]+ptx[n-1])*10.0;
   pty[n+2] = ctry-(pty[nah]+pty[n-1])*10.0;

   /* resolve the corner points */

   fac = 0.5;         /* 0 for linear, 1.0 for cubic, 0.5 for 1/2 each */
   for (i=0;i<4;i++)
      {
      x = ptx[n+i]; y = pty[n+i];
      optx[n+i] = csol[0]*x+csol[1]*y+csol[2]+fac*(csol[3]*x*y+
         csol[4]*x*x+csol[5]*y*y+csol[6]*x*x*x+csol[7]*x*x*y+
         csol[8]*x*y*y+csol[9]*y*y*y);
      if (!zgeom) opty[n+i] = csol[10]*x+csol[11]*y+csol[12]+fac*(csol[13]*x*y+
         csol[14]*x*x+csol[15]*y*y+csol[16]*x*x*x+csol[17]*x*x*y+
         csol[18]*x*y*y+csol[19]*y*y*y);
      }
   
   /* edge triangles to top outside point, note pointers are +1 */
    
   ptr = 0;
   opoint = n+1;
   for (i=0;i<nah;i++)
      {
      (*tcon1)[ptr] = opoint;
      (*tcon3)[ptr] = i+2;
      (*tcon2)[ptr++] = i+1;
      }
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = opoint+1;
   (*tcon2)[ptr++] = nahp;
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = 1;
   (*tcon2)[ptr++] = opoint+2;
   
   /* grid triangles, contents of arrays are fortran referenced (+1) */
   
   for (i=0;i<nav;i++)
      {
      for (j=0;j<nah;j++)
         {
         /* upper left triangle */
         (*tcon1)[ptr] = i*nahp+j+1;
         (*tcon3)[ptr] = i*nahp+j+2;
         (*tcon2)[ptr++] = (i+1)*nahp+j+1;
         /* lower right triangle */
         (*tcon1)[ptr] = i*nahp+j+2;
         (*tcon3)[ptr] = (i+1)*nahp+j+2;
         (*tcon2)[ptr++] = (i+1)*nahp+j+1;
         }
      /* triangles to outside side points */
      (*tcon1)[ptr] = (i+1)*nahp;
      (*tcon3)[ptr] = opoint+1;
      (*tcon2)[ptr++] = (i+2)*nahp;
      (*tcon1)[ptr] = i*nahp+1;
      (*tcon3)[ptr] = (i+1)*nahp+1;
      (*tcon2)[ptr++] = opoint+2;
      }
   
   /* edge triangles to bottom outside point */
   
   for (i=0;i<nah;i++)
      {
      (*tcon1)[ptr] = nav*nahp+i+1;
      (*tcon3)[ptr] = nav*nahp+i+2;
      (*tcon2)[ptr++] = opoint+3;
      }
   (*tcon1)[ptr] = opoint+1;
   (*tcon3)[ptr] = opoint+3;
   (*tcon2)[ptr++] = nahp*navp;
   
   (*tcon1)[ptr] = opoint+2;
   (*tcon3)[ptr] = nahp*nav+1;
   (*tcon2)[ptr++] = opoint+3;
   /*printf("triB3 %f %f\n %f %f\n%f %f \n",      useful code
     ptx[(*tcon1)[ptr-1]-1]+3388.0,pty[(*tcon1)[ptr-1]-1]+3722.0,
     ptx[(*tcon2)[ptr-1]-1]+3388.0,pty[(*tcon2)[ptr-1]-1]+3722.0,
     ptx[(*tcon3)[ptr-1]-1]+3388.0,pty[(*tcon3)[ptr-1]-1]+3722.0);*/
   
   return;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoGtUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#include "zvproto.h"

#include "cartoGtUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoVicarProtos.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

/*================================================================

int gtgetlab

gtgetlab gets a geotiff label into a string parameter.  It
mallocs a large buffer, reads the geotiff label, then mallocs
the string parameter to the exact size, copies the label, then
frees the large buffer.  A null string is returned for any
failure to read a geotiff label.  The user will usually change
to all caps for speedier key identification.

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. inp: char buf[];
	 VICAR parameter for file that contains GeoTIFF label
	 usually "inp"
      2. instance: int instance;
         which instance of the previous parm
      3. labelstr: char **labelstr;
	 (output) pointer to string containing the label; is
	 mallocked to the exact size of the string, plus
	 terminating 0. user will usually change to all caps.
      4. nl: int *nl;
	 (output) nl for case of VICAR image, -1 if not
      5. ns: int *ns;
	 (output) ns for case of VICAR image, -1 if not
*/

int gtgetlab( char * inp, int instance, char ** labelstr, int * nl, int * ns )
{
   int i,status,geounit;
   int maxlen,nelement,len;
   char *buf,valformat[9],vformat[9];
   char svalue[133],key[33];
   
   /* malloc large temporary buffer for reading the string */
   
   mz_alloc1((unsigned char **)&buf,1000001,1);
   
   /* open file */
   
   status = zvunit(&geounit,inp,instance,NULL);
   status = zvopen(geounit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA",NULL);
      
   strcpy(buf,"");
   do
      {
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      if (strcmp(key,"NL")==0)
         {
	   status=zlget(geounit,"SYSTEM",key,(char*)nl,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      if (strcmp(key,"NS")==0)
         {
	   status=zlget(geounit,"SYSTEM",key,(char*)ns,
            "ERR_ACT","SA","FORMAT","INT",NULL);
         }
      status=zlinfo(geounit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(geounit,"PROPERTY",key,svalue,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         else
            status=zlget(geounit,"PROPERTY",key,svalue,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133,NULL);
         strcat(buf,key);
         strcat(buf,"=");
         strcat(buf,svalue);
         strcat(buf,"\n");
         }
      }
   while (1);
   status = zvclose(geounit,NULL);
   
   /* resave in smaller buffer */
   
   len = strlen(buf);
   if (((*labelstr)=(char *)malloc(len+1))==NULL) zmabend("malloc failed");
   strcpy(*labelstr,buf);
   
   free(buf);
   if (strlen(*labelstr)<1) return 0; else return 1;
}

/*================================================================

int invertmap

invertmap calculates the inverse of a six-vector double precision
map.

function return:
     ier from the dgauss call (see dgauss)

arguments:
      1. map: double[6] map;
	 (input) coefficients to convert pixel to coord OR
	 COORD TO PIXEL
      2. invmap: double[6] invmap;
	 (output) coefficients to convert the other way
*/

int invertmap( double * t, double * tinv)
{
   int i,ier;
   double work[9];
   
   for (i=0;i<2;i++)
      {
      work[0] = t[2];
      work[1] = 100.0*t[1]+t[2];
      work[2] = 100.0*t[0]+t[2];
      work[3] = t[5];
      work[4] = 100.0*t[4]+t[5];
      work[5] = 100.0*t[3]+t[5];
      work[6] = 1.;
      work[7] = 1.;
      work[8] = 1.;
      if (i==0)
         {
         tinv[0] = 0.0;
         tinv[1] = 0.0;
         tinv[2] = 100.0;
         }
      else
         {
         tinv[3] = 0.0;
         tinv[4] = 100.0;
         tinv[5] = 0.0;
         }
      dgauss(work,&tinv[i*3],3,1.e-14,&ier);
      }
   
   return ier;
}

/*================================================================

int geofix

geofix translates label coordinates into a linear transformation
vectors that can be used for VICAR pixel-to-map or map-to-pixel
conversions.  If the file is a VICAR image then the mapping of 
the corner points is also returned (the (1,1) and (nline,nsamp)
centers of the corner pixels).

The convention for the transforms is (line,samp) -> (East,North)
for the map and (East,North) -> (line,samp) for the invmap
for convenience in working with VICAR.

Note that VICAR pixel referencing is different than GeoTIFF
pixel referencing (both "area" and "point"/"post" types).

function return:
     int, 1 if successful, 0 if cannot find info in label

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. map: double[6] map;
	 (output) coefficients to convert pixel to coord
      3. invmap: double[6] invmap;
	 (output) coefficients to convert coord to pixel
      4. nl: int nl;
	 (input) number of lines in vicar image to calc corner
      5. ns: int ns;
	 (input) number of samples in vicar image to calc corner
      6. corner: double[4] corner;
	 (output) the mapping of the corners (.5,.5,nl+.5,ns+.5)
	 if the file is a VICAR image, else zero
*/

int geofix( char * labelstr, double * map, double * invmap, int nl, int ns, double * corner)
{
   int i,vtype;
   int ireturn;
   char *p;
   double tie[4],voff,ddummy,scale[2];
   
   for (i=0;i<6;i++) { map[i] = 0.; invmap[i] = 0.; }
   map[0] = 1.; map[5] = 1.;
   invmap[0] = 1.; invmap[5] = 1.;
   ireturn = 1;
   
   vtype = nl!=(-1);
      
   /* read the model transformation or get the scale, etc.  Note
   reversal of matrix from samp-line to line-samp */
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) { ireturn = 0; goto closem; }
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
     
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   if (vtype)
      {
      corner[0] = 0.5*map[0]+0.5*map[1]+map[2];
      corner[1] = 0.5*map[3]+0.5*map[4]+map[5];
      corner[2] = ((double)nl+0.5)*map[0]+((double)ns+0.5)*map[1]+map[2];
      corner[3] = ((double)nl+0.5)*map[3]+((double)ns+0.5)*map[4]+map[5];
      }
   else for (i=0;i<4;i++) corner[i] = (double)0.0;
   
   invertmap(map,invmap);
   
   closem:
   
   /*for (i=0;i<6;i++) printf("map trans[%d] %f\n",i+1,map[i]);
   for (i=0;i<6;i++) printf("inv trans[%d] %f\n",i+1,invmap[i]);*/
   
   return ireturn;
}

/*================================================================

int gtrect

gtrect tests whether the mapping is "rectangular".  This means 
that the GeoTIFF label has the keyword MODELPIXELSCALETAG or that
it has the keyword MODELTRANSFORMATIONTAG and the upper left part
of the transformation has two 0.0 in diagonal formation.  To allow
for a slight inaccuracy in a calculated transformation, a 
parameter eps is provided for values very close to 0. It actually
ratios with the main terms, see code below.

function return:
     int, 1 if mapping is rectangular, else 0

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. eps: double eps;
	 (input) tolerance about zero for the off-diagonals
	 to still consider the mapping rectangular. It is a
	 ratio to the largest term.  suggest 1.0e-12.
*/

int gtrect( char * labelstr, double eps )
{
   char *p;
   double map[4],ddummy,largest,thresh;
   
   /* read the model transformation or read if scale */
   
   p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
   if (p!=0)
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      return 1;
      }
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[0] = fabs(ms_dnum(&p)); p++;
      map[1] = fabs(ms_dnum(&p)); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = fabs(ms_dnum(&p)); p++;
      map[3] = fabs(ms_dnum(&p));
      largest = MAX(map[0],map[1]);
      largest = MAX(largest,map[2]);
      largest = MAX(largest,map[3]);
      thresh = eps*largest;
      if (map[0]<thresh&&map[3]<thresh) return 1;
      if (map[1]<thresh&&map[2]<thresh) return 1;
      }
   return 0;
}

int gtcompval( char * p1, char * p2 )
{
   int charseq,stcomment;
   char *p,*q;
   double dval1,dval2,epsl,epsu;
   
   epsl = 1.0-1.0e-12;
   epsu = 1.0+1.0e-12;
   p = p1; q = p2;
   charseq = 0;
   stcomment = 0;
   do
      {
      while (*p==' ') p++;
      while (*q==' ') q++;
      if (*p=='\n') return 1;
      else if (*p==(char)0) return 1;
      else if (stcomment&&*p=='(') return 1;
      else if (isalpha(*p)||(isdigit(*p)&&charseq==1))
         {
         charseq = 1;
         stcomment = 1;
         if (*q==*p) { p++; q++; }
         else return 0;
         }
      else if (*p=='('||*p==')'||*p==','||*p==')'||*p=='=')
         {
         charseq = 0;
         if (*q=='('||*q==')'||*q==','||*q==')'||*q=='=') { p++; q++; }
            else return 0;
         }
      else if (isdigit(*p)||*p=='.'||*p=='-')
         {
         stcomment = 1;
         dval1 = ms_dnum(&p); 
         dval2 = ms_dnum(&q);
         if (dval1<0.0) { dval1 = -dval1; dval2 = -dval2; }
         if (dval1<dval2*epsl||dval1>dval2*epsu) return 0;
         }
      else return 0;
      }
   while (1);
}

/*================================================================

int gtmapcom

gtmapcom tests whether the two mappings are compatible.  This is
defined as having the same values for a list of attributes that
pertain to mappings.  The list is taken from the GeoTIFF spec
rev 1.0.  If the "value" contains parenthetical material, it is
not required to be the same.  Vector values are required to be
the same.  There is a tolerance of 1 part in 1e-12 on all numeric
values.

If one of the listed attributes is present in one label it must
also be present in the other label.  This prevents default values
from making two labels incompatible, or for alternate keywords
to do the same.

function return:
     int, 1 if mappings are compatible, else 0

arguments:
      1. labelstr1: char *labelstr;
	 (input) string containing the first GeoTIFF label
      1. labelstr2: char *labelstr2;
	 (input) string containing the second GeoTIFF label
*/

int gtmapcom( char * labelstr1, char * labelstr2 )
{
#define numattrib 45
   int iattrib,status;
   char *p1,*p2;
   char attrib[numattrib][34] = {"GTRASTERTYPEGEOKEY","GTMODELTYPEGEOKEY",
     "GEOGRAPHICTXGEOKEY","GEOGGEODETICDATUMGEOKEY","GEOGPRIMEMERIDIANGEOKEY",
     "GEOGLINEARUNITSGEOKEY","GEOGLINEARUNITSIZEGEOKEY","GEOGANGULARUNITSGEOKEY",
     "GEOGANGULARUNITSIZEGEOKEY","GEOGELLIPSOIDGEOKEY","GEOGSEMIMAJORAXISGEOKEY",
     "GEOGSEMIMINORAXISGEOKEY","GEOGINVFLATTENINGGEOKEY","GEOGAZIMUTHUNITSGEOKEY",
     "GEOGPRIMEMERIDIANLONGGEOKEY","PROJECTEDCSTYPEGEOKEY","PROJECTIONGEOKEY",
     "PROJCOORDTRANSGEOKEY","PROJLINEARUNITSGEOKEY","PROJLINEARUNITSIZEGEOKEY",
     "PROJSTDPARALLEL1GEOKEY","PROJSTDPARALLEL2GEOKEY","PROJNATORIGINLONGGEOKEY",
     "PROJNATORIGINLATGEOKEY","PROJFALSEEASTINGGEOKEY","PROJFALSENORTHINGGEOKEY",
     "PROJFALSEORIGINLONGGEOKEY","PROJFALSEORIGINLATGEOKEY","PROJFALSEORIGINEASTINGGEOKEY",
     "PROJFALSEORIGINNORTHINGGEOKEY","PROJCENTERLONGGEOKEY","PROJCENTERLATGEOKEY",
     "PROJCENTEREASTINGGEOKEY","PROJCENTERNORTHINGGEOKEY","PROJSCALEATNATORIGINGEOKEY",
     "PROJSCALEATCENTERINGEOKEY","PROJAZIMUTHANGLEGEOKEY","PROJSTRAIGHTVERTPOLELONGGEOKEY",
     "PROJSTDPARALLELGEOKEY","PROJORIGINLONGGEOKEY","PROJORIGINLATGEOKEY",
     "PROJSCALEATORIGINGEOKEY","VERTICALCSTYPEGEOKEY","VERTICALDATUMGEOKEY",
     "VERTICALUNITSGEOKEY"};
   /*int numattrib = 45;*/
   
   /* loop over attributes in string 1, finding match in string 2,
   the second part of the loop does the reverse check */
   
   for (iattrib=0;iattrib<numattrib;iattrib++)
      {
      p1 = ms_find(labelstr1,attrib[iattrib]);
      if (p1!=0)
         {
         p2 = ms_find(labelstr2,attrib[iattrib]);
         if (p2==0)
            {
            printf("Missing attribute in label 2: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      p2 = ms_find(labelstr2,attrib[iattrib]);
      if (p2!=0)
         {
         p1 = ms_find(labelstr1,attrib[iattrib]);
         if (p1==0)
            {
            printf("Missing attribute in label 1: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      }
   
   return 1;
}

/*================================================================

int gtgetrot

gtgetrot gets the rotation number of the GeoTIFF label.  A standard
VICAR rotation is 1.  See the help PDF for VICAR routine GTLIST for
information on the other rotations.

function return:
     int, a number from 0 to 8 designating the rotation relative to
     an (East,North) coordinate system.

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
*/

int gtgetrot( char * labelstr )
{
   int rot=0;
   char *p;
   double map[6],tie[4],scale[2],ddummy,xmain,xcross,xtot,voff;
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;     /* 0.5 is the default also */
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      map[1] = ms_dnum(&p); p++;
      map[0] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[2] = ms_dnum(&p)-(map[0]+map[1])*voff; p++;
      map[4] = ms_dnum(&p); p++;
      map[3] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      map[5] = ms_dnum(&p)-(map[3]+map[4])*voff;
      }
   else
      {
      p = ms_find(labelstr,"MODELTIEPOINTTAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      tie[0] = ms_dnum(&p); p++;
      tie[1] = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      tie[2] = ms_dnum(&p); p++;
      tie[3] = ms_dnum(&p);
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      scale[0] = ms_dnum(&p); p++;
      scale[1] = ms_dnum(&p);
      
      map[0] = 0.0;
      map[1] = scale[0];
      map[2] = tie[2]-map[1]*(tie[0]+voff);
      map[3] = -scale[1];
      map[4] = 0.0;
      map[5] = tie[3]-map[3]*(tie[1]+voff);
      }
   
   xmain = fabs(map[0])+fabs(map[4]);
   xcross = fabs(map[1])+fabs(map[3]);
   xtot = xmain+xcross;
   
   if (xmain/xtot<1.e-10)
      {
      if (map[1]>0.0&&map[3]>0.0) rot = 5;
      else if (map[1]>0.0&&map[3]<0.0) rot = 1;
      else if (map[1]<0.0&&map[3]>0.0) rot = 3;
      else if (map[1]<0.0&&map[3]<0.0) rot = 7;
      }
   else if (xcross/xtot<1.e-10)
      {
      if (map[0]>0.0&&map[4]>0.0) rot = 0;
      else if (map[0]>0.0&&map[4]<0.0) rot = 6;
      else if (map[0]<0.0&&map[4]>0.0) rot = 4;
      else if (map[0]<0.0&&map[4]<0.0) rot = 2;
      }
   else rot = -1;
   
   return rot;
}

/*================================================================

gtreplab

gtreplab writes a GeoTIFF label into the property part of VICAR label

function return : void

arguments:
      1. fileparm: input, char *fileparm;
         name of the file to put the label, usually "INP"
      2. nfile: input, int nfile;
         which file of fileparm, set to 1 if only one file
      3. labstr: input, char *labstr;
         string containing new GeoTIFF label, comma separated,
         see GTGEN document for format details, and TSTGTGEN.PDF
         for examples
      4. add: input, int add;
         0 - then the old label is deleted and labstr
             becomes the new label
         1 - then the old label is kept and added to,
      5. replflag: input, int replflag;
         0 - no processing of coord-pixel mapping
         1 - coord-pixel mapping replaced w/ next three params
         2 - coord-pixel mapping replaced w/ next three params, but
             MODELPIXELSCALETAG and MODELTRANSFORMATIONTAG type labels
             are swapped due to rotation
      6. tinv: input, double tinv[6];
         will recalculate every MODELTIEPOINTTAG using this transf
      7. scalestr: input, char *scalestr;
         will replace an occurrence of MODELPIXELSCALETAG with this
      8. transstr: input, char *transstr;
         will replace an occurrence of MODELTRANSFORMATIONTAG with this
      
*/

void gtreplab( char * fileparm, int nfile, char * labstr, int add, int replflag, double * tinv, char * scalestr, char * transstr )
{
   int geounit,nelement,maxlen,status,n;
   char key[33],valformat[9],temp1[100],temp2[133],buf[133];
   char ttemp1[100],ttemp2[133];
   char *p,*q,*p1,*q1,tbuf[30];
   double dummy,coord1,coord2,pline,psamp;
   
   status=zvunit(&geounit,fileparm,nfile,NULL);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA",NULL);
   if (add!=1) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ",NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
         &maxlen,&nelement,"PROPERTY","GEOTIFF",
         "ERR_ACT"," ",NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      
      if (add==2&&strcmp(key,"MODELPIXELSCALETAG")!=0
         &&strcmp(key,"MODELTIEPOINTTAG")!=0
         &&strcmp(key,"MODELTRANSFORMATIONTAG")!=0) continue;
      
      status=zldel(geounit,"PROPERTY",key,
         "ERR_ACT","SA","PROPERTY","GEOTIFF",NULL);
      }
   while (1);
   p = labstr;
   q = p+strlen(labstr);
   do
      {
      n = grab(p,'=',temp1);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      n = grab(p,'\n',temp2);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      
      if (replflag>0&&strcmp(temp1,"MODELPIXELSCALETAG")==0)
         {
         if (replflag==1) strcpy(temp2,scalestr);
         else
            {
            strcpy(temp1,"MODELTRANSFORMATIONTAG");
            strcpy(temp2,transstr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTRANSFORMATIONTAG")==0)
         {
         if (replflag==1) strcpy(temp2,transstr);
         else
            {
            strcpy(ttemp1,"MODELPIXELSCALETAG");
            strcpy(ttemp2,scalestr);
            status=zladd(geounit,"PROPERTY",ttemp1,ttemp2,
               "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
               "PROPERTY","GEOTIFF",NULL);
            /* add a tiepoint, in case transformation label didn't have one */
            strcpy(temp1,"MODELTIEPOINTTAG");
            p1 = &temp2[1];
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            coord1 = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            dummy = ms_dnum(&p1); p1++;
            coord2 = ms_dnum(&p1); p1++;
            psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
            pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
            nicelen("(",psamp,buf);
            nicelen(",",pline,tbuf);
            strcat(buf,tbuf);
            strcat(buf,",0.0");
            nicelen(",",coord1,tbuf);
            strcat(buf,tbuf);
            nicelen(",",coord2,tbuf);
            strcat(buf,tbuf);
            strcat(buf,",0.0)");
            strcpy(temp2,buf);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTIEPOINTTAG")==0)
         {
         p1 = &temp2[1];
         dummy = ms_dnum(&p1); p1++;
         dummy = ms_dnum(&p1); q1 = p1; p1++;
         dummy = ms_dnum(&p1); p1++;
         coord1 = ms_dnum(&p1); p1++;
         coord2 = ms_dnum(&p1);
         psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
         pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
         nicelen("(",psamp,buf);
         nicelen(",",pline,tbuf);
         strcat(buf,tbuf);
         strcat(buf,q1);
         strcpy(temp2,buf);
         }
      
      status=zladd(geounit,"PROPERTY",temp1,temp2,
         "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
         "PROPERTY","GEOTIFF",NULL);
      }
   while (p<q);
   zvclose(geounit, NULL);
   return;
}

/*================================================================

int gtgetscl

gtgetscl gets the scale factors of the GeoTIFF label.  There are two
types of scale that can be returned:

The first is for the four rotations that can be represented by the
MODELTIEPOINTTAG-MODELPIXELSCALETAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips".  The
odd thing about this scale is that the increasing "lines" coordinate
to the South is denoted by a positive scale factor.

The second is for the four rotations that have to be represented
by the MODELTRANSFORMATIONTAG combination.  The geographic 
coordinate X which is usually East will have the first scale number
and the Y (North) will have the second.  The plus and minus signs
on these determine the four rotations which are all "flips" of a
single ninety degree rotate.

function return:
     void

arguments:
      1. labelstr: char *labelstr;
	 (input) string containing the GeoTIFF label
      2. sctype: int *sctype;
	 (output) type of scale factors (see above)
	 1 - from MODELPIXELSCALETAG
	 2 - from MODELTRANSFORMATIONTAG
      3. scale1: double *scale1;
	 (output) scale factor 1
      4. scale2: double *scale2;
	 (output) scale factor 2
*/

void gtgetscl( char * labelstr, int * sctype, double * scale1, double * scale2 )
{
   char *p;
   double ddummy;
   
   p = ms_find(labelstr,"MODELTRANSFORMATIONTAG=(");
   if (p!=0)
      {
      *sctype = 2;
      ddummy = ms_dnum(&p); p++;
      *scale1 = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      ddummy = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p); p++;
     }
   else
      {
      p = ms_find(labelstr,"MODELPIXELSCALETAG=(");
      if (p==0) zmabend("Problem with GeoTIFF label");
      *sctype = 1;
      *scale1 = ms_dnum(&p); p++;
      *scale2 = ms_dnum(&p);
      }
    
   return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLsqUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <string.h>
#include <math.h>
#include <stdlib.h>

#include "zvproto.h"

#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoVicarProtos.h"

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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoMatUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "zvproto.h"

#include "cartoMatUtils.h"
#include "cartoMemUtils.h"
#include "cartoSortUtils.h"
#include "cartoVicarProtos.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
static int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
static double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
  cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
  ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
static short int *vgroup;

int dceiling( double x )
{
   /* not a true ceiling routine, need numbers near
   floor to go to floor*/
   
   if (x>0) return((int)(x+0.999999));
   else     return((int)(x-0.000001));
}

double xzprod( double x1, double y1, double x2, double y2 )
{
   return(x2*y1-x1*y2);
}

/* signed triangular area, for polygon use xp,yp as fixed point */
/* in (E,N) coordinates,        clockwise is positive area */
/* in (N,E) coordinates, counterclockwise is positive area */

double triarea( double xp, double yp, double x1, double y1, double x2, double y2 )
{
   return(0.5*(xp*y1-x1*yp+x1*y2-x2*y1+x2*yp-xp*y2));
}

void lin2( double * a, double * b, double * x, double eps )
{
   /* for case where the first equation is not null */
   /* provides a solution for the singular case and causes x[1] =-1 */
   /* stored by column in a */

   if (fabs(a[0])>=fabs(a[1]))
      {
      if (fabs(a[0])<eps)
         {
         x[0] = 0.5;
         x[1] = 0.5;
         return;
         }
      a[2] /= a[0]; b[0] /= a[0];
      a[3] -= a[2]*a[1]; b[1] -= b[0]*a[1];
      if (fabs(a[3])>eps)
	 {
	 x[1] = b[1]/a[3];
	 x[0] = b[0]-x[1]*a[2];
	 }
      else { x[1] = -1.; x[0] = b[0]+a[2]; }
      }
   else
      {
      a[3] /= a[1]; b[1] /= a[1];
      a[2] -= a[3]*a[0]; b[0] -= b[1]*a[0];
      if (fabs(a[2])>eps)
	 {
	 x[1] = b[0]/a[2];
	 x[0] = b[1]-x[1]*a[3];
	 }
      else { x[1] = -1.; x[0] = b[1]+a[3]; }
      }
   return;
}

void segxseg( double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double * w )
{
   /* The first segment is x1,y1 to x2,y2; the second segment is   */
   /* x3,y3 to x4,y4; the return w[0] is the index of the crossing */
   /* of the first segment; 0. is at x1,y1; 1. is at x2,y2 other   */
   /* values are linearly interpolated or extrapolated; w[1] is    */
   /* the same for the second segment                              */

   double aa[4],bb[2];

   aa[0] = x2-x1; aa[1] = y2-y1;
   aa[2] = x3-x4; aa[3] = y3-y4;
   bb[0] = x3-x1; bb[1] = y3-y1;
   lin2(aa,bb,w,1.e-8);
   return;
}


void insert_seg( int jj, int * ccount, double * p4max, double xbig, double ybig, double xjbig, double yjbig, int wchcall )
{
   int ccdel[VORMAXPOLY];
   int i,k,tcross,tdelete,srong,niter,iter,kmin=0,kc,kcmin=0,whch,irep;
   int ptr,z12,lcross,sccx[10],srl[10];
   double xm,ym,xbisv,ybisv,xbis,ybis,tw[2],xzprod();
   double xcr,ycr,ccrad,pmax,z2min=0,rat;
   double z1,z2=0,sx[10],sy[10],dmin,px,py,dot,hyp,dist,thet,dx,dy;
   
   /*if(bgg)printf("enter insert_seg jj %d\n",jj);*/
   xm = (xbig+xjbig)*.5;
   ym = (ybig+yjbig)*.5;
   xbisv = ym-yjbig;
   ybisv = xjbig-xm;
   xbis = xbisv+xm; ybis = ybisv+ym;
   /*if(bgg)printf("xm,ym %f %f\n",xm,ym);
   if(bgg)printf("xbis,ybis %f %f  ",xbis,ybis);
   if(bgg)printf("xbisv,ybisv %f %f\n",xbisv,ybisv);*/
   tcross = 0; tdelete = 0;
   for (k=0;k<*ccount;k++)
      {
      segxseg(xm,ym,xbis,ybis,cc1x[k],cc1y[k],cc2x[k],cc2y[k],tw);
      /*if(bgg)printf("tw %f %f\n",tw[0],tw[1]);*/
      if (tw[1]>=0. && tw[1]<=1.) lcross = 1; else lcross = 0.;
      xcr = xbisv*tw[0]+xm;
      ycr = ybisv*tw[0]+ym;
      /*if(bgg)printf("xcr,ycr %f %f\n",xcr,ycr);*/
      z1 = xzprod(cc1x[k]-xm,cc1y[k]-ym,xbisv,ybisv);
      z2 = xzprod(cc2x[k]-xm,cc2y[k]-ym,xbisv,ybisv);
      /*if(bgg)printf("z1,z2 %f %f\n",z1,z2);*/
      srong = 0; z12 = 1;
      if (z1<0.) srong++;
      if (z2<0.) { srong++; z12 = 2; }
      /*if(bgg)printf("a-srong,lcross %d %d\n",srong,lcross);*/
      if (lcross==0 && srong==1) srong = 0;
      if (lcross==1 && srong!=1)
	 {
	 if (fabs(z1)<fabs(z2)) z12 = 1+srong/2; else z12 = 2-srong/2;
	 srong = 1;
	 /*if(bgg)printf("end point enforced\n");*/
	 }
      if (lcross==1)
	 {
	 sx[tcross] = xcr;
	 sy[tcross] = ycr;
	 sccx[tcross] = k;
	 srl[tcross] = z12; tcross++;
	 }
      ccdel[k] = srong;
      if (srong==2) tdelete++;
      /*if(bgg)printf("b-srong,lcross %d %d\n",srong,lcross);*/
      }
   /*if(bgg)printf("tcross,tdelete %d %d\n",tcross,tdelete);*/
   if (tcross<2 && tdelete==0) return;
   if (tcross<2)
      {
      /*if(bgg)printf("add polygon cross\n");*/
      niter = 2-tcross;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (k=0;k<*ccount;k++)
	    {
	    if (ccdel[k]==1) continue;
	    px = cc1x[k]-xbis;
	    py = cc1y[k]-ybis;
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       z2 = xzprod(cc2x[k]-px,cc2y[k]-py,xbisv,ybisv);
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kmin = k; z2min = z2;
	    }
	 ccdel[kmin] = 1;
	 sx[tcross] = cc1x[kmin];
	 sy[tcross] = cc1y[kmin];
	 sccx[tcross] = kmin;
	 if (z2min<0) srl[tcross] = 2; else srl[tcross] = 1;
	 tcross++;
	 }
      }
   if (tcross>2)
      {
      /*if(bgg)printf("elim polygon cross\n");*/
      niter = tcross-2;
      for (iter=0;iter<niter;iter++)
	 {
	 dmin = 1.e35;
	 for (kc=0;kc<tcross;kc++)
	    {
	    k = sccx[kc];
	    whch = srl[kc];
	    if (whch==1)
	       { px = cc1x[k]-xbis; py = cc1y[k]-ybis; }
	    else
	       { px = cc2x[k]-xbis; py = cc2y[k]-ybis; }
	    dist = fabs(px)+fabs(py);
	    if (dist>1.e-7)
	       {
	       dot = px*xbisv+py*ybisv;
	       hyp = sqrt((double)(px*px+py*py))*
		     sqrt((double)(xbisv*xbisv+ybisv*ybisv));
	       rat = dot/hyp; if (rat>1.) rat = 1.; if (rat<(-1.)) rat = -1.;
	       thet = acos((double)rat);
	       dist = hyp*sin((double)thet);
	       }
	    if (dist>dmin) continue;
	    dmin = dist; kcmin = kc; kmin = k;
	    }
	 ccdel[kmin] = 0;
	 for (kc=kcmin;kc<tcross;kc++)
	    {
	    sx[kc] = sx[kc+1];
	    sy[kc] = sy[kc+1];
	    sccx[kc] = sccx[kc+1];
	    srl[kc] = srl[kc+1];
	    }
	 tcross--;
	 }
      }
   if (tcross!=2) zmabend("tcross.ne.2");
   if (sx[0]==sx[1] && sy[0]==sy[1]) return;
   for (irep=0;irep<2;irep++)
      {
      whch = srl[irep];
      ptr = sccx[irep];
      if (whch==1)
	 {
	 cc1x[ptr] = sx[irep];
	 cc1y[ptr] = sy[irep];
	 }
      else
	 {
	 cc2x[ptr] = sx[irep];
	 cc2y[ptr] = sy[irep];
	 }
      }
   ptr = 0;
   for (k=0;k<*ccount;k++)
      {
      cc1x[ptr] = cc1x[k]; cc1y[ptr] = cc1y[k];
      cc2x[ptr] = cc2x[k]; cc2y[ptr] = cc2y[k];
      ccdel[ptr] = ccdel[k];
      switch(wchcall)
	 {
	 case 3: ccrx[ptr] = ccrx[k]; ccry[ptr] = ccry[k];
		 ccgrp[ptr] = ccgrp[k];
	 case 2: ccjj[ptr] = ccjj[k]; break;
	 /*case 1: break;*/
	 }
      if (ccdel[k]<2) ptr++;
      }
   z1 = xzprod(sx[0]-xbig,sy[0]-ybig,sx[1]-xbig,sy[1]-ybig);
   if (z1<0) whch = 0; else whch = 1;
   cc1x[ptr] = sx[whch]; cc1y[ptr] = sy[whch];
   cc2x[ptr] = sx[1-whch]; cc2y[ptr] = sy[1-whch];
   switch(wchcall)
      {
      case 3: ccrx[ptr] = xjbig; ccry[ptr] = yjbig; ccgrp[ptr] = vgroup[jj];
      case 2: ccjj[ptr] = jj; break;
      /*case 1: break;*/
      }
   *ccount = ptr+1;
   pmax = 0.;
   for (i=0;i<*ccount;i++)
      {
      dx = cc1x[i]-xbig; dy = cc1y[i]-ybig;
      ccrad = dx*dx+dy*dy;
      if (ccrad>pmax) pmax = ccrad;
      }
   *p4max = 4.*pmax;
   return;
}

void thiessen( int npoints, int * nlinret, double reject, double skinny, int abendi, double * ptx, double * pty, int * ntriang, int ** tcon1, int ** tcon2, int ** tcon3 )
{

   int *bptr,**hash1,**hash2,**tc,bjj[VORMAXPOLY],blink[VORMAXPOLY];
   double *bufio,*buf;

   int np,i,j,npts,linct,ip1,hlen1,hlen2;
   int topstop,botstop,dirj,jj,k,ptr,jsave,jdup,trict,ibig;
   int t0,t1,t2,t3,tt3,tt,h12,h123,l,triptr,ttptr,ccount,temp;
   double dx,dy,dx2,dist2,xlink,ylink,low1,low2,upp1,upp2,tstarea,diam;
   double tw[2],p4max,xbig,ybig,xjbig,yjbig;
   double triarea();

   bgg = 0; bggset = -1;
   
   
   /* read the data */

   np = npoints*2;
   mz_alloc1((unsigned char **)&bufio,np,8);
   mz_alloc1((unsigned char **)&buf,np,8);
   mz_alloc1((unsigned char **)&bptr,npoints,4);
   for (i=0;i<10000;i++)
      {
      hlen1 = (npoints*4+1000)+i;
      if (hlen1%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen1%j==0) break;
	 if (j==37) goto rnd1;
	 }
      }
   rnd1: /*if(bgg)printf("hash length = %d\n",hlen1);*/
   mz_alloc2((unsigned char ***)&hash1,4,hlen1,4);
   for (i=0;i<hlen1;i++) hash1[0][i] = 0;

   low1 = 1.e20; low2 = 1.e20; upp1 = -1.e20; upp2 = -1.e20;
   for (i=0;i<npoints;i++)
      {
      bufio[2*i] = ptx[i];
      bufio[2*i+1] = pty[i];
      low1 = MIN(low1,ptx[i]);
      low2 = MIN(low2,pty[i]);
      upp1 = MAX(upp1,ptx[i]);
      upp2 = MAX(upp2,pty[i]);
      }
   diam = 5*((upp1-low1)+(upp2-low2));
   low1 = low1-diam;
   low2 = low2-diam;
   upp1 = upp1+diam;
   upp2 = upp2+diam;

   /* apply the voronoi routine */

   npts = np/2; ccount = 0; linct = 0; trict = 0;
   for (i=0;i<np;i++) buf[i] = bufio[i];
   for (i=0;i<npts;i++) bptr[i] = i+1;
   
   sort88(buf,bptr,npts);
   sortrec88(bufio,bptr,npts);
   
   ptr = 0;
   for (i=0;i<npts;i++)
      {
      if (bufio[i*2]<low1 || bufio[i*2]>upp1 ||
	 bufio[i*2+1]<low2 || bufio[i*2+1]>upp2)
	 { printf("outside point removed\n"); continue; }
      if (i!=0) if (bufio[i*2]==buf[ptr*2-2] &&
	 bufio[i*2+1]==buf[ptr*2-1])
	    {
	    if (abendi) zmabend("duplicate point abend");
	    printf("duplicate point rejected\n");
	    continue;
	    }
       for (j=1;ptr-j>=0;j++)
	 {
	 if (fabs(bufio[i*2]-buf[(ptr-j)*2])>reject) break;
	 if ((fabs(bufio[i*2]-buf[(ptr-j)*2])+
	    fabs(bufio[i*2+1]-buf[(ptr-j)*2+1]))<reject)
	    {
	    printf("i,j,ptr %d %d %d\n",i,j,ptr);
	    printf("bufio[i*2],buf[(ptr-j)*2] %12.3f %12.3f\n",
	                 bufio[i*2],buf[(ptr-j)*2]);
	    printf("bufio[i*2+1],buf[(ptr-j)*2+1] %12.3f %12.3f\n",
	                 bufio[i*2+1],buf[(ptr-j)*2+1]);
	                 
	    if (abendi) zmabend("close point abend");
	    printf("close point rejected\n");
	    goto clpt;
	    }
	 }
      buf[ptr*2] = bufio[i*2];
      buf[ptr*2+1] = bufio[i*2+1]; bptr[ptr++] = bptr[i];
      clpt: continue;
      }
   free(bufio);
   if (ptr==0) return; npts = ptr;
   for (ibig=0;ibig<npts;ibig++)
      {
      if (ibig%10000==9999) printf("%d pts processed\n",ibig);
      xbig = buf[ibig*2];
      ybig = buf[ibig*2+1];
      /*if (bggset>=0) printf("xbig,ybig %f %f\n",xbig,ybig);*/
      if (ibig==bggset) bgg = 1; else bgg = 0;
      cc1x[0] = upp1; cc1y[0] = upp2; cc2x[0] = low1; cc2y[0] = upp2;
      cc1x[1] = upp1; cc1y[1] = low2; cc2x[1] = upp1; cc2y[1] = upp2;
      cc1x[2] = low1; cc1y[2] = low2; cc2x[2] = upp1; cc2y[2] = low2;
      cc1x[3] = low1; cc1y[3] = upp2; cc2x[3] = low1; cc2y[3] = low2;
      ccjj[0] = ibig; ccjj[1] = ibig;
      ccjj[2] = ibig; ccjj[3] = ibig;
      p4max = upp1+upp2-low1-low2; p4max = p4max*p4max; ccount = 4;
      topstop = 0; botstop = 0;
      for (j=0;j<2*npts;j++)
	 {
	 dirj = j%2;
	 jj = ibig+(dirj*2-1)*((j+2)/2);
	 /*if(bgg)printf("a-ibig,j,jj %d %d %d\n",ibig,j,jj);
	 if(bgg)printf("dirj,tops,bots %d %d %d\n",dirj,topstop,botstop);*/
	 if (jj<0 || jj>=npts) continue;
	 if (dirj==0 && topstop) continue;
	 if (dirj==1 && botstop) continue;
	 xjbig = buf[jj*2];
	 yjbig = buf[jj*2+1];
	 dx = xjbig-xbig; dy = yjbig-ybig; dx2 = dx*dx;
	 dist2 = dx2+dy*dy;
	 /*if(bgg)printf("xjbig,yjbig,dist2 %f %f %f\n",xjbig,yjbig,dist2);*/
	 if (dist2<p4max)
	    insert_seg(jj,&ccount,&p4max,xbig,ybig,xjbig,yjbig,2);
	 /*if(bgg)printf("set-stop %f %f %f\n",xbig,xjbig,p4max);*/
	 if (dx2>p4max)
	    { if (dirj==0) topstop = 1; else botstop = 1; }
	 if (topstop&&botstop) break;
	 }

/* output the polygon in chain order, zero length edges are kept
   but can be thinned by user proc */

      for (i=0;i<ccount;i++) blink[i] = 0;
      ptr = 0;
      xlink = cc2x[0]; ylink = cc2y[0]; jsave = 0;
      for (i=0;i<ccount;i++)
	 {
	 jdup = 0;
	 for (j=0;j<ccount;j++)
	    {
	    if (blink[j]) continue;
	    if (cc1x[j]!=xlink || cc1y[j]!=ylink) continue;
	    if (j==jsave) continue;
	    jdup++;
	    if (jdup>1&&cc1x[jsave]==cc2x[jsave]&&
			cc1y[jsave]==cc2y[jsave]) break;
	    jsave = j;
	    }
	 if (ccjj[jsave]!=ibig) bjj[ptr++] = ccjj[jsave];
	 xlink = cc2x[jsave]; ylink = cc2y[jsave];
	 blink[jsave] = 1;
	 }
      for (i=0;i<ptr;i++)
	 {
	 if (i==(ptr-1)) ip1 = 0; else ip1 = i+1;
	 if (bjj[i]!=ibig && bjj[ip1]!=ibig)
	    {
	    if (bjj[i]==bjj[ip1]) continue;
	    tstarea = triarea((double)(buf[ibig*2]),(double)(buf[ibig*2+1]),
		       (double)(buf[bjj[i]*2]),(double)(buf[bjj[i]*2+1]),
		       (double)(buf[bjj[ip1]*2]),(double)(buf[bjj[ip1]*2+1]));
            if (tstarea<skinny) continue;
	    /*if (bgg) printf("***************saved\n");*/
	    t1 = bptr[ibig];
	    t2 = bptr[bjj[i]];
	    t3 = bptr[bjj[ip1]];
	    if (t1>t2) { tt=t1; t1=t2; t2=tt; }
	    if (t1>t3) { tt=t1; t1=t3; t3=tt; }
	    if (t2>t3) { tt=t2; t2=t3; t3=tt; }
	    h123 = (t1*7+t2*330+t3*4199)%hlen1-1;
	    for (j=0;j<hlen1;j++)
	       {
	       h123++; if (h123>=hlen1) h123 = 0;
	       if (hash1[0][h123]==0) goto stor1;
	       if (hash1[1][h123]!=t1) continue;
	       if (hash1[2][h123]!=t2) continue;
	       if (hash1[3][h123]!=t3) continue;
	       hash1[0][h123]++; goto done1;
	       }
	    printf("debug:trict %d\n",trict);
	    /*mifcb2 = mi_fopen("hash1dbg","w");
	    lab[0] = '\0';
	    mi_labwr(mifcb2,lab,0,0,"graphics","polytr");
	    mg_put(mifcb2,1,4,hlen1,hash1[0]);
	    mg_put(mifcb2,2,4,hlen1,hash1[1]);
	    mg_put(mifcb2,3,4,hlen1,hash1[2]);
	    mg_put(mifcb2,4,4,hlen1,hash1[3]);
	    fclose(mifcb2);
	    zmabend("hash error 1");*/
	    stor1: hash1[1][h123] = t1;
		   hash1[2][h123] = t2;
		   hash1[3][h123] = t3;
		   hash1[0][h123] = 1;
		   /*if (bgg)
		      {
		      printf("###########saving h123 %d\n",h123);
		      printf("ibig,ptr,i %d %d %d\n",ibig,ptr,i);
		      for (k=0;k<ptr;k++)
			 printf("k,bjj[k],bptr[bjj[k]] %d %d %d\n",
				   k,bjj[k],bptr[bjj[k]]);
		      }*/
		   trict++;
		   continue;
	    done1: continue;
	    }
	 }
      }

   /* condense the triangles, then remove intersecting triangles that
       result from perfect grid squares, prefer higher count duplicated
       triangles; uses a hash table of pairs of points from triangles */

   free(buf); free(bptr);
   mz_alloc2((unsigned char ***)&tc,4,trict,4);
   triptr = 0;

   for (i=3;i>0;i--)
      {
      for (j=0;j<hlen1;j++)
	 {
	 if (hash1[0][j]<i) continue;
	 tc[0][triptr] = hash1[0][j];
	 tc[1][triptr] = hash1[1][j];
	 tc[2][triptr] = hash1[2][j];
	 tc[3][triptr++] = hash1[3][j];
	 hash1[0][j] = 0;
	 }
      }
   mz_free2((unsigned char **)hash1,4);
   if (triptr!=trict) zmabend("hash error 2");
   for (i=0;i<10000;i++)
      {
      hlen2 = (trict*3+1000)+i;
      if (hlen2%2==0) continue;
      for (j=3;j<=37;j+=2)
	 {
	 if (hlen2%j==0) break;
	 if (j==37) goto rnd2;
	 }
      }
   rnd2: /*if(bgg)printf("hash length = %d\n",hlen2);*/
   mz_alloc2((unsigned char ***)&hash2,3,hlen2,4);
   for (i=0;i<hlen2;i++) hash2[0][i] = -1;
   for (i=0;i<trict;i++)
      {
      if (i%10000==9999) printf("%d triangles processed\n",i);
      t0 = tc[0][i];
      for(j=0;j<3;j++)
	 {
	 t1 = tc[j/2+1][i];
	 t2 = tc[(j+8)/3][i];
	 t3 = tc[3-j][i];
	 h12 = (t1*7+t2*330)%hlen2-1;
	 for (k=0;k<hlen2;k++)
	    {
	    h12++; if (h12>=hlen2) h12 = 0;
	    if (hash2[0][h12]==(-1)) goto stor2;
	    if (hash2[1][h12]!=t1) continue;
	    if (hash2[2][h12]!=t2) continue;
	    /* may enter more than once */
	    ttptr = hash2[0][h12];
	    if (tc[0][ttptr]==(-99)) continue;
	    for (l=1;l<4;l++)
	       {
	       tt3 = tc[l][ttptr];
	       if (tt3!=t1&&tt3!=t2) break;
	       }
	    segxseg(ptx[t1-1],pty[t1-1],ptx[t3-1],pty[t3-1],
		    ptx[t2-1],pty[t2-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    segxseg(ptx[t2-1],pty[t2-1],ptx[t3-1],pty[t3-1],
		    ptx[t1-1],pty[t1-1],ptx[tt3-1],pty[tt3-1],tw);
	    if(tw[0]>=0.01&&tw[0]<=0.99&&tw[1]>=0.01&&tw[1]<=0.99)goto done2;
	    }
	 zmabend("hash error 3");
	 stor2: hash2[0][h12] = i;
		hash2[1][h12] = t1;
		hash2[2][h12] = t2;
		continue;
	 done2: tc[0][i] = -99;
		break;
	 }
      }

   mz_free2((unsigned char **)hash2,3);
   mz_alloc1((unsigned char **)tcon1,triptr,4);
   mz_alloc1((unsigned char **)tcon2,triptr,4);
   mz_alloc1((unsigned char **)tcon3,triptr,4);
   trict = 0;
   for (i=0;i<triptr;i++)
      {
      if (tc[0][i]==(-99)) continue;
      (*tcon1)[trict] = tc[1][i];
      (*tcon2)[trict] = tc[2][i];
      (*tcon3)[trict++] = tc[3][i];
      }

   /* now sort the triangles in vertical order, as required by tiegrid */

   mz_free2((unsigned char **)tc,4);
   mz_alloc1((unsigned char **)&buf,trict,8);
   mz_alloc1((unsigned char **)&bptr,trict,4);
   for (i=0;i<trict;i++)
      {
      buf[i] = ptx[(*tcon1)[i]-1]+ptx[(*tcon2)[i]-1]+ptx[(*tcon3)[i]-1];
      bptr[i] = i+1;
      }
   sort8(buf,bptr,trict);
   sortrec4(*tcon1,bptr,trict);
   sortrec4(*tcon2,bptr,trict);
   sortrec4(*tcon3,bptr,trict);

   /* triangles also have to be clockwise */

   for (i=0;i<trict;i++)
      {
      tstarea = triarea((double)(ptx[(*tcon1)[i]-1]),(double)(pty[(*tcon1)[i]-1]),
		 (double)(ptx[(*tcon2)[i]-1]),(double)(pty[(*tcon2)[i]-1]),
		 (double)(ptx[(*tcon3)[i]-1]),(double)(pty[(*tcon3)[i]-1]));
      if (tstarea>0.0) continue;
      temp = (*tcon3)[i]; (*tcon3)[i] = (*tcon1)[i]; (*tcon1)[i] = temp;
      }

   *ntriang = trict;             /* can check this with Euler 2P-2-CVXHULL*/
   *nlinret = npoints+trict-1;   /* using Euler */
   printf("%d points %d lines %d triangles\n",npoints,*nlinret,trict);
   free(buf); free(bptr);
   return;
}

int insidetri( double x, double y, double x1, double y1, double x2, double y2, double x3, double y3 )
{
   if ((x-x1)*(y2-y1)-(y-y1)*(x2-x1)>0.) return(0);
   if ((x-x2)*(y3-y2)-(y-y2)*(x3-x2)>0.) return(0);
   if ((x-x3)*(y1-y3)-(y-y3)*(x1-x3)>0.) return(0);
   return(1);
}

 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoMemUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "zvproto.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoVicarProtos.h"


/*=========================================================

zvparmalloc

Allocate enough memory for named parameter, assumed to be a single
string. Retrieve parameter, storing in allocated buffer. Return
buffer.

arguments:
     1. name: parameter name

Returned pointer should be freed when no longer needed.

*/

char* zvparmalloc( char* name ) {
  int count = 0;
  int def = 0;
  int maxlen = 0;
  char type[7];   /* The possible values are "INT", "REAL", and "STRING" */
  char * buf = NULL;

  zvpstat( name, &count, &def, &maxlen, type );

  buf = (char*) malloc( maxlen + 1 ); /* plus null */

  zvparm( name, buf, &count, &def, 1, 0);

  return buf;
}  

/*=========================================================

mz_alloc1

allocate a one dimensional array of any type

arguments:
     1. buf: output, unsigned char **buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	dimension of the array
     3. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space may be
released with the free(buf) statement.
*/
void mz_alloc1( unsigned char ** buf, int d1, int w )
/*    int d1,w; */
/*    unsigned char **buf; */
{
   if ((*buf=(unsigned char *)malloc(1+d1*w))==NULL)
			    zmabend("malloc failed");
   return;
}

/*=========================================================

mz_alloc2

allocate a two dimensional array of any type

arguments:
     1. buf: output, unsigned char ***buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free2(buf,d1) so all of the parts can be freed
in reverse order.
*/
void mz_alloc2( unsigned char *** buf, int d1, int d2, int w )
/*    int d1,d2,w; */
/*    unsigned char ***buf; */
{
   int i;
   if ((*buf=(unsigned char **)malloc(1+d1*sizeof(void*)))==NULL)
			    zmabend("malloc failed");

   for (i=0;i<d1;i++)
      if (((*buf)[i]=(unsigned char *)malloc(1+d2*w))==NULL)
			    zmabend("malloc failed");
   return;
}

/*=========================================================

mz_free2

free a two dimensional array created by mz_alloc2

arguments:
     1. buf: output, unsigned char **buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array

The subparts are freed first and then the top part.  Use
the first dimension from the mz_alloc2 call.

*/
void mz_free2( unsigned char ** buf, int d1 )
/*    int d1; unsigned char **buf; */
{
   int i;
   for (i=0;i<d1;i++) free(buf[i]);
   free(buf);
   return;
}

/*=========================================================

mz_alloc3

allocate a three dimensional array of any type

arguments:
     1. buf: output, unsigned char ****buf;
	contents set to pointer to array.
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array
     4. d3: input, int d3;
	third dimension of the array
     5. w: input, int w;
	number of bytes per array element

memalign is used to align on doubleword boundary,  1 is added
to guarantee a non-zero request.  Type of data in array is
immaterial, but length is given by w parameter.  The space cannot
be released with a simple call to free(buf) but must be released
with a call to mz_free3(buf,d1,d2) so all of the parts can be freed
in reverse order.
*/
void mz_alloc3( unsigned char **** buf, int d1, int d2, int d3, int w )
/*    int d1,d2,w; */
/*    unsigned char ****buf; */
{
   int i,j;
   if ((*buf=(unsigned char ***)malloc(1+d1*sizeof(void*)))==NULL)
			    zmabend("malloc failed");
   for (i=0;i<d1;i++)
      {
      if (((*buf)[i]=(unsigned char **)malloc(1+d2*sizeof(void*)))==NULL)
			    zmabend("malloc failed");
      for (j=0;j<d2;j++)
	 if (((*buf)[i][j]=(unsigned char *)malloc(1+d3*w))==NULL)
			       zmabend("malloc failed");
      }
   return;
}

/*=========================================================

mz_free3

free a three dimensional array created by mz_alloc3

arguments:
     1. buf: output, unsigned char ***buf;
	array to be freed (not a pointer)
     2. d1: input, int d1;
	first dimension of the array
     3. d2: input, int d2;
	second dimension of the array

The subparts are freed first and then the top part.  Use
the first two dimensions from the mz_alloc3 call.

*/
void mz_free3( unsigned char *** buf, int d1, int d2)
/*    int d1,d2; */
/*    unsigned char ***buf; */
{
   int i,j;
   for (i=0;i<d1;i++)
      {
      for (j=0;j<d2;j++) free(buf[i][j]);
      free(buf[i]);
      }
   free(buf);
   return;
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoSortUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "cartoSortUtils.h"
#include "cartoStrUtils.h"
#include "cartoVicarProtos.h"

/***************************************************/
/* This function is a helper function for          */
/* get*SortIndices to swap the indices.            */
/***************************************************/
void swap(int *array, int i, int j)
{
   int tmp;

   tmp = array[i];
   array[i] = array[j];
   array[j] = tmp;
}

/***************************************************/
/* This function is a helper function for          */
/* get*SortIndices to compare different data       */
/* types.                                          */
/*                                                 */
/* IN: void *array - buffer containing data to     */
/*                   compare                       */
/*     int index1 - index of element to compare    */
/*     int index2 - index of element to compare    */
/*     int type - type of elements in array        */
/*           1 = char                              */
/*           2 = short int                         */
/*           3 = int                               */
/*           4 = float                             */
/*           5 = double                            */
/*           6 = long int                          */
/*           7 = unsigned char                     */
/*           8 = unsigned short int                */
/*           9 = unsigned int                      */
/*           10 = unsigned long int                */
/*                                                 */
/* RETURN: -1 if index1 is greater than index2     */
/*         0  if equal                             */
/*         1  if index2 is greater than index1     */
/***************************************************/
int compare(void *array, int index1, int index2, int type)
{
   switch(type)
   {
      case CART_CHAR:
         if(((char*)array)[index1] == ((char*)array)[index2]) return 0;
	 if(((char*)array)[index1] > ((char*)array)[index2]) return -1;
         if(((char*)array)[index1] < ((char*)array)[index2]) return 1;
      case CART_SHORT:
         if(((short int*)array)[index1] == ((short int*)array)[index2]) return 0;
	 if(((short int*)array)[index1] > ((short int*)array)[index2]) return -1;
         if(((short int*)array)[index1] < ((short int*)array)[index2]) return 1;
      case CART_INT:
         if(((int*)array)[index1] == ((int*)array)[index2]) return 0;
	 if(((int*)array)[index1] > ((int*)array)[index2]) return -1;
         if(((int*)array)[index1] < ((int*)array)[index2]) return 1;
      case CART_FLOAT:
         if(((float*)array)[index1] == ((float*)array)[index2]) return 0;
	 if(((float*)array)[index1] > ((float*)array)[index2]) return -1;
         if(((float*)array)[index1] < ((float*)array)[index2]) return 1;
      case CART_DOUBLE:
         if(((double*)array)[index1] == ((double*)array)[index2]) return 0;
	 if(((double*)array)[index1] > ((double*)array)[index2]) return -1;
         if(((double*)array)[index1] < ((double*)array)[index2]) return 1;
      case CART_LONG:
         if(((long int*)array)[index1] == ((long int*)array)[index2]) return 0;
	 if(((long int*)array)[index1] > ((long int*)array)[index2]) return -1;
         if(((long int*)array)[index1] < ((long int*)array)[index2]) return 1;
      case CART_USHORT:
         if(((unsigned short int*)array)[index1] == ((unsigned short int*)array)[index2]) return 0;
	 if(((unsigned short int*)array)[index1] > ((unsigned short int*)array)[index2]) return -1;
         if(((unsigned short int*)array)[index1] < ((unsigned short int*)array)[index2]) return 1;
      case CART_UINT:
         if(((unsigned int*)array)[index1] == ((unsigned int*)array)[index2]) return 0;
	 if(((unsigned int*)array)[index1] > ((unsigned int*)array)[index2]) return -1;
         if(((unsigned int*)array)[index1] < ((unsigned int*)array)[index2]) return 1;
      case CART_ULONG:
         if(((unsigned long int*)array)[index1] == ((unsigned long int*)array)[index2]) return 0;
	 if(((unsigned long int*)array)[index1] > ((unsigned long int*)array)[index2]) return -1;
         if(((unsigned long int*)array)[index1] < ((unsigned long int*)array)[index2]) return 1;
   }

   return CANNOT_COMPARE;
}

/***************************************************/
/* This function performs selection sort on the    */
/* unsorted array and stores the SORTED ORDER      */
/* INDICES into indices array.  This function      */
/* does not move around the data but only returns  */
/* what the sorted index would be inside indices.  */
/*                                                 */
/* IN: void *unsorted - buffer containing unsorted */
/*     data                                        */
/*     int n - number of entries in unsorted buf   */
/*     int type - type of elements in unsorted     */
/*                buffer                           */
/*           1 = char                              */
/*           2 = short int                         */
/*           3 = int                               */
/*           4 = float                             */
/*           5 = double                            */
/*           6 = long int                          */
/*           7 = unsigned char                     */
/*           8 = unsigned short int                */
/*           9 = unsigned int                      */
/*           10 = unsigned long int                */
/*                                                 */
/* OUT: int *indices - returns sorted index order  */
/***************************************************/
void getSelectionSortIndices(void *unsorted, int *indices, int n, int type)
{
   int i;

   for(i = 0; i < n; i++) indices[i] = i;

   for(i = 0; i < n-1; i++)
   {
      int j, minv;
      minv = i;

      for(j = i+1; j < n; j++)
      {
         int cmpResult;

	 cmpResult = compare(unsorted, indices[minv], indices[j], type);
	 assert(cmpResult != CANNOT_COMPARE);

	 if(cmpResult == -1) minv = j;
      }

      if(i != minv) swap(indices, i, minv);
   }
}

void sort8( double * buf, int * ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sort88( double * buf, int * ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com; sorts a vector (x,y,x,y,x,y...) on x
      then on y */
      
      int l,m,k,j,iptr;
      double ibufx,ibufy;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l25;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibufx = buf[2*k];
      ibufy = buf[2*k+1];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&((buf[2*j+2]>buf[2*j])||(
         (buf[2*j+2]==buf[2*j])&&(buf[2*j+3]>buf[2*j+1]) ))) j++;
      if ((buf[2*j]<ibufx)||(
         (buf[2*j]==ibufx)&&(buf[2*j+1]<=ibufy) )) goto l45;
      buf[2*k] = buf[2*j];
      buf[2*k+1] = buf[2*j+1];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[2*k] = ibufx;
      buf[2*k+1] = ibufy;
      ptr[k] = iptr;
      ibufx = buf[0];
      ibufy = buf[1];
      iptr = ptr[0];
      buf[0] = buf[2*l];
      buf[1] = buf[2*l+1];
      ptr[0] = ptr[l];
      buf[2*l] = ibufx;
      buf[2*l+1] = ibufy;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sortrec4( int * key, int * ptr, int len )
{
   int i,*temp;
   
   if (len<2) return;
   if ((temp=(int *)malloc(4*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void sortrec88( double * key, int * ptr, int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(16*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) 
      {
      temp[i*2] = key[i*2];
      temp[i*2+1] = key[i*2+1];
      }
   for (i=0;i<len;i++)
      {
      key[i*2] = temp[ptr[i]*2-2];
      key[i*2+1] = temp[ptr[i]*2-1];
      }
   free(temp);
   return;
}

void sortrec8( double *key, int* ptr,int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(8*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void sortretn8( double *key, int* ptr, int len )
{
   int i;
   double *temp;
   
   if (len<2) return;
   if ((temp=(double *)malloc(8*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[ptr[i]-1] = temp[i];
   free(temp);
   return;
}

void sort4(int *buf, int *ptr, int n)
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}

void sort7( float *buf, int *ptr, int n )
{
      /* quick and dirty translation of quicksort with middle pivot
      taken from sortin.com */
      
      int l,m,k,j,iptr;
      double ibuf;
   
      if (n<2) return;
      l = n-1;
      m = n/2-1;

 l10: k = m;
      ibuf = buf[k];
      iptr = ptr[k];

 l20: j = 2*k+1;
      if (j>=n) goto l25;
      if (j<(n-1)&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l25;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l20;

 l25: buf[k] = ibuf;
      ptr[k] = iptr;
      m--;
      if (m>=0) goto l10;

 l30: k = 0;
      ibuf = buf[k];
      iptr = ptr[k];

 l40: j = 2*k+1;
      if (j>l) goto l45;
      if (j<l&&buf[j+1]>buf[j]) j++;
      if (buf[j]<=ibuf) goto l45;
      buf[k] = buf[j];
      ptr[k] = ptr[j];
      k = j;
      goto l40;

 l45: buf[k] = ibuf;
      ptr[k] = iptr;
      ibuf = buf[0];
      iptr = ptr[0];
      buf[0] = buf[l];
      ptr[0] = ptr[l];
      buf[l] = ibuf;
      ptr[l] = iptr;
      l--;
      if (l>0) goto l30;

      return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoStrUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ctype.h>

#include "cartoStrUtils.h"
#include "cartoVicarProtos.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

void rztrim( char * buf )
{
   int len;
   char *p;
   
   len = strlen(buf);
   p = &buf[len-1];
   while (*p=='0'&&*(p-1)!='.') { *p = (char)0; p--; }
   return;
}

void nicelen( char * hdr, double val, char * buf)
{
   int i,flen,len[3];
   char fmtstr[20];
   
   flen = MAX(13-(int)(log10(fabs(val)+.9)),3);
   strcpy(fmtstr,hdr);
   strcat(fmtstr,"%1.*f\0");
   for (i=0;i<3;i++)
      {
      sprintf(buf,fmtstr,flen-2*i,val);
      rztrim(buf);
      len[i] = strlen(buf);
      if (i==0&&len[0]<9) return;
      }
   if ((len[0]-len[2])<9)
      {
      sprintf(buf,fmtstr,flen,val);
      rztrim(buf);
      }
   else if ((len[0]-len[1])>=4)
      {
      sprintf(buf,fmtstr,flen-2,val);
      rztrim(buf);
      }
   
   return;
}

void scalefmt( char * outbuf, double scale1, double scale2 )
{
   /* scale2 must be formatted for GeoTIFF (-1 times) */
   
   char buf[30];

   nicelen("(",scale1,buf);
   strcpy(outbuf,buf);
   nicelen(",",scale2,buf);
   strcat(outbuf,buf);
   strcat(outbuf,",0.0)");
   return;
}

void trnsfmt( char * outbuf, double * t )
{
   /* t must be formatted for GeoTIFF */
   
   char buf[30];
   
   nicelen("(",t[0],buf);
   strcpy(outbuf,buf);
   nicelen(",",t[1],buf);
   strcat(outbuf,buf);
   nicelen(",0,",t[2],buf);
   strcat(outbuf,buf);
   nicelen(",",t[3],buf);
   strcat(outbuf,buf);
   nicelen(",",t[4],buf);
   strcat(outbuf,buf);
   nicelen(",0,",t[5],buf);
   strcat(outbuf,buf);
   strcat(outbuf,",0,0,0,0,0,0,0,1)");
   
   return;
}

int grab( char * p, char c, char * buf )
{
   int n;
   
   n = 0;
   while (*p!=c&&*p!=0)
      {
      if (*p==0) return 0;
      buf[n++] = *p;
      p++;
      }
   buf[n++] = (char)0;
   return n;
}

/*================================================================

ms_dnum

ms_dnum converts a string to a double and moves the pointer, also
allows for positive and negative exponent with e or E or D or d, for
example 123.45E-002

function return : double

argument :
      1. num_ptr: input, char **num_ptr;

*/

double ms_dnum ( char ** num_ptr )
{
   double sign = 1., lvalue = 0.0, rvalue = 0.0,
         decpt = 0.0, powr = -9999.0, powsign = 1.0;

   while (**num_ptr==' ') (*num_ptr)++;
   if (**num_ptr == '-')
   {
      sign = -1.;
      (*num_ptr)++;
   }
   for (;;(*num_ptr)++)
      {
      if (**num_ptr=='e' || **num_ptr=='E' ||
          **num_ptr=='d' || **num_ptr=='D') { powr = 0.0; continue;}
      if (**num_ptr=='+') continue;
      if (**num_ptr=='-') { powsign = -1.0; continue; }
      if (**num_ptr=='.') { decpt = .1; continue;}
      if (**num_ptr < '0' || **num_ptr > '9') break;
      if (powr!=(-9999.0)) { powr = 10.*powr+(**num_ptr)-'0'; continue; }
      else if (decpt==0.) { lvalue = 10.*lvalue+(**num_ptr)-'0'; continue; }
	 else { rvalue = rvalue+decpt*((**num_ptr)-'0'); decpt *= .1; }
      }
   if (powr!=(-9999.0)) return (sign*(lvalue+rvalue)*pow(10.0,powr*powsign));
   else return (sign*(lvalue+rvalue));
}

/*================================================================
ms_num

ms_num converts a string to an integer.

function return : integer

argument :
      1. num_ptr: input, char *num_ptr;

*/

int ms_num ( char *num_ptr )
{
   int sign = 1,
       value = 0;

   while (*num_ptr==' ') num_ptr++;
   if (*num_ptr == '-')
   {
      sign = -1;
      num_ptr++;
   }
   for (; *num_ptr >= '0' && *num_ptr <= '9'; num_ptr++)
      value = 10*value+(*num_ptr)-'0';
   return (sign*value);
}


/*===================================================================

ms_find

ms_find searches string str1 for the substring str2 and returns
a pointer to the first location in string after the substring.

function return : character pointer

arguments :

      1. str1: input, char *str1;

      2. str2: input, char *str2;

Null pointer is returned if substring is not found.

*/

char *ms_find( char * str1, char * str2 )
{
   char *str1c;
   int str2_len = strlen(str2);

   str1c = str1;
   for (; strlen(str1c) >= str2_len; str1c++)
      if (strncmp(str1c, str2, str2_len)==0)
	 return (str1c += str2_len);
   return ((char)0);
}

char *nameget(char* s)
{
   int ix;
   
   ix = strlen(s)-1;
   while ((s[ix]!='\\')&&(s[ix]!='/')&&(ix>0)) ix--;
   if ((s[ix]=='\\')||(s[ix]=='/')) ix++;
   
   return &s[ix];
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoVicarProtos.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef CARTOVICARPROTOS_H
#define CARTOVICARPROTOS_H

/* Missing from vicar includes */

/* 30sep2011 - copied from Cart Lab
   14nov2011 -lwk- all templates removed:  the TAE ones are in taeextproto.h,
                   prnt.h is in VICAR $P1INC; zmabend will be delivered to $P2INC;
                   also removed taeconf.inp since it's contained in taeextproto.h
                   (parblk.inc and <stdio.h> needed to define variables in latter)
*/

#include <stdio.h>
#include "taeextproto.h"
#include "parblk.inc"
#include "prnt.h"
#include "zmabend.h"
/* conflicts with hdf.h */
#undef FAIL

#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create carto_subs.imake
/* imake file for routines copied from Cart lab VICAR to support
   new versions of programs obtained from them */

#define SUBROUTINE carto_subs
#define P2_SUBLIB

#define MODULE_LIST cartoGridUtils.c cartoGtUtils.c cartoLsqUtils.c cartoMatUtils.c \
                    cartoMemUtils.c cartoSortUtils.c cartoStrUtils.c

#define INCLUDE_LIST cartoVicarProtos.h

#define USES_ANSI_C

#define LIB_TAE

/*#define LIB_LOCAL       /* for development, remove on delivery */ 
/*#define DEBUG           /* for development, remove on delivery */ 

$ Return
$!#############################################################################
