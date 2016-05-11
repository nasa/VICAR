$!****************************************************************************
$!
$! Build proc for MIPL module carto_subs
$! VPACK Version 2.1, Monday, April 11, 2016, 12:45:50
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
	   cartoMemUtils.c cartoSortUtils.c cartoStrUtils.c cartoTaeUtils.c -
	   astroreference_camera.c ephreference_camera.c eos_coords.c mat33.c -
	   qmalloc.c quaternion.c rodrigues.c time_conversion.c safe_sqrt.c -
	   time_utils.c strsel.c count_lines.c tokenize.c fgetl.c -
	   sprintf_alloc.c cartoTieUtils.c ibisControlMapper.c ibishelper.c -
	   cartoLinkedList.c ImageUtils.c cloud_masks.c cartoLoggerUtils.c -
	-i carto_subs.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create cartoGridUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>

#include "zmabend.h"
#include "cartoGridUtils.h"
#include "cartoMemUtils.h"

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

#include "zmabend.h"
#include "zvproto.h"

#include "cartoGtUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"

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
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoMatUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "zmabend.h"
#include "zvproto.h"

#include "cartoMatUtils.h"
#include "cartoMemUtils.h"
#include "cartoSortUtils.h"

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

#include "zmabend.h"
#include "zvproto.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"


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

#include "zmabend.h"
#include "cartoSortUtils.h"
#include "cartoStrUtils.h"

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
$ create cartoTaeUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h> /* necessary for taeextproto.h */
#include <string.h>

#include "zmabend.h"
#include "zvproto.h"
#include "taeconf.inp"
#include "taeextproto.h"
#include "parblk.inc"

//#include "carto/cartoTaeUtils.h"
//#include "carto/cartoVicarProtos.h"

/*======================================================

mq_out_int

output integer value to parameter block

arguments :

      1. pname: input, char *pname;
         Parameter name.

      2. val: output, int val;
         Value output to parameter block.

mq_out_int returns integer variables to the parameter block.
*/

void mq_out_int (char *pname, int val)
{
   int i_vec[1];
   struct PARBLK parblk;        /* TAE parameter block */

   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   i_vec[0] = val;
   q_intg(&parblk, pname, (FUNINT)1, i_vec, (FUNINT)P_ADD);
   zvq_out(&parblk);
}

/*======================================================

mq_out_real

output real value to parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, double val;
	 Value output to parameter block.

mq_out_real returns integer variables to the parameter block.  The name
and value are output to the print log.
*/

void mq_out_real (char *pname, double val)
{
   struct VARIABLE *p_find();
   double r_vec[1];
   struct PARBLK parblk;	/* TAE parameter block */
   
   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   r_vec[0] = val;
   q_real(&parblk, pname, (FUNINT)1, r_vec, (FUNINT)P_ADD);
   zvq_out(&parblk);
}

/*======================================================

mq_out_string

returns character string to the parameter block

arguments :

      1. pname: input, char *pname;
	 Parameter name.

      2. val: output, char *val;
	 String returned to parameter block.

      3. maxlen: input, int maxlen;
	 max length to put to output parameter block

mq_out_string returns character strings to the
parameter block.
*/

void mq_out_string (char *pname, char *val, int maxlen)
{
   char *t_vec[1];
   int k;
   struct PARBLK parblk;	/* TAE parameter block */
   
   q_init(&parblk,(FUNINT)500,(FUNINT)P_ABORT); /* Initialize a local par block */
   if (maxlen+1 >= 132) zmabend("error in mq_out_string");
   k = strlen(val);
   if (k>maxlen)
      {
      printf("string truncated\n");
      k = maxlen;
      }

   t_vec[0] = (char *)malloc(sizeof(char)*(k+1));
   strncpy(t_vec[0],val,k+1);
   t_vec[0][k] = '\0';
   q_string(&parblk,pname,(FUNINT)1,t_vec,(FUNINT)P_ADD);
   zvq_out(&parblk);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create astroreference_camera.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     astroreference_camera
  Author:    Mike Burl 
  Date:      2007/01/25
  Function:  This function constructs a set of correspondences between image plane (u,v)
               (where u = sample, v = line) coordinates and J2000 right ascension and
               declination (DEC) coordinates.

  History:  2007/05/24 (MCB) - Changed APIs again so we now take:
              (1) an elapsed TT  (in seconds) since the instant 2000-01-01T12:00:00 TT and 
              (2) an elpased UT1 (in seconds) since the instant 2000-01-01T12:00:00 UT1.

            2007/02/26 (MCB) - Changed APIs so that we now take a TDB and UT1 time
              expressed in seconds since J2000.0 epoch rather than a UTC_string and 
              Delta_UT1 as we did previously.

            2007/02/21 (MCB) - Changed function API so that the orientation is now taken
              as a quaternion (4-element unit vector with scalar part first) rather
              than as a rodrigues rotation vector.

            2007/02/20 (MCB) - 
              The new wframe argument indicates which frame (ECEF (0) or TOD (1)) is the
                reference frame for w_t_c and w_o_c. 
              
              If the reference is ECEF (wframe == 0), then the time of the observation
                (UTC_string) and the offset between UTC and UT1 (Delta_UT1) are both
                essential to convert to TOD (and eventually to J2000).

              If the reference is TOD (wframe == 1), then the exact offset between UTC
                and UT1 (Delta_UT1) is unimportant, but the time of the observation
                (UTC_string) is essential for determining the correct conversion from 
                TOD to J2000 (due to time dependence of precession and nutation of 
                TOD axes wrt J2000 frame).

            2007/01/25 (MCB) - Based on x_eos_coords.cpp and georeference_camera.c
 
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "qmalloc.h"
#include "rodrigues.h"
#include "quaternion.h"
#include "eos_coords.h"
#include "astroreference_camera.h"
#include "mat33.h"

/******************************/
/* ASTROREFERENCE_CAMERA_SV_C */
/******************************/

int astroreference_camera_sv_c(double *urange, double *vrange, 
    double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, 
    double fu, double fv, double q, double u0, double v0, double *kappa, 
    double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr)
{
  double    TOD_R_SV[3*3];
  double    SV_R_C[3*3];
  double    TOD_R_C[3*3];
  double    dum[3];
  double    TOD_t_C[3];
  double    TOD_q_C[4];
  int       wframe;
  int       status;
  /*  char      infunc[] = "astroreference_camera_sv_c"; */

  /* Perhaps not the best way, but convert quaternions to rotation matrices */
  quaternion_to_mat(TOD_q_SV, TOD_R_SV);
  quaternion_to_mat(SV_q_C, SV_R_C);
  
  /* Multiply to get composite rotation */
  mat33_mat33_mult(TOD_R_SV, SV_R_C, TOD_R_C);

  /* Convert TOD_R_C back to quaternion */
  mat_to_quaternion(TOD_R_C, TOD_q_C);

  /* Compute TOD_t_C = TOD_R_SV * SV_t_C + TOD_t_SV  */  
  mat33_vec31_mult(TOD_R_SV, SV_t_C, dum);
  vec31_add(dum, TOD_t_SV, TOD_t_C);

  wframe = ASTROREFERENCE_TOD;
  status = astroreference_camera(urange, vrange, wframe, TOD_t_C, TOD_q_C, fu, fv, q, u0, v0, kappa, TT, UT1, gr_adr, gc_adr, G_adr);

  return(status);
}

/*************************/
/* ASTROREFERENCE_CAMERA */
/*************************/
int astroreference_camera(double *urange, double *vrange, int wframe, double *w_t_c, double *w_q_c, double fu, double fv, double q, double u0, double v0, double *kappa, double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr)
{
  double               w_o_c[3];
  double               J2000_R_TOD[3*3], ECEF_R_TOD[3*3];
  double               TOD_R_ECEF[3*3];
  double               J2000_R_C[3*3];
  double               TOD_R_C[3*3], ECEF_R_C[3*3];
  double               RA, DEC;
  int                  nu, nv;
  int                  i, j;
  double               u, v, un, vn;
  double               vec[3], d[3];
  int                  ind;
  double               *G;
  int                  gr, gc;
  char                 infunc[] = "astroreference_camera";

  /* Determine coordinate transformations */
  /*   If wframe is TOD, we do not care about ECEF_R_TOD, however, it is */
  /*   calculated anyway. */
  eos_coords(TT, UT1, J2000_R_TOD, ECEF_R_TOD);
  printf("J2000_R_TOD:\n");
  mat33_print(J2000_R_TOD);
  printf("\n");

  /* Convert quaternion into rodrigues vector for internals */
  quaternion_to_rodrigues(w_q_c, w_o_c);

  /* Create TOD_R_C from w_o_c */
  if (wframe == ASTROREFERENCE_ECEF) {
    rodrigues_vec2mat(w_o_c, ECEF_R_C);
    mat33_inverse(ECEF_R_TOD, TOD_R_ECEF);
    mat33_mat33_mult(TOD_R_ECEF, ECEF_R_C, TOD_R_C);
  }
  else {
    rodrigues_vec2mat(w_o_c, TOD_R_C);
  }
  printf("TOD_R_C:\n");
  mat33_print(TOD_R_C);
  printf("\n");

  /* Express camera coordinate system wrt J2000 */
  mat33_mat33_mult(J2000_R_TOD, TOD_R_C, J2000_R_C);

  printf("J2000_R_C:\n");
  mat33_print(J2000_R_C);

  /* Determine number of steps along each dimension of grid */
  nu = 1 + (int) floor((urange[2]-urange[0]+0.1)/urange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  nv = 1 + (int) floor((vrange[2]-vrange[0]+0.1)/vrange[1]); /* Add 0.1 as hack to avoid epsilon errors */
  gr = nu *nv;
  gc = 4;

  /* Allocate array to hold correspondences */
  G = (double *) qmalloc(gr*gc, sizeof(double), 0, infunc, "G");

  ind = 0;
  for (i = 0; i < nu; i++) {
    u = urange[0] + i*urange[1];
    un = (u-u0)/fu;
    for (j = 0; j < nv; j++) {
      v = vrange[0] + j*vrange[1];
      vn = (v-v0)/fv;

      /* Determine the ray direction */
      vec31_assign(vec, un-q*vn, vn, D_ONE);
      mat33_vec31_mult(J2000_R_C, vec, d);
      vec31_unit(d); /* Make into a unit vector */

      /* Convert d into RA and DEC */
      DEC = RAD2DEG * asin(d[2]);
      RA  = RAD2DEG * atan2(d[1], d[0]);
          
      /* Stuff correspondences into G */
      G[ind] = u; /* sample */
      G[ind+1] = v; /* line */
      G[ind+2] = RA;  /* right ascension in decimal degrees */
      G[ind+3] = DEC; /* declination in decimal degrees */
      ind += gc;
    }
  }
  *gr_adr = gr;
  *gc_adr = gc;
  *G_adr = G;

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ephreference_camera.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     ephreference_camera
  Author:    Al Zobrist 
  Date:      2007/01/25
  Function:  This is a combined function for earth or star coordinates.  See the
             individual programs for more details.
             
             clen        expected number of grid points
             calc_mode   0 = earth first, if off earth, then space
                         1 = earth only
                         2 = space only
             *calc_case  1 = calculated successfully for earth coordinates
                         2 = calculated successfully for space coordinates
                         3 = failed (requested earth coord, off earth)
 
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ephreference_camera.h"
#include "georeference_camera.h"
#include "astroreference_camera.h"

/******************************/
/* EPHREFERENCE_CAMERA_SV_C */
/******************************/

int ephreference_camera_sv_c(double *urange, double *vrange, double *hrange,
    double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, 
    double fu, double fv, double q, double u0, double v0, double *kappa, 
    double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr,
    int clen, int calc_mode,int *calc_case)
{
  *calc_case = 3;
  if (calc_mode!=2)
      {
      georeference_camera_sv_c(urange, vrange, hrange, 
          TOD_t_SV, TOD_q_SV, SV_t_C, SV_q_C, fu,
          fv, q, u0, v0, kappa, TT, UT1, gr_adr, gc_adr, G_adr);
      if (*gr_adr==clen) *calc_case = 1;
      }
   if ((calc_mode==2)||((calc_mode==0)&&(*calc_case==3)))
      {
      astroreference_camera_sv_c(urange,vrange,
          TOD_t_SV,TOD_q_SV,SV_t_C,SV_q_C,fu,fv,q,u0,v0,
          kappa,TT,UT1,gr_adr,gc_adr,G_adr);
      *calc_case = 2;
      }

return 0;
}
 
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create eos_coords.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     eos_coords
  Author:    Mike Burl 
  Date:      2006/12/14
  Function:  This function finds rotation matrix from True-of-Date coordinate system (which
               includes both precession and nutation) to (a) the J2000 coordinate system
               and (b) the ECEF coordinate system.

  History:   2007/05/24 (MCB) - Changed API so we now take:
              (1) an elapsed TT  (in seconds) since the instant 2000-01-01T12:00:00 TT and 
              (2) an elpased UT1 (in seconds) since the instant 2000-01-01T12:00:00 UT1.


             2007/03/09 (MCB) - Left nutation and precession calculations in TDB pending
               clarification from contractor. Fixed comments regarding input.

             2007/03/05 (MCB) - Modified to incorporate higher-order nutation model
               from 3rd revision of "Attitude Control Coordinate Frames".

             2007/02/26 (MCB) - Modified to take barycentric dynamical time and ut1 time,
               both expressed in seconds since the J2000.0 epoch, as arguments rather than
               a UTC_string and Delta_UT1 value. This means that the caller of eos_coords()
               may need to first call utc_to_tdb_and_ut1() or 
               utc_iso_time_string_to_tdb_and_ut1(), which have been added to
               time_conversion.c.

             2006/12/27 (MCB) - Revised to incorporate note 1a below, plus fixed two errors
               found in the 2006/07/03 memo (units on nutation coefficients and units on 
               Theta_GMST).

             2006/12/14 (MCB) - Based on eos_coords.m

  INPUT:
  -----
  TT          - elapsed time (in seconds) since 2000-01-01T12:00:00 TT (the J2000.0 epoch).
  UT1         - elpased Universal time (in seconds) since 2000-01-01T12:00:00 UT1.

  OUTPUT: 
  ------
  J2000_R_TOD - (3 X 3) rotation matrix to transform position vectors from True-of-date (TOD) 
                   coordinate frame to J2000 coordinate frame.
  ECEF_R_TOD  - (3 X 3) rotation matrix to transform position vectors from True-of-date (TOD)
                  coordinate frame to ECEF.


  NOTES:
  -----
  1a. Current version is based on rev 3 of a memo (see also notes 1b and 1c) that is
      titled "Attitude Control Coordinate Frames".
  1b. Based on rev 2 of a memo titled "Coordinate Systems for Earth Observing Spacecraft", 
        2006/07/03, which in turn is based on D. A. Valladio, "Fundamentals of Astrodynamics 
        and Applications", El Segundo, CA, Mircocosm Press, (2001).
  1c. An earlier version of this function was based on rev1 of the memo in Note 1b,
        dated 2005/01/14.

  2.  See also time_conversion.c

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "qmalloc.h"
#include "time_conversion.h"
#include "eos_coords.h"
#include "mat33.h"

#define N_NUTATION_TERMS 106

/***********************/
/* EOS_COORDS          */
/***********************/
int eos_coords(double TT, double UT1, double *J2000_R_TOD, double *ECEF_R_TOD)
{
  double               T_UT1, T_UT1_POWERS[5];
  double               T_TT, T_TT_POWERS[5];
  int                  i, j;

  double               zeta_arcsec, zeta_rad;
  double               Theta_arcsec, Theta_rad;
  double               z_arcsec, z_rad;

  double               F1_deg, F2_deg, F3_deg, F4_deg, F5_deg;
  double               msvec[5];

  double               M1[9], M2[9], M3[9], M4[9], MOD_R_J2000[9];
  double               G1[9], G2[9], G3[9], G4[9], TOD_R_MOD[9], TOD_R_J2000[9];
  double               A_deg[N_NUTATION_TERMS], A_rad[N_NUTATION_TERMS];
  double               alpha;
  double               Delta_psi_uarcsec, Delta_psi_arcsec, Delta_psi_rad;
  double               Delta_epsilon_uarcsec, Delta_epsilon_arcsec;
  double               epsilon_prime_deg, epsilon_prime_rad;
  double               epsilon_deg, epsilon_rad;

  double               alpha_GMST_sec;
  double               EQ_sec, EQ_arcsec;
  double               alpha_GAST_deg;
  double               alpha_GAST_rad;
  double               alpha_GAST_deg_standard;

  /* Coefficients for various calculations */
  static double        C_zeta_arcsec[5]  = { D_ZERO, 2306.2181, 0.30188, 0.017998, D_ZERO }; /* MCB: fixed 2 errors in memo-rev3 */
  static double        C_Theta_arcsec[5] = { D_ZERO, 2004.3109, -0.42665, -0.041833, D_ZERO };
  static double        C_z_arcsec[5]     = {D_ZERO, 2306.2181, 1.09468, 0.018203, D_ZERO }; /* MCB: Fixed 2 errors in memo-rev3 */

  static double        C_F1[5] = { 134.9629814, 477198.8674, 0.008697, 1.7778e-05, D_ZERO };
  static double        C_F2[5] = { 357.5277233, 35999.05034, -0.000160, -3.3e-06, D_ZERO };
  static double        C_F3[5] = {  93.27191028, 483202.0175, -0.0036825, 3.0556e-06, D_ZERO };
  static double        C_F4[5] = { 297.8503631, 445267.1115, -0.001914, 5.2778e-06, D_ZERO };
  static double        C_F5[5] = { 125.0445222, -1934.136261, 0.002071, 2.22e-06, D_ZERO };

  static double        C_epsilon_deg[5] = { 23.43929111, -0.0130042, -1.6389e-07, 5.0361e-07, D_ZERO };
  static double        C_alpha_GMST[5] = { 67310.54841, (876600.0 * 3600.0 + 8640184.812866), 0.093104, -6.2e-06, D_ZERO };

  static double        NUT[N_NUTATION_TERMS][9] = {
                         { 0,  0,  0,  0,  1, -171996, -174.2, 92025,  8.9}, /* 1-10 */
                         { 0,  0,  0,  0,  2,    2062,    0.2,  -895,  0.5},
			 {-2,  0,  2,  0,  1,      46,    0.0,   -24,  0.0},
			 { 2,  0, -2,  0,  0,      11,    0.0,     0,  0.0},
			 {-2,  0,  2,  0,  2,      -3,    0.0,     1,  0.0},
			 { 1, -1,  0, -1,  0,      -3,    0.0,     0,  0.0},
			 { 0, -2,  2, -2,  1,      -2,    0.0,     1,  0.0},
			 { 2,  0, -2,  0,  1,       1,    0.0,     0,  0.0},
                         { 0,  0,  2, -2,  2,  -13187,   -1.6,  5736, -3.1},
                         { 0,  1,  0,  0,  0,    1426,   -3.4,    54, -0.1},

			 { 0,  1,  2, -2,  2,    -517,    1.2,   224, -0.6}, /* 11-20 */
			 { 0, -1,  2, -2,  2,     217,   -0.5,   -95,  0.3},
			 { 0,  0,  2, -2,  1,     129,    0.1,   -70,  0.0},
			 { 2,  0,  0, -2,  0,      48,    0.0,     1,  0.0},
			 { 0,  0,  2, -2,  0,     -22,    0.0,     0,  0.0},
			 { 0,  2,  0,  0,  0,      17,   -0.1,     0,  0.0},
			 { 0,  1,  0,  0,  1,     -15,    0.0,     9,  0.0},
			 { 0,  2,  2, -2,  2,     -16,    0.1,     7,  0.0},
			 { 0, -1,  0,  0,  1,     -12,    0.0,     6,  0.0},
			 {-2,  0,  0,  2,  1,      -6,    0.0,     3,  0.0},

			 { 0, -1,  2, -2,  1,      -5,    0.0,     3,  0.0}, /* 21-30 */
			 { 2,  0,  0, -2,  1,       4,    0.0,    -2,  0.0},
			 { 0,  1,  2, -2,  1,       4,    0.0,    -2,  0.0},
			 { 1,  0,  0, -1,  0,      -4,    0.0,     0,  0.0},
			 { 2,  1,  0, -2,  0,       1,    0.0,     0,  0.0},
			 { 0,  0, -2,  2,  1,       1,    0.0,     0,  0.0},
			 { 0,  1, -2,  2,  0,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  0,  2,       1,    0.0,     0,  0.0},
			 {-1,  0,  0,  1,  1,       1,    0.0,     0,  0.0},
			 { 0,  1,  2, -2,  0,      -1,    0.0,     0,  0.0},

                         { 0,  0,  2,  0,  2,   -2274,   -0.2,   977, -0.5}, /* 31-40 */
                         { 1,  0,  0,  0,  0,     712,    0.1,    -7,  0.0},
			 { 0,  0,  2,  0,  1,    -386,   -0.4,   200,  0.0},
			 { 1,  0,  2,  0,  2,    -301,    0.0,   129, -0.1},
			 { 1,  0,  0, -2,  0,    -158,    0.0,    -1,  0.0},
			 {-1,  0,  2,  0,  2,     123,    0.0,   -53,  0.0},
			 { 0,  0,  0,  2,  0,      63,    0.0,    -2,  0.0},
			 { 1,  0,  0,  0,  1,      63,    0.1,   -33,  0.0},
			 {-1,  0,  0,  0,  1,     -58,   -0.1,    32,  0.0},
			 {-1,  0,  2,  2,  2,     -59,    0.0,    26,  0.0},

			 { 1,  0,  2,  0,  1,     -51,    0.0,    27,  0.0}, /* 41-50 */
			 { 0,  0,  2,  2,  2,     -38,    0.0,    16,  0.0},
			 { 2,  0,  0,  0,  0,      29,    0.0,    -1,  0.0},
			 { 1,  0,  2, -2,  2,      29,    0.0,   -12,  0.0},
			 { 2,  0,  2,  0,  2,     -31,    0.0,    13,  0.0},
			 { 0,  0,  2,  0,  0,      26,    0.0,    -1,  0.0},
			 {-1,  0,  2,  0,  1,      21,    0.0,   -10,  0.0},
			 {-1,  0,  0,  2,  1,      16,    0.0,    -8,  0.0},
			 { 1,  0,  0, -2,  1,     -13,    0.0,     7,  0.0},
			 {-1,  0,  2,  2,  1,     -10,    0.0,     5,  0.0},

			 { 1,  1,  0, -2,  0,      -7,    0.0,     0,  0.0}, /* 51-60 */
			 { 0,  1,  2,  0,  2,       7,    0.0,    -3,  0.0},
			 { 0, -1,  2,  0,  2,      -7,    0.0,     3,  0.0},
			 { 1,  0,  2,  2,  2,      -8,    0.0,     3,  0.0},
			 { 1,  0,  0,  2,  0,       6,    0.0,     0,  0.0},
			 { 2,  0,  2, -2,  2,       6,    0.0,    -3,  0.0},
			 { 0,  0,  0,  2,  1,      -6,    0.0,     3,  0.0},
			 { 0,  0,  2,  2,  1,      -7,    0.0,     3,  0.0},
			 { 1,  0,  2, -2,  1,       6,    0.0,    -3,  0.0},
			 { 0,  0,  0, -2,  1,      -5,    0.0,     3,  0.0},

			 { 1, -1,  0,  0,  0,       5,    0.0,     0,  0.0}, /* 61-70 */
			 { 2,  0,  2,  0,  1,      -5,    0.0,     3,  0.0},
			 { 0,  1,  0, -2,  0,      -4,    0.0,     0,  0.0},
			 { 1,  0, -2,  0,  0,       4,    0.0,     0,  0.0},
			 { 0,  0,  0,  1,  0,      -4,    0.0,     0,  0.0},
			 { 1,  1,  0,  0,  0,      -3,    0.0,     0,  0.0},
			 { 1,  0,  2,  0,  0,       3,    0.0,     0,  0.0},
			 { 1, -1,  2,  0,  2,      -3,    0.0,     1,  0.0},
			 {-1, -1,  2,  2,  2,      -3,    0.0,     1,  0.0},
			 {-2,  0,  0,  0,  1,      -2,    0.0,     1,  0.0},

			 { 3,  0,  2,  0,  2,      -3,    0.0,     1,  0.0}, /* 71-80 */
			 { 0, -1,  2,  2,  2,      -3,    0.0,     1,  0.0},
			 { 1,  1,  2,  0,  2,       2,    0.0,    -1,  0.0},
			 {-1,  0,  2, -2,  1,      -2,    0.0,     1,  0.0},
			 { 2,  0,  0,  0,  1,       2,    0.0,    -1,  0.0},
			 { 1,  0,  0,  0,  2,      -2,    0.0,     1,  0.0},
			 { 3,  0,  0,  0,  0,       2,    0.0,     0,  0.0},
			 { 0,  0,  2,  1,  2,       2,    0.0,    -1,  0.0},
			 {-1,  0,  0,  0,  2,       1,    0.0,    -1,  0.0},
			 { 1,  0,  0, -4,  0,      -1,    0.0,     0,  0.0},

			 {-2,  0,  2,  2,  2,       1,    0.0,    -1,  0.0}, /* 81-90 */
			 {-1,  0,  2,  4,  2,      -2,    0.0,     1,  0.0},
			 { 2,  0,  0, -4,  0,      -1,    0.0,     0,  0.0},
			 { 1,  1,  2, -2,  2,       1,    0.0,    -1,  0.0},
			 { 1,  0,  2,  2,  1,      -1,    0.0,     1,  0.0},
			 {-2,  0,  2,  4,  2,      -1,    0.0,     1,  0.0},
			 {-1,  0,  4,  0,  2,       1,    0.0,     0,  0.0},
			 { 1, -1,  0, -2,  0,       1,    0.0,     0,  0.0},
			 { 2,  0,  2, -2,  1,       1,    0.0,    -1,  0.0},
			 { 2,  0,  2,  2,  2,      -1,    0.0,     0,  0.0},

			 { 1,  0,  0,  2,  1,      -1,    0.0,     0,  0.0}, /* 91-100 */
			 { 0,  0,  4, -2,  2,       1,    0.0,     0,  0.0},
			 { 3,  0,  2, -2,  2,       1,    0.0,     0,  0.0},
			 { 1,  0,  2, -2,  0,      -1,    0.0,     0,  0.0},
			 { 0,  1,  2,  0,  1,       1,    0.0,     0,  0.0},
			 {-1, -1,  0,  2,  1,       1,    0.0,     0,  0.0},
			 { 0,  0, -2,  0,  1,      -1,    0.0,     0,  0.0},
			 { 0,  0,  2, -1,  2,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  2,  0,      -1,    0.0,     0,  0.0},
			 { 1,  0, -2, -2,  0,      -1,    0.0,     0,  0.0},

			 { 0, -1,  2,  0,  1,      -1,    0.0,     0,  0.0},/* 101-106 */
			 { 1,  1,  0, -2,  1,      -1,    0.0,     0,  0.0},
			 { 1,  0, -2,  2,  0,      -1,    0.0,     0,  0.0},
			 { 2,  0,  0,  2,  0,       1,    0.0,     0,  0.0},
			 { 0,  0,  2,  4,  2,      -1,    0.0,     0,  0.0},
			 { 0,  1,  0,  1,  0,       1,    0.0,     0,  0.0}}; /* MCB: Fixed probable error in memo-rev3 */

  /* char                 infunc[] = "eos_coords"; */

  /*=====================*/
  /* Get powers of T_TT  */
  /*=====================*/
  T_TT= TT/ (86400.0 * 36525.0); /* T_TT is TT expressed in Julian centuries */
  T_TT_POWERS[0] = D_ONE;
  for (i = 1; i <= 4; i++) {
    T_TT_POWERS[i] = T_TT * T_TT_POWERS[i-1];
  }

  /*======================*/
  /* Get powers of T_UT1  */
  /*======================*/
  T_UT1 = UT1/(86400.0*36525.0); /* T_UT1 is UT1 expressed in Julian centuries */
  T_UT1_POWERS[0] = D_ONE;
  for (i = 1; i <= 4; i++) {
    T_UT1_POWERS[i] = T_UT1 * T_UT1_POWERS[i-1];
  }
  /*===========================*/
  /* Precession Transformation */
  /*===========================*/
  zeta_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    zeta_arcsec += C_zeta_arcsec[i] * T_TT_POWERS[i];
  }
  zeta_rad = zeta_arcsec * ARCSEC2RAD;

  Theta_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    Theta_arcsec += C_Theta_arcsec[i] * T_TT_POWERS[i];
  }
  Theta_rad = Theta_arcsec * ARCSEC2RAD;

  z_arcsec = D_ZERO;
  for (i = 0; i < 5; i++) {
    z_arcsec += C_z_arcsec[i] * T_TT_POWERS[i];
  }
  z_rad = z_arcsec * ARCSEC2RAD;

  mat33_assign(M1, cos(z_rad), -sin(z_rad), D_ZERO, sin(z_rad), cos(z_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);    
  mat33_assign(M2, cos(Theta_rad), D_ZERO, -sin(Theta_rad), D_ZERO, D_ONE, D_ZERO, sin(Theta_rad), D_ZERO, cos(Theta_rad));
  mat33_assign(M3, cos(zeta_rad), -sin(zeta_rad), D_ZERO, sin(zeta_rad), cos(zeta_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);    
  mat33_mat33_mult(M2, M3, M4);
  mat33_mat33_mult(M1, M4, MOD_R_J2000);


  /*========================*/
  /* Sun and Moon Variables */
  /*========================*/
  /* F1 = l = Mean Anomaly of the Moon */
  F1_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F1_deg += C_F1[i] * T_TT_POWERS[i];
  }

  /* F2 = l' = Mean anomaly of the Sun */
  F2_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F2_deg += C_F2[i] * T_TT_POWERS[i];
  }
	
  /* F3 = F = Mean longitude of the Moon minus mean longitude of moons node */
  F3_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F3_deg += C_F3[i] * T_TT_POWERS[i];
  }

  /* F4 = D = Mean elongation of the Moon from the Sun */
  F4_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F4_deg += C_F4[i] * T_TT_POWERS[i];
  }

  /* F5 = Omega = Longitutde of the mean ascending node of the Moon */
  F5_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    F5_deg += C_F5[i] * T_TT_POWERS[i];
  }
	     
  msvec[0] = F1_deg;
  msvec[1] = F2_deg;
  msvec[2] = F3_deg;
  msvec[3] = F4_deg;
  msvec[4] = F5_deg;

  /*=========================*/
  /* Nutation Transformation */
  /*=========================*/

  /* Calculate the mean obliquity of date */
  epsilon_deg = D_ZERO;
  for (i = 0; i < 5; i++) {
    epsilon_deg += C_epsilon_deg[i] * T_TT_POWERS[i];
  }
  epsilon_rad = epsilon_deg * DEG2RAD;

  /* Calculate Ai terms */
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    A_deg[i] = D_ZERO;
    for (j = 0; j < 5; j++) {
      A_deg[i] += NUT[i][j] * msvec[j];
    }
    A_rad[i] = A_deg[i] * DEG2RAD;
  }

  /* Calculate Delta_psi */
  /*   Coefficients in nutation table are in tenths of milliarcseconds, */
  /*   so need to multiply by 100 to get uarcsec. */
  Delta_psi_uarcsec = D_ZERO;
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    alpha = (NUT[i][5] + NUT[i][6]*T_TT) * sin(A_rad[i])*100;
    Delta_psi_uarcsec += alpha;
  }
  Delta_psi_arcsec = Delta_psi_uarcsec * 1e-06;
  Delta_psi_rad = Delta_psi_arcsec/((double) 3600.0) * DEG2RAD;

  /* Calculate Delta_epsilon */
  /*   Coefficients in nutation table are in tenths of milliarcseconds, */
  /*   so need to multiply by 100 to get uarcsec. */
  Delta_epsilon_uarcsec = D_ZERO;
  for (i = 0; i < N_NUTATION_TERMS; i++) {
    alpha = (NUT[i][7] + NUT[i][8]*T_TT) * cos(A_rad[i])*100;
    Delta_epsilon_uarcsec += alpha;
  }
  Delta_epsilon_arcsec = Delta_epsilon_uarcsec * 1e-06;


  /* Calculate epsilon_prime */
  epsilon_prime_deg = epsilon_deg + Delta_epsilon_arcsec/((double) 3600.0);
  epsilon_prime_rad = epsilon_prime_deg * DEG2RAD;

  /* Compose the basic rotations */
  /* The memo dated 2007/04/02 has a bug in the description of nutation calculation */
  mat33_assign(G1, D_ONE, D_ZERO, D_ZERO, D_ZERO, cos(epsilon_prime_rad), -sin(epsilon_prime_rad), D_ZERO, sin(epsilon_prime_rad), cos(epsilon_prime_rad));
  mat33_assign(G2, cos(Delta_psi_rad), -sin(Delta_psi_rad), D_ZERO, sin(Delta_psi_rad), cos(Delta_psi_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);
  mat33_assign(G3, D_ONE, D_ZERO, D_ZERO, D_ZERO, cos(epsilon_rad), sin(epsilon_rad), D_ZERO, -sin(epsilon_rad), cos(epsilon_rad));
  mat33_mat33_mult(G2, G3, G4);
  mat33_mat33_mult(G1, G4, TOD_R_MOD);

  /* Transforms between TOD and J2000 */
  mat33_mat33_mult(TOD_R_MOD, MOD_R_J2000, TOD_R_J2000);
  mat33_inverse(TOD_R_J2000, J2000_R_TOD);

  /*=====================*/
  /* Spin Transformation */
  /*=====================*/
  /* NOTE: alpha_GMST_sec is in time seconds not arcseconds! */
  alpha_GMST_sec = D_ZERO;
  for (i = 0; i < 5; i++) {
    alpha_GMST_sec += C_alpha_GMST[i] * T_UT1_POWERS[i];
  }

  /* Commented part of equation includes the moon correction terms, which are apparently omitted */
  /*   in partner's calculations. */
  EQ_arcsec = Delta_psi_arcsec * cos(epsilon_prime_rad); /* + 0.00264*sin(F1_deg * DEG2RAD) + 0.000063*sin(2 * F1_deg * DEG2RAD); */
  EQ_sec = EQ_arcsec/((double) 15.0); /* 15 degrees per 3600 seconds is 15 arcsec per second */

  alpha_GAST_deg = (alpha_GMST_sec + EQ_sec) * ((double) 15.0)/((double) 3600.0); /* 15 degrees per 3600 time seconds */
  alpha_GAST_rad = alpha_GAST_deg * DEG2RAD;

  /* for debugging only */
  alpha_GAST_deg_standard = alpha_GAST_deg - 360 * floor(alpha_GAST_deg / ((double) 360.0));
  /* printf("alpha_GAST_deg_standard = %.15f\n",
     alpha_GAST_deg_standard); */

  /* Transformations between ECEF and TOD */
  mat33_assign(ECEF_R_TOD, cos(alpha_GAST_rad), sin(alpha_GAST_rad), D_ZERO, -sin(alpha_GAST_rad), cos(alpha_GAST_rad), D_ZERO, D_ZERO, D_ZERO, D_ONE);
  /* mat33_inverse(ECEF_R_TOD, TOD_R_ECEF); */

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create mat33.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/* Functions to handle various operations on 3 x 3 matrices */
/* HISTORY:*/
/*   2002/07/24 (MCB) Found and fixed bug in mat33_inverse involving */
/*                      incorrect formula for element B[7]=-c5 */
/*   2002/05/22 (MCB) Improved mat33_mat33_mult to allow inplace op. */
/*   2002/05/21 (MCB) Found and fixed a bug in mat33_transpose. */

#include <stdio.h>
#include <math.h>
#include "burl.h"
#include "mat33.h"

#define EPS 1e-12

int mat33_mat33_mult(double *A, double *B, double *C) {

  double c0, c1, c2;
  double c3, c4, c5;
  double c6, c7, c8;

  c0 = A[0]*B[0]+A[1]*B[3]+A[2]*B[6];
  c1 = A[0]*B[1]+A[1]*B[4]+A[2]*B[7];
  c2 = A[0]*B[2]+A[1]*B[5]+A[2]*B[8];

  c3 = A[3]*B[0]+A[4]*B[3]+A[5]*B[6];
  c4 = A[3]*B[1]+A[4]*B[4]+A[5]*B[7];
  c5 = A[3]*B[2]+A[4]*B[5]+A[5]*B[8];

  c6 = A[6]*B[0]+A[7]*B[3]+A[8]*B[6];
  c7 = A[6]*B[1]+A[7]*B[4]+A[8]*B[7];
  c8 = A[6]*B[2]+A[7]*B[5]+A[8]*B[8];

  mat33_assign(C, c0, c1, c2, c3, c4, c5, c6, c7, c8);

  return(OK);
}

int mat33_print(double *A) {
  printf("%.15f %.15f %.15f\n", A[0], A[1], A[2]);
  printf("%.15f %.15f %.15f\n", A[3], A[4], A[5]);
  printf("%.15f %.15f %.15f\n", A[6], A[7], A[8]);

  return(OK);
}

int vec31_print(double *A) {
  printf("%.15f\n", A[0]);
  printf("%.15f\n", A[1]);
  printf("%.15f\n", A[2]);

  return(OK);
}

double mat33_det(double *A) {
  double det;

  det = A[0]*(A[4]*A[8]-A[5]*A[7]) - A[1]*(A[3]*A[8]-A[5]*A[6]) + A[2]*(A[3]*A[7]-A[4]*A[6]);

  return(det);
}

int mat33_copy(double *A, double *B) {

  A[0] = B[0];
  A[1] = B[1];
  A[2] = B[2];
  A[3] = B[3];
  A[4] = B[4];
  A[5] = B[5];
  A[6] = B[6];
  A[7] = B[7];
  A[8] = B[8];

  return(OK);
}

int vec31_copy(double *A, double *B) {

  A[0] = B[0];
  A[1] = B[1];
  A[2] = B[2];

  return(OK);
}

int mat33_inverse(double *A, double *B) {
  double c0, c1, c2;
  double c3, c4, c5;
  double c6, c7, c8;
  double det, idet;

  c0 = (A[4]*A[8]-A[5]*A[7]);
  c1 = (A[3]*A[8]-A[5]*A[6]);
  c2 = (A[3]*A[7]-A[4]*A[6]);
  c3 = (A[1]*A[8]-A[2]*A[7]);
  c4 = (A[0]*A[8]-A[2]*A[6]);
  c5 = (A[0]*A[7]-A[1]*A[6]);
  c6 = (A[1]*A[5]-A[2]*A[4]);
  c7 = (A[0]*A[5]-A[2]*A[3]);
  c8 = (A[0]*A[4]-A[1]*A[3]);
  det = A[0]*c0 - A[1]*c1 + A[2]*c2;
  idet = D_ONE/det;
  mat33_assign(B, c0*idet, -c3*idet, c6*idet, -c1*idet, c4*idet, -c7*idet, c2*idet, -c5*idet, c8*idet);

  return(OK);
}

int mat33_assign(double *A, double a0, double a1, double a2, double a3, double a4, double a5, double a6, double a7, double a8) {
  A[0] = a0;
  A[1] = a1;
  A[2] = a2;
  A[3] = a3;
  A[4] = a4;
  A[5] = a5;
  A[6] = a6;
  A[7] = a7;
  A[8] = a8;

  return(OK);
}

int mat33_transpose(double *A, double *B) {
  /* Do it with all these temp variable to allow transpose in place */
  double a0, a1, a2;
  double a3, a4, a5;
  double a6, a7, a8;

  a0 = A[0];
  a1 = A[1];
  a2 = A[2];
  a3 = A[3];
  a4 = A[4];
  a5 = A[5];
  a6 = A[6];
  a7 = A[7];
  a8 = A[8];

  mat33_assign(B, a0, a3, a6, a1, a4, a7, a2, a5, a8);

  return(OK);
}

int vec31_assign(double *V, double v0, double v1, double v2) {
  V[0] = v0;
  V[1] = v1;
  V[2] = v2;

  return(OK);
}

int mat33_vec31_mult(double *A, double *V, double *B) {
  double c0, c1, c2;
  c0 = A[0]*V[0] + A[1]*V[1] + A[2]*V[2];
  c1 = A[3]*V[0] + A[4]*V[1] + A[5]*V[2];
  c2 = A[6]*V[0] + A[7]*V[1] + A[8]*V[2];
  vec31_assign(B, c0, c1, c2);

  return(OK);
}

int vec31_add(double *A, double *B, double *C) {
  C[0] = A[0]+B[0];
  C[1] = A[1]+B[1];
  C[2] = A[2]+B[2];

  return(OK);
}

int vec31_subtract(double *A, double *B, double *C) {
  C[0] = A[0]-B[0];
  C[1] = A[1]-B[1];
  C[2] = A[2]-B[2];

  return(OK);
}

int mat33_add(double *A, double *B, double *C) {

  int  i;

  for (i = 0; i < 8; i++) {
    C[i] = A[i] + B[i];
  }

  return(OK);
}

int mat33_scale(double *A, double s, double *B) {
  int i;
  for (i = 0; i < 8; i++) {
    B[i] = s*A[i];
  }
  return(OK);
}

int vec31_scale(double *A, double s, double *B) {

  B[0] = s*A[0];
  B[1] = s*A[1];
  B[2] = s*A[2];

  return(OK);
}

/*MCB: 20051110 - changed name from mat33_xax to mat33_xtax */
double mat33_xtax(double *X, double *A) {
  int    i, j;
  double d;

  d = D_ZERO;
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      d += X[i] * A[i*3+j] * X[j];
    }
  }
  return(d);
}

double mat33_xtay(double *X, double *A, double *Y) {
  int    i, j;
  double d;

  d = D_ZERO;
  for (i = 0; i < 3; i++) {
    for (j = 0; j < 3; j++) {
      d += X[i] * A[i*3+j] * Y[j];
    }
  }
  return(d);
}

double vec31_vec31_dot(double *A, double *B) {
  return( A[0]*B[0] + A[1]*B[1] + A[2]*B[2]);
}

int vec31_axpy(double a, double *X, double *Y, double *Z) {
  int    i;

  for (i = 0; i < 3; i++) {
    Z[i] = a*X[i] + Y[i];
  }

  return(OK);
}

int vec31_vec31_cross(double *A, double *B, double *C) {
  C[0] = A[1]*B[2] - A[2]*B[1];
  C[1] = A[2]*B[0] - A[0]*B[2];
  C[2] = A[0]*B[1] - A[1]*B[0];

  return(OK);
}

double vec31_norm(double *A) {
  return( sqrt(A[0]*A[0] + A[1]*A[1] + A[2]*A[2]) );
}

int vec31_unit(double *A) {
  double s;
  char   infunc[] = "vec31_unit";

  s = vec31_norm(A);

  if (s < EPS) {
    fprintf(stderr, "ERROR (%s): cannot normalize 0 vector\n", infunc);
    return(ERR);
  }
  vec31_scale(A, D_ONE/s, A);

  return(OK);
}

int vec31_vec31_basis(double *A, double *B, double *C) {
  /* Compute an orthonormal basis from two vectors A and B */
  /* Each *ROW* of C will be a unit vector for the basis */
  double Ahat[3], Bhat[3];
  double D[3], E[3];
  char   infunc[] = "vec31_vec31_basis";

  vec31_assign(Ahat, A[0], A[1], A[2]);
  if (vec31_unit(Ahat) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use A for basis\n", infunc);
    return(ERR);
  }
  vec31_assign(Bhat, B[0], B[1], B[2]);
  if (vec31_unit(Bhat) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use B for basis\n", infunc);
    return(ERR);
  }
  
  vec31_vec31_cross(Ahat, Bhat, D);
  if (vec31_unit(D) == ERR) {
    fprintf(stderr, "ERROR (%s): cannot use D for basis\n", infunc);
    return(ERR);
  }

  vec31_vec31_cross(D, Ahat, E);

  /* Place vectors into appropriate rows of C */
  vec31_assign(C,   Ahat[0], Ahat[1], Ahat[2]);
  vec31_assign(C+3, E[0], E[1], E[2]);
  vec31_assign(C+6, D[0], D[1], D[2]);

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create qmalloc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     qmalloc
  Author:    Mike Burl
  Date:      Nov 17, 1998
  Function:  Routine to malloc space and report errors if out of space. Can be
               used to allocate space and reset (i.e., like calloc) if the
               reset flag is set to TRUE.

  History:   2005/04/28 (MCB) - Added a bit more info to the error message
             2005/02/09 (MCB) - Added include of stdio.h to resolve stderr.

*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include "qmalloc.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/


/**************************************/
/* qmalloc                            */
/**************************************/

void *qmalloc(size_t nelem, size_t elsize, int reset, char *infunc, char *vname)

{
  void  *ptr;

/*-------------------------------------------------------------------------------*/
  if (reset == QMALLOC_TRUE) {
    ptr = calloc(nelem, elsize);
  }
  else {
    ptr = malloc(nelem * elsize);
  }

  if (ptr == NULL) {
    fprintf(stderr, "ERROR (%s): couldn't malloc space for %s, requested (%ld X %ld)\n", infunc, vname, (unsigned long) nelem, (unsigned long) elsize);
    return(NULL);
  }
  else {
    return(ptr);
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create quaternion.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:    quaternion
  Author:   Mike Burl 
  Date:     20051109
  Function: Functions to convert a quaternion into a rodrigues vector or an
              orthonormal matrix.

  History:  20070828 (MCB) - FIxed the "* f" bug in mat_to_quaternion().

            20070309 (MCB) - Introduced safe_sqrt instead of sqrt to avoid getting
              NaN's in rot_to_quaternion().

            20061024 (MCB) - Found a bug in mat_to_quaternion; wasn't dividing 
              by 2 in initial calculations.
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "quaternion.h"
#include "mat33.h"
#include "safe_sqrt.h"

#define QUATERNION_EPS 1e-12

/****************************/
/* RODRIGUES_TO_QUATERNION  */
/****************************/
/* Convert a rodrigues vector into a unit quaternion */

int rodrigues_to_quaternion(double *v, double *q)

{
  double         theta, c, s;
  /*  char       infunc[] = "rodrigues_to_quaternion"; */

  theta = safe_sqrt(v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
  c = cos(theta/D_TWO);
  s = sin(theta/D_TWO);
  if (fabs(theta) > QUATERNION_EPS) {
    q[0] = c;
    q[1] = s * v[0]/theta;
    q[2] = s * v[1]/theta;
    q[3] = s * v[2]/theta;
  }
  else {
    q[0] = D_ONE;
    q[1] = D_ZERO;
    q[2] = D_ZERO;
    q[3] = D_ZERO;
  }
  return(OK);
}

/****************************/
/* QUATERNION_TO_RODRIGUES  */
/****************************/
/* Convert a unit quaternion into a rodrigues vector */

int quaternion_to_rodrigues(double *q, double *v)

{
  double     theta, f;
  /*  char       infunc[] = "quaternion_to_rodrigues"; */

  theta = D_TWO * acos(q[0]);
  f = safe_sqrt(D_ONE - q[0]*q[0]);

  if (fabs(theta) < QUATERNION_EPS) {
    /* The rotation is zero */
    v[0] = D_ZERO;
    v[1] = D_ZERO;
    v[2] = D_ZERO;
  }
  else {
    v[0] = theta * q[1]/f;
    v[1] = theta * q[2]/f;
    v[2] = theta * q[3]/f;
  }

  return(OK);
}

/**********************/
/* QUATERNION_TO_MAT  */
/**********************/
/* Convert a quaternion to an orthonormal rotation matrix */

int quaternion_to_mat(double *q, double *R)

{
  double     q00, q01, q02, q03;
  double     q11, q12, q13;
  double     q22, q23;
  double     q33;
  /*  char       infunc[] = "quaternion_to_mat"; */

  q00 = q[0] * q[0];
  q01 = q[0] * q[1];
  q02 = q[0] * q[2];
  q03 = q[0] * q[3];
  q11 = q[1] * q[1];
  q12 = q[1] * q[2];
  q13 = q[1] * q[3];
  q22 = q[2] * q[2];
  q23 = q[2] * q[3];
  q33 = q[3] * q[3];

  R[0] = q00 + q11 - q22 - q33;
  R[1] = D_TWO * (-q03 + q12);
  R[2] = D_TWO * (q02 + q13);
  R[3] = D_TWO * (q03 + q12);
  R[4] = q00-q11+q22-q33;
  R[5] = D_TWO * (-q01 + q23);
  R[6] = D_TWO * (-q02 + q13);
  R[7] = D_TWO * (q01 + q23);
  R[8] = q00 - q11 - q22 + q33;

  return(OK);
}

/**********************/
/* MAT_TO_QUATERNION  */
/**********************/
/* Convert a rotation matrix into a quaternion */

int mat_to_quaternion(double *R, double *q)
{
  double     q0, q1, q2, q3, f;
  char       infunc[] = "mat_to_quaternion";

  /* This is only correct up to a sign ambiguity */
  q0 = safe_sqrt(1 + R[0] + R[4] + R[8])/D_TWO ;
  q1 = safe_sqrt(1 + R[0] - R[4] - R[8])/D_TWO ;
  q2 = safe_sqrt(1 - R[0] + R[4] - R[8])/D_TWO ;
  q3 = safe_sqrt(1 - R[0] - R[4] + R[8])/D_TWO ;

  /* Determine which of this is the maximum */
  if ((q0 >= q1) && (q0 >= q2) && (q0 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q0);
    q[0] = q0;
    q[1] = (R[7]-R[5]) * f;
    q[2] = (R[2]-R[6]) * f;
    q[3] = (R[3]-R[1]) * f;
  }
  else if ((q1 >= q0) && (q1 >= q2) && (q1 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q1);
    q[0] = (R[7]-R[5]) * f;
    q[1] = q1;
    q[2] = (R[1]+R[3]) * f;
    q[3] = (R[2]+R[6]) * f;
  }
  else if ((q2 >= q0) && (q2 >= q1) && (q2 >= q3)) {
    f = D_ONE/( ((double) 4.0) * q2);
    q[0] = (R[2]-R[6]) * f;
    q[1] = (R[1]+R[3]) * f;
    q[2] = q2;
    q[3] = (R[5]+R[7]) * f;
  }
  else if ((q3 >= q0) && (q3 >= q1) && (q3 >= q2)) {
    f = D_ONE/( ((double) 4.0) * q3);
    q[0] = (R[3]-R[1]) * f;
    q[1] = (R[2]+R[6]) * f;
    q[2] = (R[5]+R[7]) * f;
    q[3] = q3;
  }
  else {
    fprintf(stderr, "ERROR (%s): bug in code\n", infunc);
    printf("q0 = %.15f, q1 = %.15f, q2 = %.15f, q3 = %.15f\n", q0, q1, q2, q3);
    mat33_print(R);
    return(ERR);
  }
  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create rodrigues.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:    rodrigues
  Author:   Mike Burl 
  Date:     20030611
  Function: Functions to convert a rodrigues-style rotation vector into an orthonormal 
              rotation matrix via rodrigues' formula and vice-versa. Note that
              the rodrigues vector is equivalent to the unit-vector axis of
              rotation multiplied by the angle of rotation about that axis (in
              radians).

  History: 20070125 (MCB) - Added check that the incoming matrix in 
              rodrigues_mat2vec has det = 1. Also, fixed a longstanding bug
              with respect to Bouguet's method of disambiguating signs in 
              the theta = pi case.

*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "rodrigues.h"

#define RODRIGUES_EPS 1e-12

/**********************/
/* RODRIGUES_VEC2MAT  */
/**********************/
/* Convert a rotation vector into an orthonormal rotation matrix
   via rodrigues' formula */

int rodrigues_vec2mat(double *wrot, double *R)

{
  double     theta;
  double     alpha, beta, gamma;
  double     w0, w1, w2;
  /*  char       infunc[] = "rodrigues_vec2mat"; */

  theta = sqrt(wrot[0]*wrot[0] + wrot[1]*wrot[1] + wrot[2]*wrot[2]);
  if (theta < RODRIGUES_EPS) {
    /* R = identity matrix */
    R[0] = D_ONE;  R[1] = D_ZERO; R[2] = D_ZERO;
    R[3] = D_ZERO; R[4] = D_ONE;  R[5] = D_ZERO;
    R[6] = D_ZERO; R[7] = D_ZERO; R[8] = D_ONE;
  }
  else {
    alpha = cos(theta);
    beta  = sin(theta);
    gamma = 1-alpha;
    w0   = wrot[0]/theta;
    w1   = wrot[1]/theta;
    w2   = wrot[2]/theta;
    R[0] = alpha            + gamma*w0*w0; 
    R[1] =         -beta*w2 + gamma*w0*w1;
    R[2] =          beta*w1 + gamma*w0*w2;
    R[3] =          beta*w2 + gamma*w1*w0;
    R[4] = alpha            + gamma*w1*w1;  
    R[5] =         -beta*w0 + gamma*w1*w2;
    R[6] =         -beta*w1 + gamma*w2*w0;
    R[7] =          beta*w0 + gamma*w2*w1;
    R[8] = alpha            + gamma*w2*w2;
  }

  return(OK);
}
/**********************/
/* RODRIGUES_MAT2VEC  */
/**********************/
/* Convert an orthonormal rotation matrix into a rodrigues-style rotation vector
   via rodrigues' formula */

int rodrigues_mat2vec(double *R, double *wrot)

{
  double     c, s, theta;
  double     uabs, vabs, wabs;
  double     u, v, w;
  double     fac;
  double     mvec[3];
  int        i, hash, idx;
  int        n_hashvec = 11;
  int        hashvec[11] = { 0, -1, -3, -9, 9, 3, 1, 13, 5, -7, -11 };
  int        svec[11][3] = {
              { 1,  1,  1 },
              { 1,  0, -1 },
              { 0,  1, -1 },
              { 1, -1,  0 },
              { 1,  1,  0 },
              { 0,  1,  1 },
              { 1,  0,  1 },
              { 1,  1,  1 },
              { 1,  1, -1 },
              { 1, -1, -1 },
              { 1, -1,  1 }};
  char       infunc[] = "rodrigues_mat2vec";

  c = (R[0] + R[4] + R[8] - D_ONE)/D_TWO;
  theta = acos(c);
  s = sin(theta);

  if (fabs(theta) < RODRIGUES_EPS) {
    /* No rotation */
    wrot[0] = D_ZERO;
    wrot[1] = D_ZERO;
    wrot[2] = D_ZERO;
  }
  else if (fabs(theta-M_PI) < RODRIGUES_EPS) {
    /* Rotation by PI */
    uabs = sqrt((R[0]+D_ONE)/D_TWO);
    vabs = sqrt((R[4]+D_ONE)/D_TWO);
    wabs = sqrt((R[8]+D_ONE)/D_TWO);

    mvec[0] = (R[1] + D_ONE)/D_TWO;
    mvec[1] = (R[5] + D_ONE)/D_TWO;
    mvec[2] = (R[2] + D_ONE)/D_TWO;

    hash = 0;
    if (mvec[0] > RODRIGUES_EPS) {
      hash += 9;
    }
    else if (mvec[0] < -RODRIGUES_EPS) {    
      hash -= 9;
    }
    if (mvec[1] > RODRIGUES_EPS) {
      hash += 3;
    }
    else if (mvec[1] < -RODRIGUES_EPS) {    
      hash -= 3;
    }
    if (mvec[2] > RODRIGUES_EPS) {
      hash += 1;
    }
    else if (mvec[2] < -RODRIGUES_EPS) {    
      hash -= 1;
    }
    idx = -1;
    for (i = 0; i < n_hashvec; i++) {
      if (hash == hashvec[i]) {
        idx = i;
        break;
      }
    }
    if (idx == -1) {
      fprintf(stderr, "ERROR (%s): invalid hash = %d\n", infunc, hash);
      return(ERR);
    }
    wrot[0] = theta * uabs * svec[idx][0];
    wrot[1] = theta * vabs * svec[idx][1];
    wrot[2] = theta * wabs * svec[idx][2];
  }
  else {
    /* the normal case */
    fac = theta/(D_TWO*s);
    u   = R[7] - R[5];
    v   = R[2] - R[6];
    w   = R[3] - R[1];
    wrot[0] = fac * u;
    wrot[1] = fac * v;
    wrot[2] = fac * w;
  }

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create time_conversion.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     time_conversion
  Author:    Mike Burl 
  Date:      2006/12/27
  Function:  This file contains a number of functions for converting between 
               various time systems such as UTC, UT1, TT, and TDB. It is important
               to distinguish between "absolute times" and elpased time since
               some event.

  History:   2008/07/09 (MCB) - Added functionality to use the JD field in the
               USNO leap second file to offset a leap second so that it does
               not occur exactly at midnight (UTC).

             2008/06/08 (MCB) - Added utc_iso_time_string_to_dut1tbl_time_string()
               function written by Kyle/Walt.

             2008/06/02 (MCB) - Added a function to populate the leap second table
               by reading in a file in the U.S. Naval Observatory format. See:

               http://maia.usno.navy.mil/ser7/tai-utc.dat

               The table is read into a set of global variables. Also, modified 
               interface to functions that return leap second count so that the leap
               second count is returned as a double.

             2007/09/17 (MCB) - Realized ACS_time is *apparent elapsed UTC seconds* 
                NOT the true elapsed SI seconds since the 2000-01-01T12:00:00UTC epoch.
                Changed old routines to be si_to_tt_and_ut1() and si_to_utc_iso_time_string()
                and introduced new routines to deal with new understanding of ACS_time.
             2007/05/25 (MCB) - Went through functions again. Added the following
               routines: utc_iso_time_string_to_tt_and ut1()
                         acs_to_tt_and ut1().
               Note that the resulting tt is elapsed time in SI seconds since
               2000-01-01T12:00:00 TT (i.e., the J2000.0 epoch), while the
               resulting ut1 time is elapsed universal time since 
               2000-01-01T12:00:00 UT1.
             2007/03/09 (MCB) - Carefully went through and checked out and fixed
               several functions.
             2007/03/06 (MCB) - moved calculation of leap second count into
               separate functions. Fixed very minor bug in leap_second_table,
               which would have only affected things if you put in a UTC time
               string such as 1972-06-30T23:59:60, i.e., right at a leap second
               instant. Moved some repeated code dealing with standardizing
               time components into the proper ranges over to time_utils.c.
             2007/02/26 (MCB) - reorganized the functions to be more logical;
               added two function to convert from UTC or UTC_string to 
               TDB and UT1.             

             Based on various .m files of same names.

  NOTES:
  -----
  1.  On/around 2007/09/18, made changes to reflect fact that ACS_time is apparent
        elapsed UTC seconds since 2000-01-01T12:00:00UTC. 
  2a. Updated implementation on/around 2007/05/25 to incorporate concept of ACS_time
        and fact that elpased UT1 time should be measured from 2000-01-01T12:00:00 UT1
        rather than from J2000.0.
  2b. Updated implementation on/around 2007/03/09 and 2007/03/06 to take better 
        understanding into account.
  2c. Updated implementation based on a revision to above memo dated 2006/07/03.
  2d. Based in part on a memo titled "Coordinate Systems for Earth Observing Spacecraft", 
       2005/01/4, which in turn is based on D. A. Valladio, "Fundamentals of Astrodynamics 
       and Applications", El Segundo, CA, Mircocosm Press, (2001).

  3. See http://en.wikipedia.org/wiki/Coordinated_Universal_Time for more information on UTC.

  4. Delta_AT = 33 as of 2006/01/01
  5. On 2000-01-01, Delta_UT1 = +0.3554752 sec.
     On 2005-12-29, Delta_UT1 = -0.6610984 sec.
     On 2006-06-29, Delta_UT1 = +0.1958360 sec.

  6. The behavior for some of these routines is undefined if a time is given that
       falls *WITHIN* or *NEAR THE BOUNDARY OF* a leap second, e.g., if you call 
       utc_iso_string_to_utc with UTC_string = 2005-12-31T23:59:60.5, the result 
       should not be trusted as it is probably wrong.

*******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <strings.h>
#include "burl.h"
#include "strsel.h"
#include "time_utils.h"
#include "count_lines.h"
#include "tokenize.h"
#include "qmalloc.h"
#include "fgetl.h"
#include "time_conversion.h"
#include "sprintf_alloc.h"

#define MAX_N_LEAPS 1024

/***********************/
/* GLOBAL DECLARATIONS */
/***********************/
int    N_LEAPS = 0;
int    LEAP_TABLE_Y[MAX_N_LEAPS];   /* year */
int    LEAP_TABLE_T[MAX_N_LEAPS];   /* month */
int    LEAP_TABLE_D[MAX_N_LEAPS];   /* day */
int    LEAP_TABLE_H[MAX_N_LEAPS];   /* hour */
int    LEAP_TABLE_M[MAX_N_LEAPS];   /* minute */
double LEAP_TABLE_S[MAX_N_LEAPS];   /* second */
double LEAP_TABLE_ADJ[MAX_N_LEAPS]; /* adjustment */

/************************************/
/* General Notes on Time References */
/************************************/
/* UT0 - Universal Time based upon observations of stars (tied to Earth's angular rotation rate) */
/* UT1 - UT0 with corrections for polar motion */
/* UTC - Coordinated Universal Time derived from Atomic Time with updates to */
/*         maintain it within +/- 0.9 sec from UT1 */
/* TAI - International atomic time standard */
/* TT  - Terrestrial time */
/* TDB - Barycentric Dynamical Time (referenced to solar system barycenter) */ 
/*                                                                          */
/* Some useful relationships regarding absolute time: */
/*   TAI = UTC + Delta_AT;  where Delta_AT is number of leap seconds */
/*   TT  = TAI + 32.184;    */
/*   UT1 = UTC + Delta_UT1; */
/*                                                                             */
/* Delta_UT1 is available from http://tf.nist.gov/pubs/bulletin/leapsecond.htm */
/*   On Jan 01, 2000, Delta_UT1 = +0.3554732.     */
/*   On Dec 29, 2005, Delta_UT1 = -0.6611092 sec. */
/*   On Jun 29, 2006, Delta_UT1 = +0.1961948 sec. */
/*                                                                               */
/* Delta_AT is available from http://hpiers.obspm.fr/iers/bul/bulc/bulletinc.dat */
/* NOTE: that the value published on the hpiers.obspm.fr web page is the negative of Delta_AT */
/*   As of Jan 1, 2006, Delta_AT = +33 sec. This is the total number of leapseconds */
/*   added since 1972 (plus original 10 second offset). */
/*                                                                                  */
/* When time is given in seconds, it is important to know the start reference.      */
/*   For elpased TT time, the reference is 2000-01-01T12:00:00 TT (the J2000 epoch).*/
/*   For elapsed UT1 time, the reference is 2000-01-01T12:00:00 UT1.                */
/*   For ACS time, the reference is 2000-01-01T12:00:00 UTC.                        */

/**************************************/
/* UTC_ISO_TIME_STRING_TO_SI          */
/**************************************/
/* Given an absolute UTC ISO time string, determine the number of elapsed SI        */
/*   seconds since 2000-01-01T12:00:00 UTC. Note that this will be different than   */
/*   the number of *APPARENT* UTC seconds (obtained naively by converting end time  */
/*   and start time to Julian dates and subtracting) due to leap  seconds.          */

int utc_iso_time_string_to_si(char *UTC_string, double *SI_adr)
{
  int                  y1, t1, d1, h1, m1;
  double               s1;
  double               jd0 = 0.0;
  double               jd1;
  char                 *TT_string;
  char                 infunc[] = "utc_iso_time_string_to_si";

  utc_iso_time_string_to_tt_iso_time_string(UTC_string, &TT_string);
  parse_iso_time_string(TT_string, &y1, &t1, &d1, &h1, &m1, &s1);
  if (TT_string != NULL) {
    free((void *) TT_string);
  }

  if (y1 < 2000) {
    fprintf(stderr, "ERROR (%s): only valid for years 2000 and up\n", infunc);
    return(ERR);
  }
  /* Convert current TT time to a Julian date */
  julian_date(y1, t1, d1, h1, m1, s1, &jd1);

  /* Convert absolute TT time at 2000-01-01T12:00:00 UTC to a Julian date */
  julian_date(2000, 1, 1, 12, 1, ((double) 4.184), &jd1); 

  *SI_adr = (jd1 - jd0) * 86400; /* Elpased SI seconds since 2000-01-01T12:00:00 UTC */
  return(OK);
}

/**************************************/
/* UTC_ISO_TIME_STRING_TO_TT_AND_UT1 */
/**************************************/
/* Given an absolute UTC ISO time string, determine the elpased TT time and elapsed   */
/*   UT1 time. The elpased TT time is measured from 2000-01-01T12:00:00 TT, while the */
/*   elapsed UT1 time is measured from 2000-01-01T12:00:00 UT1.                       */

int utc_iso_time_string_to_tt_and_ut1(char *UTC_string, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  char                 *TT_string;
  double               TT;
  /*  char                 infunc[] = "utc_iso_time_string_to_tdb_and_ut1";*/

  /* UTC_string -> TT_string -> TT -> TDB */
  utc_iso_time_string_to_tt_iso_time_string(UTC_string, &TT_string);
  tt_iso_time_string_to_tt(TT_string, &TT);
  /*  printf("UTC_string = %s, TT_string = %s, TT = %.15f\n", UTC_string, TT_string, TT); */
  if (TT_string != NULL) {
    free((void *) TT_string);
  }

  /* TT -> UT1 */
  tt_to_ut1(TT, Delta_UT1, UT1_adr);
  *TT_adr = TT;

  return(OK);
}


/***********************************************/
/* UTC_ISO_TIME_STRING_TO_TT_ISO_TIME_STRING   */
/***********************************************/
/* Convert an absolute UTC time specified as an ISO (T) time string into an      */
/*   absolute TT time (also specified as an ISO time string).                    */
/*                                                                               */
/* INPUT:  UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                 */
/* OUTPUT: TT_string in ISO format, e.g., 1972-01-01T00:00:42.184                */
/*                                                                               */
/* NOTE:   This function is only valid for times after 1972-01-01T00:00:00 UTC.  */

int utc_iso_time_string_to_tt_iso_time_string(char *UTC_string, char **TT_string_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  int        y2, t2, d2, h2, m2;
  double     s2;
  double     leapsec_count;
  double     delta;
  /*  char       infunc[] = "utc_iso_time_string_to_tt_iso_time_string"; */

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  utc_time_components_to_leapsec_count(y0, t0, d0, h0, m0, s0, &leapsec_count);
  delta = leapsec_count+32.184;
  /* printf("leap adjustment  = %.3f\n", leapsec_count); */
  /* printf("TAI adjustment   = %.3f\n", 32.184); */
  /* printf("Total adjustment = %.3f\n", delta); */
  standardize_time_components(y0, t0, d0, h0, m0, s0+delta, &y2, &t2, &d2, &h2, &m2, &s2);
  compose_iso_time_string(y2, t2, d2, h2, m2, s2, TT_string_adr);

  return(OK);
}

/**********************************************/
/* UTC_ISO_TIME_STRING_TO_UT1_ISO_TIME_STRING */
/**********************************************/
/* Convert an absolute UTC time specified as an ISO (T) time string into an absolute UT1 */
/*   ISO time string                                                                     */
/*                                                                                       */
/* INPUT:                                                                                */
/* ------                                                                                */
/*   UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                               */
/*   Delta_UT1:   scalar value indicating UT1-UTC difference that is correct             */
/*                   for the date represented by UTC_string.                             */
/*                                                                                       */
/* OUTPUT:                                                                               */
/* ------                                                                                */
/* UT1_string - UT1 time in ISO format, e.g., 1972-01-01T00:00:42.184                    */
/*                                                                                       */
/* NOTES:                                                                                */
/* -----                                                                                 */
/* See also lookup_delta_ut1().                                                          */

int utc_iso_time_string_to_ut1_iso_time_string(char *UTC_string, double Delta_UT1, char **UT1_string_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  int        y2, t2, d2, h2, m2;
  double     s2;
  /*  char  infunc[] = "utc_iso_time_string_to_ut1_iso_time_string"; */

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  standardize_time_components(y0, t0, d0, h0, m0, s0+Delta_UT1, &y2, &t2, &d2, &h2, &m2, &s2);

  compose_iso_time_string(y2, t2, d2, h2, m2, s2, UT1_string_adr);

  return(OK);
}

/******************************************/
/* UTC_ISO_TIME_STRING_TO_LEAPSEC_COUNT   */
/******************************************/
/* Given an absolute UTC time specified as an ISO (T) time string, determine number of */
/*   leap seconds that have occurred prior to this time.                               */
/*                                                                                     */
/* INPUT:  UTC_string in ISO format, e.g., 1972-01-01T00:00:00.0                       */  
/* OUTPUT: integer number of leap seconds.                                             */
/* NOTE:   This function is only valid for UTC times after 1972-01-01T00:00:00 UTC.    */

int utc_iso_time_string_to_leapsec_count(char *UTC_string, double *leapsec_count_adr)
{
  int        y0, t0, d0, h0, m0;
  double     s0;
  char       infunc[] = "utc_iso_time_string_to_leapsec_count";

  parse_iso_time_string(UTC_string, &y0, &t0, &d0, &h0, &m0, &s0);
  if (y0 < 1972) {
    fprintf(stderr, "ERROR (%s): function only valid for UTC times after 1972-01-01T00:00:00, not %s\n", infunc, UTC_string);
    return(ERR);
  }
  utc_time_components_to_leapsec_count(y0, t0, d0, h0, m0, s0, leapsec_count_adr);

  return(OK);
}

/*****************************************************************/
/* UTC_ISO_TIME_STRING_TO_DUT1TBL_TIME_STRING                    */
/*****************************************************************/
/* Takes UTC iso-format time string and converts to DUT1 Table format (still UTC
content) */
/* NOTE: This function truncates input seconds to whole seconds. */

int utc_iso_time_string_to_dut1tbl_time_string(char *UTC_string, char **DUT1_table_string_adr)
{
  int y1, t1, d1, h1, m1;
  double s1;

  parse_iso_time_string(UTC_string, &y1, &t1, &d1, &h1, &m1, &s1);
  compose_dut1_table_time_string(y1, t1, d1, h1, m1, (int) s1, DUT1_table_string_adr);

  return(OK);
}

/*=========================================================================================*/

/******************************************/
/* UTC_TIME_COMPONENTS_TO_LEAPSEC_COUNT   */
/******************************************/
/* Given a UTC time split into time components (year, month, day, hour, minute, second) */
/*   determine number of leap seconds that have occured prior to this time.             */
/*                                                                                      */
/* INPUT:  UTC time components, e.g., 1972, 1, 1, 0, 0, 0.00                            */
/* OUTPUT: number of leap seconds (an integer, but returned as a double).               */
/* NOTE:   This function is only valid for years >= 1972.                               */

int utc_time_components_to_leapsec_count(int y0, int t0, int d0, int h0, int m0, double s0, double *leapsec_count_adr)
{
  int        k;
  char       infunc[] = "utc_time_components_to_leapsec_count";

  if (utc_time_components_to_leap_table_index(y0, t0, d0, h0, m0, s0, &k) == ERR) {
    fprintf(stderr, "ERROR (%s): unable to determine leap table index\n", infunc);
    return(ERR);
  }

  *leapsec_count_adr = LEAP_TABLE_ADJ[k];

  return(OK);
}

/*******************************************/
/* UTC_TIME_COMPONENTS_TO_LEAP_TABLE_INDEX */
/*******************************************/
/* Given a UTC time split into time components (year, month, day, hour, minute, second) */
/*   determine number of leap seconds that have occurred prior to this time.            */
/*                                                                                      */
/* INPUT:  UTC time components, e.g., 1972, 1, 1, 0, 0, 0.00                            */
/* OUTPUT: number of leap seconds (an integer, but returned as a double).               */
/* NOTE:   This function is only valid for years >= 1972.                               */

int utc_time_components_to_leap_table_index(int y0, int t0, int d0, int h0, int m0, double s0, int *k_adr)
{
  int        yi, ti, di, hi, mi;
  double     si;
  int        i, k;
  char       infunc[] = "utc_time_components_to_leap_table_index";

  if (N_LEAPS == 0) {
    fprintf(stderr, "ERROR (%s): leap second table not properly initialized\n", infunc);
    return(ERR);
  }
  k = N_LEAPS-1;
  for (i = 0; i < N_LEAPS; i++) {
    yi = LEAP_TABLE_Y[i];
    ti = LEAP_TABLE_T[i];
    di = LEAP_TABLE_D[i];
    hi = LEAP_TABLE_H[i];
    mi = LEAP_TABLE_M[i];
    si = LEAP_TABLE_S[i];
    if (y0 < yi) {
      k = i-1;
      break;
    }
    else if (y0 == yi) {
      if (t0 < ti) {
        k = i-1;
        break;
      }
      else if (t0 == ti) {
        if (h0 < hi) {
          k = i-1;
           break;
        }
        else if (h0 == hi) {
          if (m0 < mi) {
            k = i-1;
            break;
          }
          else if (m0 == mi) {
            if (s0 < si) {
              k = i-1;
              break;
            }
          }
	}
      }
    }
  }

  if (k == -1) {
    fprintf(stderr, "ERROR (%s): cannot handle UTC times before 1972-01-01T00:00:00\n", infunc);
    return(ERR);
  }

  *k_adr = k;

  return(OK);
}

/*=========================================================================================*/

/**********************/
/* ACS_TO_TT_AND_UT1   */
/**********************/
/* Given the number of apparent UTC seconds that have elapsed since 2000-01-01T12:00:00 UTC, */
/*   determine the number of TT seconds since 2000-01-01T12:00:00 TT and the number of       */
/*   UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1.                            */

int acs_to_tt_and_ut1(double ACS_time, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  /*  char      infunc[] = "acs_to_tt_and_ut1"; */

  acs_to_tt(ACS_time, TT_adr);
  acs_to_ut1(ACS_time, Delta_UT1, UT1_adr);

  return(OK);
}

/**************/
/* ACS_TO_TT   */
/**************/
/* INPUT:  ACS_time is the number of apparent elapsed UTC seconds since 2000-01-01T12:00:00 UTC. */
/*                                                                                          */
/* OUTPUT: TT is the number of elapsed TT seconds since 2000-01-01T12:00:00 TT.             */
/*                                                                                          */
/* NOTE:   The duration of a TT second is the same as an SI second.                         */

int acs_to_tt(double ACS_time, double *TT_adr)
{
  char     *UTC_string;
  double   leapsec_count;
  /*  char     infunc[] = "acs_to_tt"; */

  acs_to_utc_iso_time_string(ACS_time, &UTC_string);
  utc_iso_time_string_to_leapsec_count(UTC_string, &leapsec_count);
  free((void *) UTC_string);
  *TT_adr = ACS_time + 64.184 + (leapsec_count - 32); /* 32 is leapsec count at 2000-01-01T12:00:00UTC */
                                                 /* 64.184 is time elapsed from TT epoch to SC epoch */

  return(OK);
}

/**************/
/* ACS_TO_UT1  */
/**************/
/* Convert an ACS time (apparent elapsed UTC seconds since 2000-01-01T12:00:00 UTC)  */
/*   into the number of UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1. */
/*   UT1 is closely related to UTC by design. The value of Delta_UT1 is published in */
/*   tables. See the lookup_delta_ut1() function further below.                      */

int acs_to_ut1(double ACS_time, double Delta_UT1, double *UT1_adr)
{
  /*  char     infunc[] = "acs_to_ut1"; */

  *UT1_adr = ACS_time + Delta_UT1;

  return(OK);
}

/******************************/
/* ACS_TO_UTC_ISO_TIME_STRING  */
/******************************/
/* Take apparent elpased UTC seconds since 2000-01-01T12:00:00 UTC and convert into  */
/* a UTC iso-format time string.                                           */

int acs_to_utc_iso_time_string(double ACS_time, char **UTC_string_adr)
{
  int      y1, t1, d1, h1, m1;
  double   s1;
  /* char     infunc[] = "acs_to_utc_iso_time_string"; */

  standardize_time_components(2000, 1, 1, 12, 0, ACS_time, &y1, &t1, &d1, &h1, &m1, &s1);
  compose_iso_time_string(y1, t1, d1, h1, m1, s1, UTC_string_adr);

  return(OK);
}


/*=========================================================================================*/

/**********************/
/* SI_TO_TT_AND_UT1   */
/**********************/
/* Given the number of SI seconds that have elapsed since 2000-01-01T12:00:00 UTC,     */
/*   determine the number of TT seconds since 2000-01-01T12:00:00 TT and the number of */
/*   UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1.                      */

int si_to_tt_and_ut1(double SI, double Delta_UT1, double *TT_adr, double *UT1_adr)
{
  /*  char      infunc[] = "si_to_tt_and_ut1"; */

  si_to_tt(SI, TT_adr);
  si_to_ut1(SI, Delta_UT1, UT1_adr);

  return(OK);
}

/**************/
/* SI_TO_TT   */
/**************/
/* INPUT:  SI is the number of elapsed SI seconds since 2000-01-01T12:00:00 UTC. */
/*                                                                                */
/* OUTPUT: TT is the number of elapsed TT seconds since 2000-01-01T12:00:00 TT.   */
/*                                                                                */
/* NOTE:   The duration of a TT second is the same as an SI second.               */

int si_to_tt(double SI, double *TT_adr)
{
  char     infunc[] = "si_to_tt";

  if (SI < (-64.184 - 1e-07)) { /* Add an epsilon for numerical precision errors */
    fprintf(stderr, "ERROR (%s): SI time must be >= -64.184\n", infunc);
    return(ERR);
  }

  *TT_adr = SI + 64.184;

  return(OK);
}

/**************/
/* SI_TO_UT1  */
/**************/
/* Convert an SI time (elapsed SI seconds since 2000-01-01T12:00:00 UTC)            */
/*   into the number of UT1 seconds that have elapsed since 2000-01-01T12:00:00 UT1. */
/*   UT1 is clsoely related to UTC by design. The value of Delta_UT1 is published in */
/*   tables. See the lookup_delta_ut1() function further below.                      */

int si_to_ut1(double SI, double Delta_UT1, double *UT1_adr)
{
  char     *UTC_string;
  double   leapsec_count;
  /*  char     infunc[] = "si_to_ut1"; */

  si_to_utc_iso_time_string(SI, &UTC_string);
  utc_iso_time_string_to_leapsec_count(UTC_string, &leapsec_count);
  if (UTC_string != NULL) {
    free((void *) UTC_string);
  }
  *UT1_adr = SI + Delta_UT1 - (leapsec_count - 32); /* 32 is leapsec_count at 2000-01-01T12:00:00 UTC */

  return(OK);
}

/******************************/
/* SI_TO_UTC_ISO_TIME_STRING  */
/******************************/
/* Take elpased SI seconds since 2000-01-01T12:00:00 UTC and convert into  */
/* a UTC iso-format time string.                                           */

int si_to_utc_iso_time_string(double SI, char **UTC_string_adr)
{
  double   TT;
  /* char     infunc[] = "si_to_utc_iso_time_string"; */

  si_to_tt(SI, &TT);
  tt_to_utc_iso_time_string(TT, UTC_string_adr);

  return(OK);
}

/*=========================================================================================*/

/*****************************/
/* TT_ISO_TIME_STRING_TO_TT  */
/*****************************/
/* Convert an absolute TT time (Terrestrial Time) specified as an ISO (T) */
/*   time string into TT time in seconds since the J2000.0 epoch          */
/*   (2000-01-01:12:00:00 TT).                                            */
/*                                                                        */
/* NOTE: See also the NAIF CSPICE library function STR2ET.c               */

int tt_iso_time_string_to_tt(char *TT_string, double *TT_adr)
{
  int                      year, month, day, hour, minute;
  double                   JD;
  double                   second;
  /*  char                 infunc[] = "tt_iso_time_string_to_tt"; */

  parse_iso_time_string(TT_string, &year, &month, &day, &hour, &minute, &second);
  julian_date(year, month, day, hour, minute, second, &JD);
  *TT_adr = (JD - 2451545.0) * 86400.0; /* TT seconds since J2000.0 epoch */

  return(OK);
}

/*****************************/
/* TT_ISO_TIME_STRING_TO_TDB */
/*****************************/
/* Convert a TT time (Terrestrial Time) specified as an ISO (T) time string into */
/*   TDB time (Barycentric Dynamical Time) in seconds since the J2000.0 epoch,   */
/*   which is defined as 2000-01-01:12:00:00 TT.                                 */
/*                                                                               */
/* NOTE 1: See also the NAIF CSPICE library function STR2ET.c                    */
/* NOTE 2: tt_to_tdb evaluated at J2000.0 (TT=0) returns a TDB value ~0.0007.    */
/*           Unsure whether this small TDB value should be subtracted to get     */
/*           an official elpased TDB or whether this is just ignored. For now,   */
/*           it is being ignored.                                                */

int tt_iso_time_string_to_tdb(char *TT_string, double *TDB_adr)
{
  double                   TT;
  /*  char                 infunc[] = "tt_iso_time_string_to_tdb"; */

  tt_iso_time_string_to_tt(TT_string, &TT);
  tt_to_tdb(TT, TDB_adr);

  return(OK);
}

/*=========================================================================================*/


/******************************/
/* TT_TO_UTC_ISO_TIME_STRING */
/******************************/
/* Take a TT elapsed time given as seconds since the J2000.0 epoch         */
/* (2000-01-01T12:00:00 TT) and convert into a UTC iso-format time string. */
/*                                                                         */
/*  NOTE: if TT put us inside of a leap second, we report an error.        */

int tt_to_utc_iso_time_string(double TT, char **UTC_string_adr)
{
  int      y0, t0, d0, h0, m0;
  double   s0;
  int      y1, t1, d1, h1, m1;
  double   s1;
  double   leapfix;
  int      inside_leap;
  double   tt_pre_leap_instant, tt_post_leap_instant;
  char     *UTC_tmp;
  double   utc_jd0, utc_jd;
  double   this_leap, apparent_seconds_since_j2000, total_leap_adjustment;
  int      i, k;
  char     infunc[] = "tt_to_utc_iso_time_string";

  /* Check that input is valid */
  if (TT < 0) {
    fprintf(stderr, "ERROR (%s): TT must be non-negative\n", infunc);
    return(ERR);
  }

  /* Calculate Julian date at J2000 instant using UTC time scale */
  julian_date(2000, 1, 1, 11, 58, (double) 55.816, &utc_jd0);

  /* Determine row in leap table for J2000 */ 
  utc_time_components_to_leap_table_index(2000, 1, 1, 11, 58, (double) 55.816, &k);

  inside_leap = 0;
  leapfix = LEAP_TABLE_ADJ[N_LEAPS-1] - LEAP_TABLE_ADJ[k]; /* If TT is past end of leap table */

  /* See where TT falls relative to the leap seconds that have occurred since J2000 */
  for (i = k+1; i < N_LEAPS; i++) {
    julian_date(LEAP_TABLE_Y[i], LEAP_TABLE_T[i], LEAP_TABLE_D[i], LEAP_TABLE_H[i], LEAP_TABLE_M[i], LEAP_TABLE_S[i], &utc_jd);
    apparent_seconds_since_j2000 = (utc_jd - utc_jd0) * 86400.0;
    this_leap =  LEAP_TABLE_ADJ[i] - LEAP_TABLE_ADJ[i-1];
    total_leap_adjustment =  LEAP_TABLE_ADJ[i] - LEAP_TABLE_ADJ[k];
    tt_post_leap_instant = apparent_seconds_since_j2000 + total_leap_adjustment;
    tt_pre_leap_instant = tt_post_leap_instant - this_leap;

    /* printf("tt_pre_leap_instant = %.15f, tt_post_leap_instant = %.15f\n", tt_pre_leap_instant, tt_post_leap_instant); */
    if (TT < tt_pre_leap_instant) {
      leapfix = total_leap_adjustment - this_leap;
      break;
    }
    else if ((TT >= tt_pre_leap_instant) && (TT < tt_post_leap_instant)) {
      /* TT puts us inside leap second interval. Break out. */
      inside_leap = 1;
      break;
    }
    else {
      /* TT is past the current tt_post_leap_instant - do next iteration or drop out of loop */
    }

  }
  /*  printf("after loop, inside_leap = %d, leapfix = %.15f\n", inside_leap, leapfix);*/
  if (inside_leap == 0) {
    /* Normal case - not inside leap second */
    y0 = 2000;
    t0 = 1;
    d0 = 1;
    h0 = 11;
    m0 = 58;
    s0 = 55.816 + TT - leapfix;
    standardize_time_components(y0, t0, d0, h0, m0, s0, &y1, &t1, &d1, &h1, &m1, &s1);
    compose_iso_time_string(y1, t1, d1, h1, m1, s1, UTC_string_adr);
  }
  else {
    compose_iso_time_string(LEAP_TABLE_Y[i], LEAP_TABLE_T[i], LEAP_TABLE_D[i], LEAP_TABLE_H[i], LEAP_TABLE_M[i], LEAP_TABLE_S[i], &UTC_tmp);
    fprintf(stderr, "ERROR (%s): TT falls within a leap second (%s UTC)\n", infunc, UTC_tmp);
    free((void *) UTC_tmp);
    return(ERR);
  }

  return(OK);
}

/**************/
/* TT_TO_SI   */
/**************/
/* Given a TT time as elpased SI seconds since 2000-01-01T12:00:00 TT, determine   */
/*   the elapsed time in SI seconds since 2000-01-01T12:00:00 UTC.                 */
/* NOTE:   The duration of a TT second is the same as an SI second.                */

int tt_to_si(double TT, double *SI_adr)
{
  char     infunc[] = "tt_to_si";

  if (TT < 0) { 
    fprintf(stderr, "ERROR (%s): TT time must be >= 0\n", infunc);
    return(ERR);
  }

  *SI_adr = TT - 64.184;

  return(OK);
}


/**************/
/* TT_TO_UT1  */
/**************/
/* TT is given as elapsed seconds since 2000-01-01T12:00:00 TT.       */
/* UT1 is given as elapsed UT1 seconds since 2000-01-01T12:00:00 UT1. */ 

int tt_to_ut1(double TT, double Delta_UT1, double *UT1_adr)
{
  double   SI;
  /*  char     infunc[] = "tt_to_ut1"; */

  tt_to_si(TT, &SI);
  si_to_ut1(SI, Delta_UT1, UT1_adr);

  return(OK);
}


/**************/
/* TT_TO_TDB */
/**************/
/* Convert a TT time (Terrestrial Time) in seconds since the J2000.0 epoch,   */
/*   which is defined as 2000-01-01:12:00:00 TT, into a TDB time (Barycentric */
/*   Dynamical Time) in seconds since the J2000.0 epoch.                      */
/*                                                                            */
/* NOTE 1: See also the NAIF CSPICE library function STR2ET.c                 */
/* NOTE 2: At the J2000.0 epoch, where TT = 0, this function returns a non-   */
/*           zero value for TBD (something like 0.00007).                     */

int tt_to_tdb(double TT, double *TDB_adr)
{
  double                   T_TT;
  double                   M_Earth_deg;
  double                   M_Earth_rad;
  /*  char                     infunc[] = "tt_to_tdb"; */

  T_TT = TT/(86400.0 * 36525.0);  /* T_TT is TT expressed in Julian centuries */
  M_Earth_deg = ((double) 357.5277233) + ((double) 35999.05034) * T_TT;
  M_Earth_rad = M_Earth_deg * DEG2RAD;

  /* The barycentric dynamical time is approximately given by: [formula from */
  /*   memo]. The NAIF CSPICE STR2ET function uses a slightly more complex */
  /*   formula. */
  *TDB_adr = TT + 0.001658*sin(M_Earth_rad) + 0.00001385 * sin(2*M_Earth_rad);

  return(OK);
}

/*=========================================================================================*/

/********************/
/* LOOKUP_DELTA_UT1 */
/********************/
/* Given a date in yyyymmdd form, determine through table lookup, the proper */
/*   value of Delta_UT1, the difference between UTC and UT1 for the date. */
/*   The table is stored in a file named fname. Each line has a yyyymmdd date */
/*   followed by a real value for Delta_UT1. Lines in file are sorted by increasing */
/*   date. */

int lookup_delta_ut1(char *fname, int yyyymmdd, double *Delta_UT1_adr)
{
  FILE    *fp;
  int     i;
  int     n_lines;
  int     n_tokens;
  int     *b, *e;
  char    tok0[64];
  char    tok1[64];
  int     L;
  int     yyyymmdd_i;
  int     found;
  double  Delta_UT1;
  char    line[MAXSTRING];
  char    infunc[] = "lookup_delta_ut1";

  count_lines(fname, 2, &n_lines, &fp);

  found = 0;
  for (i = 0; i < n_lines; i++) {
    fgetl(line, MAXSTRING, fp);
    tokenize(line, " ", &n_tokens, &b, &e);
    if (n_tokens != 2) {
      fprintf(stderr, "ERROR (%s): expected 2 tokens on line |%s|, found %d\n", infunc, line, n_tokens);
      return(ERR);
    }
    L = get_max_token_length(n_tokens, b, e);
    if (L > 63) {
      fprintf(stderr, "ERROR (%s): token too long on line |%s|\n", infunc, line);
      return(ERR);
    }
    extract_token(tok0, line, b, e, 0);
    extract_token(tok1, line, b, e, 1);
    free((void *) b);
    free((void *) e);

    yyyymmdd_i  = (int) atoi(tok0);
    if (yyyymmdd == yyyymmdd_i) {
      found = 1;
      Delta_UT1 = (double) atof(tok1);
      break;
    }
  }
  fclose(fp);

  if (found == 0) {
    fprintf(stderr, "ERROR (%s): lookup failed for date %d in file %s\n", infunc, yyyymmdd, fname); 
    return(ERR);
  }
  *Delta_UT1_adr = Delta_UT1;

  return(OK);
}

/********************************/
/* INITIALIZE_LEAP_SECOND_TABLE */
/********************************/
int initialize_leap_second_table(char *filename)
{
  FILE    *fp;
  char    tok0[64], tok1[64], tok2[64], tok3[64], tok4[64], tok5[64], tok6[64];
  int     n, nn, n_lines, n_tokens;
  int     y, t, d;
  double  L;
  int     *b, *e;
  int     yf, tf, df, hf, mf;
  double  sf;
  double  jd0, jd1, offset;
  char    line[MAXSTRING];
  char    infunc[] = "initialize_leap_second_table";

  if (count_lines(filename, 2, &n_lines, &fp) == ERR) {
    fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
    return(ERR);
  }
  if (n_lines > MAX_N_LEAPS) {
    fprintf(stderr, "ERROR (%s): number of lines in %s exceeds MAX_N_LEAPS = %d\n", infunc, filename, (int) MAX_N_LEAPS);
    return(ERR);
  }

  nn = 0;
  for (n = 0; n < n_lines; n++) {
    fgetl(line, MAXSTRING, fp);
    tokenize(line, " ", &n_tokens, &b, &e);
   
    /*    for (i = 0; i < n_tokens; i++) { */
    /*      extract_token(tok0, line, b, e, i);*/
    /*      printf("token %d = |%s|\n", i, tok0);*/
    /*    }*/

    if (n_tokens < 7) {
      fprintf(stderr, "ERROR (%s): expected at least 7 tokens on line |%s|, found %d\n", infunc, line, n_tokens);
      return(ERR);
    }
    /* For safety check that no tokens longer than 63; */
    /*   really only need to check that tokens 0,1,2, and 6 meet this requirement */
    L = get_max_token_length(n_tokens, b, e);
    if (L > 63) {
      fprintf(stderr, "ERROR (%s): token too long on line |%s|\n", infunc, line);
      return(ERR);
    }

    extract_token(tok0, line, b, e, 0); /* year */
    extract_token(tok1, line, b, e, 1); /* month string */
    extract_token(tok2, line, b, e, 2); /* day */
    extract_token(tok3, line, b, e, 3); /* =JD */
    extract_token(tok4, line, b, e, 4); /* juian date */
    extract_token(tok5, line, b, e, 5); /* TAI-UTC= */
    extract_token(tok6, line, b, e, 6); /* leap second adjustment */

   /* Verify that token 3 is "=JD" */
    if (strcasecmp(tok3, "=JD") != 0) {
      fprintf(stderr, "ERROR (%s): invalid tok3 = |%s| on line |%s|\n", infunc, tok3, line);
      return(ERR);
    }

    /* Verify that token 5 is "TAI-UTC=" */
    if (strcasecmp(tok5, "TAI-UTC=") != 0) {
      fprintf(stderr, "ERROR (%s): invalid tok5 = |%s| on line |%s|\n", infunc, tok5, line);
      return(ERR);
    }

    y = (int) atoi(tok0);
    if (y >= 1972) {
      /* Only process for years >= 1972 */
      if (month_string_to_month(tok1, &t) == ERR) {
        fprintf(stderr, "ERROR (%s): bad month string |%s| on line |%s|\n", infunc, tok1, line);
        return(ERR);
      }
      L = (double) atof(tok6);
      d = (int) atoi(tok2);
      julian_date(y, t, d, 0, 0, 0, &jd0); /* nominal julian date at midnight given year, month, and day */
      jd1 = (double) atof(tok4); /* supplied value of julian date in token 4 */
      offset = (jd1 - jd0) * 86400; /* Number of seconds by which to adjust leap second instant based on JD */
      standardize_time_components(y, t, d, 0, 0, offset, &yf, &tf, &df, &hf, &mf, &sf); /* negative offsets are ok */
      LEAP_TABLE_Y[nn] = yf;
      LEAP_TABLE_T[nn] = tf;
      LEAP_TABLE_D[nn] = df;
      LEAP_TABLE_H[nn] = hf;
      LEAP_TABLE_M[nn] = mf;
      LEAP_TABLE_S[nn] = sf;
      LEAP_TABLE_ADJ[nn] = L;
      /* printf("nn = %d, yf = %d, tf = %d, df = %d, hf = %d, mf = %d, sf = %.15f\n", nn, yf, tf, df, hf, mf, sf);*/
      nn++;
    }
    N_LEAPS = nn;

    free((void *) b);
    free((void *) e);
  }
  fclose(fp);
  
  return(OK);
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create safe_sqrt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:    safe_sqrt
  Author:   Mike Burl 
  Date:     20070309
  Function: Check for less than zero condition before taking sqrt.
              Issue warning and return 0 if argumnet is negative.

  History: 
*******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "burl.h"
#include "safe_sqrt.h"


/**********************/
/* SAFE_SQRT          */
/**********************/

double safe_sqrt(double a)

{
  char       infunc[] = "safe_sqrt";

  if (a < D_ZERO) {
    fprintf(stderr, "WARNING (%s): received negative operand = %.15f\n", infunc, a);
    return(D_ZERO);
  }
  else {
    return(sqrt(a));
  }
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create time_utils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     time_utils
  Author:    Mike Burl 
  Date:      2006/12/27
  Function:  This file contains a number of functions for handling
               generic time and date formats/conversions. 

  History:   2008/07/09 (MCB) - Modified standardize_time_components to allow
               a negative value for seconds. Also, modified calendar_increment
               to allow a negative value for n_days. Found a "theoretical" bug
               in standardize_time_components where if input value of month or day
               was out-of-range it would not get standardized. 

             2008/06/08 (MCB) - Added compose_dut1_table_time_string() function 
               written by Kyle/Walt.

             2007/02/20 (MCB) - removed floor function from parse_yyyymmdd as it
               is unecessary and was causing a bug under solaris. (unsafe due to 
               epsilon error in division?).

             Based on various .m files of same names.
 
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "burl.h"
#include "time_utils.h"
#include "strsel.h"
#include "sprintf_alloc.h"

/***********************/
/* GLOBAL DECLARATIONS */
/***********************/


/***********************/
/* JULIAN_DATE         */
/***********************/
int julian_date(int year, int month, int day, int hour, int minute, double second, double *jd_adr)
{
  int                  a, y, m;
  int                  jdi;
  double               jd;
  /* char                 infunc[] = "julian_date"; */

  a = (14-month)/12; /* integer division is intentional */
  y = year + 4800-a;
  m = month + 12*a-3;
  jdi = day + ((153*m+2)/5) + y/4 - (y/100) + (y/400) - 32045; /* again integer division is intentional */
  jd = jdi + 365.0 * y + ((double) (hour-12.0))/((double) 24.0) + ((double) minute)/((double) 1440.0) + second/((double) 86400.0);

  *jd_adr = jd;

  return(OK);
}


/*************************/
/* PARSE_ISO_TIME_STRING */
/*************************/
int parse_iso_time_string(char *T, int *year_adr, int *month_adr, int *day_adr, int *hour_adr, int *minute_adr, double *second_adr)
{
  int                  L;
  char                 tmp[128];
  char                 infunc[] = "parse_iso_time_string";

  L = strlen(T);
  if (L < 18) {
    fprintf(stderr, "ERROR (%s): input time string = [%s] has bad format\n", infunc, T);
    return(ERR);
  }
  if ((T[4] != '-') || (T[7] != '-') || (T[10] != 'T') || (T[13] != ':') || (T[16] != ':')) {
    fprintf(stderr, "ERROR (%s): input time string = [%s] has bad format\n", infunc, T);
    return(ERR);
  }

  strsel(tmp, T, 0, 3);
  *year_adr = (int) atoi(tmp);
  strsel(tmp, T, 5, 6);
  *month_adr = (int) atoi(tmp);
  strsel(tmp, T, 8, 9);
  *day_adr = (int) atoi(tmp);
  strsel(tmp, T, 11, 12);
  *hour_adr = (int) atoi(tmp);
  strsel(tmp, T, 14, 15);
  *minute_adr = (int) atoi(tmp);
  strsel(tmp, T, 17, L-1);
  *second_adr = (double) atof(tmp);

  return(OK);
}

/***************************/
/* COMPOSE_ISO_TIME_STRING */
/***************************/
/* OUTPUT: T - Time string is ISO (T) format: 2006-12-19T11:48:12.3457 */

int compose_iso_time_string(int year, int month, int day, int hour, int minute, double second, char **T_adr)
{
  int    wholesec;
  /*  char                 infunc[] = "compose_iso_time_string"; */

  wholesec = (int) second;
  if (wholesec >= 10) {
    sprintf_alloc(T_adr, "%04d-%02d-%02dT%02d:%02d:%.12f", year, month, day, hour,  minute, second);
  }
  else {
    sprintf_alloc(T_adr, "%04d-%02d-%02dT%02d:%02d:%d%.12f", year, month, day, hour,  minute, 0, second);
  }
  return(OK);
}

/**************************************************/
/* COMPOSE_DUT1_TABLE_TIME_STRING                 */
/**************************************************/
/* OUTPUT: T - Time string is DUT1 Table format: 12-19-2006 11:48:12 */
/*   NOTE: the input "second" is specified as an int! */

int compose_dut1_table_time_string(int year, int month, int day, int hour, int minute, int second, char **T_adr)
{

  sprintf_alloc(T_adr, "%02d-%02d-%04d %02d:%02d:%02d", month, day, year, hour, minute,
second);

  return(OK);
}

/****************/
/* IS_LEAP_YEAR */
/****************/
/* Given a year in the Gregorian (standard) calendar, determine whether it is */
/*   a leap year. */

int is_leap_year(int year)
{
  int                  leap_year_flag;
  /*  char                 infunc[] = "is_leap_year"; */

  if ((year % 400) == 0) {
    /* Divisible by 400 */
    leap_year_flag = 1;
  }
  else if ((year % 100) == 0) {
    /* Not divisible by 400, but divisible by 100 */
    leap_year_flag = 0;
  }
  else if ((year % 4) == 0) {
    /* Not divisible by 400, not divisible by 100, but divisible by 4 */
    leap_year_flag = 1;
  }
  else {
    /* Not divisible by 400, 100, or 4 */
    leap_year_flag = 0;
  }

  return(leap_year_flag);
}


/******************/
/* PARSE_YYYYMMDD */
/******************/
/* Take an input calendar date as an integer in the form yyyymmdd */
/*   and split into year, month, and day. */

int parse_yyyymmdd(int yyyymmdd, int *year_adr, int *month_adr, int *day_adr)
{
  *year_adr  = yyyymmdd/10000; /* integer-division is intentional */
  *month_adr = (yyyymmdd - 10000*(*year_adr))/100; /* interger division is intentional */
  *day_adr   = yyyymmdd - 10000*(*year_adr) - 100*(*month_adr);

  return(OK);
}

/********************/
/* COMPOSE_YYYYMMDD */
/********************/
/* Take an input calendar date split into year, month, and day and */
/*   compose a single integer representing the date in yyyymmdd form */

int compose_yyyymmdd(int year, int month, int day, int *yyyymmdd_adr)
{

  *yyyymmdd_adr = 10000*year+100*month+day;

  return(OK);
}


/******************************************/
/* STANDARDIZE_TIME_COMPONENTS            */
/******************************************/
/* Given a time split into time components (year, month, day, hour, minute, second) */
/*   standardize the components, so that:                                           */
/*                                                                                  */
/*   0 <= second < 60;                                                              */
/*   0 <= minute < 60                                                               */
/*   0 <= hour < 24                                                                 */
/*   day within range of days for month and year                                    */
/*   1 <= month <= 12                                                               */
/*                                                                                  */
/* NOTE: We assume all incoming parameter values are nonnegative except possibly    */
/*   for the value of seconds.                                                      */
/*   This func does not deal with leap seconds, e.g., if you had split a UTC        */ 
/*   time string into 2005, 12, 31, 23, 59, 65 and then called this function, you   */
/*   would get back 2006, 1, 1, 0, 0, 5, even though UTC added a leap second at     */
/*   2005-12-31T23:59:59.                                                           */
/*                                                                                  */
/* 2008/07/09 (MCB) - modified to allow s0 to be negative. Other input params must  */
/*   still be non-negative.                                                         */

int standardize_time_components(int y0, int t0, int d0, int h0, int m0, double s0, 
      int *y1_adr, int *t1_adr, int *d1_adr, int *h1_adr, int *m1_adr, double *s1_adr)
{
  static int  days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int         leap_days[12];
  int         yyyymmdd;
  int         yyyymmdd_new;
  int         i;
  char        infunc[] = "standardize_time_components";

  if ((y0 < 0) || (t0 < 0) || (d0 < 0) || (h0 < 0) || (m0 < 0)) {
    fprintf(stderr, "ERROR (%s): input values except for s0 must be non-negative\n", infunc);
    return(ERR);
  }

  /* In case month or day is out of range */
  for (i = 0; i < 12; i++) {
    leap_days[i] = 0;
  }
  leap_days[2-1] = is_leap_year(y0);
  while (t0 > 12) {
    t0 = t0-12;
    y0 = y0+1;
    leap_days[2-1] = is_leap_year(y0);
  }
  while (d0 > (days_in_month[t0-1] + leap_days[t0-1])) {
    d0 = d0 - (days_in_month[t0-1] + leap_days[t0-1]);
    t0 = t0+1;
  }  
  while (t0 > 12) {
    t0 = t0-12;
    y0 = y0+1;
    leap_days[2-1] = is_leap_year(y0);
  }
  compose_yyyymmdd(y0, t0, d0, &yyyymmdd); /* Should have a valid yyyymmdd now */


  if (s0 >= 0) {
    while (s0 >= 60) {
      s0 = s0-60;
      m0 = m0+1;
    }

    while (m0 >= 60) {
      m0 = m0-60;
      h0 = h0+1;
    }

    while (h0 >= 24) {
      h0 = h0-24;
      calendar_increment(yyyymmdd, 1, &yyyymmdd_new);
      yyyymmdd = yyyymmdd_new;
    }
  }
  else {
    while (s0 < 0) {
      s0 = s0+60;
      m0 = m0-1;
    }
    while (m0 < 0) {
      m0 = m0+60;
      h0 = h0-1;
    }
    while (h0 < 0) {
      h0 = h0+24;
      calendar_increment(yyyymmdd, -1, &yyyymmdd_new);
      yyyymmdd = yyyymmdd_new;
    }
  }

  parse_yyyymmdd(yyyymmdd, y1_adr, t1_adr, d1_adr);
  *h1_adr = h0;
  *m1_adr = m0;
  *s1_adr = s0;

  return(OK);
}

/**********************/
/* CALENDAR_INCREMENT */
/**********************/
/* Take an input calendar date as an integer in the form yyyymmdd    */
/*   and add n_days to that to get a new calendar date in the same   */
/*   yyyymmdd format.                                                */
/*                                                                   */
/* 2008/07/09 (MCB) - modified to also allow decrements (n_days < 0) */

int calendar_increment(int yyyymmdd_old, int n_days, int *yyyymmdd_new_adr)
{
  static int  days_in_month[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
  int         year, month, day;
  int         leap_days[12];
  int         i;
  /*  char        infunc[] = "calendar_increment";*/

  parse_yyyymmdd(yyyymmdd_old, &year, &month, &day);
  for (i = 0; i < 12; i++) {
    leap_days[i] = 0;
  }
  leap_days[2-1] = is_leap_year(year);

  if (n_days >= 0) {
    /* increment case */
    day = day + n_days;
    while (day > (days_in_month[month-1] + leap_days[month-1]) ) {
      day = day - (days_in_month[month-1]+leap_days[month-1]);
      month++;
      while (month > 12) {
        month -= 12;
        year++;
        leap_days[2-1] = is_leap_year(year);
      }
    }
    compose_yyyymmdd(year, month, day, yyyymmdd_new_adr);
  }
  else {
    /* decrement case */
    day = day + n_days;
    while (day < 1) {
      if (month == 1) {
        day = day + (days_in_month[12-1]+leap_days[12-1]);
      }
      else {
        day = day + (days_in_month[month-2]+leap_days[month-2]);
      }
      month--;
      while (month < 1) {
        month += 12;
        year--;
        leap_days[2-1] = is_leap_year(year);
      }
    }
    compose_yyyymmdd(year, month, day, yyyymmdd_new_adr);
  }

  return(OK);
}

/*************************/
/* MONTH_STRING_TO_MONTH */
/*************************/
/* Convert a month string to month. Only looks at first three characters, so "Janus",  */
/*   "Jan", "jan", etc. will all match to "January". The month string must be at least */
/*   three characters even though some months are unambiguous at 1. */

int month_string_to_month(char *month_str, int *month_adr)
{
  int   L;
  int   month;
  char  infunc[] = "month_string_to_month";

  L = strlen(month_str);
  if (L < 3) {
    fprintf(stderr, "ERROR (%s): month string %s does not contain at least 3 chars\n", infunc, month_str);
    *month_adr = 0;
    return(ERR);
  } 

  if (strncasecmp(month_str, "jan", 3) == 0) {
    month = 1;
  }
  else if (strncasecmp(month_str, "feb", 3) == 0) {
    month = 2;
  }
  else if (strncasecmp(month_str, "mar", 3) == 0) {
    month = 3;
  }
  else if (strncasecmp(month_str, "apr", 3) == 0) {
    month = 4;
  }
  else if (strncasecmp(month_str, "may", 3) == 0) {
    month = 5;
  }
  else if (strncasecmp(month_str, "jun", 3) == 0) {
    month = 6;
  }
  else if (strncasecmp(month_str, "jul", 3) == 0) {
    month = 7;
  }
  else if (strncasecmp(month_str, "aug", 3) == 0) {
    month = 8;
  }
  else if (strncasecmp(month_str, "sep", 3) == 0) {
    month = 9;
  }
  else if (strncasecmp(month_str, "oct", 3) == 0) {
    month = 10;
  }
  else if (strncasecmp(month_str, "nov", 3) == 0) {
    month = 11;
  }
  else if (strncasecmp(month_str, "dec", 3) == 0) {
    month = 12;
  }
  else {
    fprintf(stderr, "ERROR (%s): could not find match for month string %s\n", infunc, month_str);
    *month_adr = 0;
    return(ERR);
  }

  *month_adr = month;

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create strsel.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************
Title:    strsel.c
Author:   Mike Burl
Date:     2005/01/05

Function: Pick a substring out of a bigger string

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "utils_return_values.h"
#include "qmalloc.h"
#include "strsel.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* strsel                             */
/**************************************/

/* Note: dest must be preallocated to have at least i1-i0+2 elements. */

int strsel(char *dest, char *src, int i0, int i1)
{
  int                    i, j, L;
  /*  char                   infunc[] = "strsel"; */

  /*--------------------------------------------------------------*/
  L = i1-i0+1;
  for (i = i0, j = 0; i <= i1; i++, j++) {
    dest[j] = src[i];
  }
  dest[L] = '\0';

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create count_lines.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

Title:    count_lines
Author:   Mike Burl
Date:     Oct 16, 1998
Function: This is the C-version of the count_lines.m routine I wrote for MATLAB.
            There are four different ways to call the function, as listed under USAGE.
            For the most basic version (a), the function opens a file, counts the 
            number of lines and closes the file. For (b), we leave the file open
            and "rewound". For (c) we count lines beginning with certain comment
            characters separately. Option (d) is a combination of (b) and (c).
 
Usage:    (1) count_lines(filename, 1, &n_lines);
          (2) count_lines(filename, 2, &n_lines, &fp);
          (3) count_lines(filename, 3, cc, &n_lines, &n_comments);
          (4) count_lines(filename, 4, cc, &n_lines, &n_comments, &fp);

NOTE:    A shortcut for remembering the correct value of _usage_ is to
           count the number of variable arguments (i.e., the args after
           _usage_ in the function call).

*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include "burl.h"
#include "count_lines.h"

#ifndef MAXBUF
#define MAXBUF 4096
#endif

/**********************************/
/* count_lines                    */
/**********************************/

int count_lines(char *filename, int usage, ...)

{
  int         *nl_adr;
  FILE        **fp_adr, *fp;
  int         *nc_adr;
  char        *cc;
  va_list     ap;
  char        buffer[MAXBUF];
  int         nl, nc;
  char        infunc[] = "count_lines";

  /*-------------------------------------------------------------------------------*/
  va_start(ap, usage);

  switch(usage) {
    case 1: {
      nl_adr = va_arg(ap, int *);
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      nl = 0;
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        ++nl;
      }
      *nl_adr = nl;
      fclose(fp);
      break;
    }
    case 2: {
      nl_adr = va_arg(ap, int *);
      fp_adr = va_arg(ap, FILE **);
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      nl = 0;
      while (fgets(buffer,MAXBUF, fp) != NULL) {
        ++nl;
      }

      *nl_adr = nl;
      *fp_adr = fp;
      rewind(fp);
       
      break;
    }
    case 3: {
      cc = va_arg(ap, char *);
      /* printf("Comment characters: %s\n", cc);*/
      nl_adr = va_arg(ap, int *);
      nc_adr = va_arg(ap, int *);
      nl = 0;
      nc = 0;
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERROR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        if (buffer[0] == cc[0]) {
          ++nc;
        }
        else {
          ++nl;
        }
      }

      *nl_adr = nl;
      *nc_adr = nc;
      fclose(fp);
      break;
    }
    case 4: {
      cc = va_arg(ap, char *);
      printf("Comment characters: %s\n", cc);
      nl_adr = va_arg(ap, int *);
      nc_adr = va_arg(ap, int *);
      fp_adr = va_arg(ap, FILE **);
      fp = fopen(filename, "r");

      nl = 0;
      nc = 0;
      if ((fp = fopen(filename, "r")) == NULL) {
        fprintf(stderr, "ERR (%s): can't open %s\n", infunc, filename);
        return(ERR);
      }
      while (fgets(buffer, MAXBUF, fp) != NULL) {
        if (buffer[0] == cc[0]) {
          ++nc;
        }
        else {
          ++nl;
        }
      }

      *nl_adr = nl;
      *nc_adr = nc;
      *fp_adr = fp;
      rewind(fp);
      break;
    }
    default: {
      fprintf(stderr, "ERROR (%s): usage %d is not supported\n", infunc, usage);       
      return(ERR);
    }
  }

  va_end(ap);

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create tokenize.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utils_return_values.h"
#include "qmalloc.h"
#include "tokenize.h"

/*******************************************************************************

Title:    tokenize.c (also includes function extract_token)
Author:   Mike Burl
Date:     20030611
Function: Find beginning and end of each token in line. Tokens are separated by 
            sep_chars or white space.

History:  2005/02/09 (MCB) - Added a function to determine maximum token length, 
            since this is commonly needed (for example, to allocatate an array of 
            characters of appropriate size to hold any of the tokens).

*******************************************************************************/


/*******************************/
/* TOKENIZE                    */
/*******************************/
/* Find beginning and end of each token in line. Tokens are separated by sep_chars or
   white space. White space or separators within a quoted string are ignored. i.e., the
   quoted string is returned as a single token. Also, if multiple separators occur
   with no valid non-sep/non-white characters between, it is treated as
   no token, rather than some kind of null token. */

int tokenize(char *line, char *sep_char, int *n_adr, int **b_adr, int **e_adr)
{
  int     l, s;
  int     *b, *e;
  int     *btmp, *etmp;
  int     i, k, n;
  int     inquote, intoken;
  int     white_space, sep;
  char    infunc[] = "tokenize";

  l = strlen(line);
  s = strlen(sep_char);
  
  btmp = (int *) qmalloc(l, sizeof(int), 1, infunc, "btmp");
  etmp = (int *) qmalloc(l, sizeof(int), 1, infunc, "etmp");
  
  inquote = 0;
  intoken = 0;
  n = 0;
  for (i = 0; i < l; i++) {
    if (inquote == 1) {
      /* See if this char ends the quote */
      if (line[i] == '"') {
        inquote = 0; 
      }
    }
    else {
      /* See if this char starts a quote */
      if (line[i] == '"') {
        inquote = 1;
        btmp[n] = i;
        intoken = 1;
      }
      else {
        /* See if this char is white space */
        if ((line[i] == ' ') || (line[i] == '\t') || (line[i] == '\n')) {
          white_space = 1;
        }
        else {
          white_space = 0;
        }
        /* See if this char is one of the sep chars */
        sep = 0;
        for (k = 0; k < s; k++) {
          if (line[i] == sep_char[k]) {
            sep = 1;
            break;
          }
        }
        if ((white_space == 1) || (sep == 1)) {  
          if (intoken == 1) {
            etmp[n] = i-1;
            intoken = 0;
            n++;
          }
        }
        else {
          /* Regular character */
          if (intoken == 0) {
            btmp[n] = i;
            intoken = 1;
	  }
	}
      }
    }
  }

  if (inquote == 1) {
    fprintf(stderr, "ERROR (%s): mismatched quotes in line %s\n", infunc, line);
    return(ERR);
  }
  if (intoken == 1) {
    /* Need to close it off */
    etmp[n] = l-1;
    intoken = 0;
    n++;
  }

  /* Copy the good data from btmp and etmp to e and b */
  b = (int *) qmalloc(n, sizeof(int), 1, infunc, "b");
  e = (int *) qmalloc(n, sizeof(int), 1, infunc, "e");
  for (k = 0; k < n; k++) {
    b[k] = btmp[k];
    e[k] = etmp[k];
  }
  *e_adr = e;
  *b_adr = b;
  *n_adr = n;
  free(btmp);
  free(etmp);
 
  return(OK);
}

/*******************************/
/* GET_MAX_TOKEN_LENGTH        */
/*******************************/
int get_max_token_length(int n_tokens, int *bt, int *et)
{
  int   i;
  int   L;

  L = 0;
  for (i = 0; i < n_tokens; i++) {
    if (et[i] - bt[i] + 1 > L) {
      L = et[i]-bt[i]+1;
    }
  }
  return(L);
}

/*******************************/
/* EXTRACT_TOKEN               */
/*******************************/
/* Extract w-th token from source and copy to dest with termination.
   Dest must be preallocated to at least e[w]-b[w]+2 characters. */

int extract_token(char *dest, char *src, int *b, int *e, int w)
{
  /*  char    infunc[] = "extract_token"; */

  strncpy(dest, src+b[w], e[w]-b[w]+1);
  dest[e[w]-b[w]+1] = '\0';
 
  return(OK);
}

/*******************************/
/* GET_TOKEN_LIST              */
/*******************************/
/* First call tokenize to determine the beginning and end positions of the tokens */
/* and the number of tokens, then call this function to extract each token into */
/* the equivalent of a cell array of strings */

int get_token_list(char *src, int n_tokens, int *b, int *e, char ***token_list_adr)
{
  int  i, L;
  char **token_list;
  char infunc[] = "get_token_list";

  token_list = (char **) qmalloc(n_tokens, sizeof(char *), 0, infunc, "token_list");
  *token_list_adr = token_list;

  for (i = 0; i < n_tokens; i++) {
    L = e[i] - b[i] + 1;
    token_list[i] = (char *) qmalloc((L+1), sizeof(char), 0, infunc, "token_list[i]");
    extract_token(token_list[i], src, b, e, i);
  }

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create fgetl.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:    fgetl
  Author:   Mike Burl 
  Function: Similar to the fgets() library function except that the
            carriage return and newline characters are not included
            as part of the line. Gets at most n characters per line
            where n is a user-specified value.

*******************************************************************************/
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include "fgetl.h"

char *fgetl(char *s, int n, FILE *fp)

{
  int  L;
  char *val;
  int  i;

  if ((val = fgets(s, n, fp)) != NULL) {
    L = strlen(s);
    i = L-1;
    while ((i >= 0) && ((s[i] == '\n') || (s[i] == '\r'))) {
      s[i--] = '\0';
    }
  }

  return(val);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create sprintf_alloc.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*******************************************************************************

  Title:     sprintf_alloc
  Author:    Mike Burl
  Date:      2005/03/16
  Function:  Routine to do an sprintf into a string that is allocated to be
               exactly the right size to hold it. Note that unlike sprintf,
               the first arg to sprintf_alloc must be **char (NOT *char).

  History:   

*******************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <strings.h>
#include <stdarg.h>
#include "qmalloc.h"
#include "burl.h"
#include "sprintf_alloc.h"


#define MAXSPRINTF 32768

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/


/**************************************/
/* sprintf_alloc                      */
/**************************************/

int sprintf_alloc(char **S_adr, char *fmt, ...)

{
  va_list   ap;
  char      buffer[MAXSPRINTF];
  char      *S;
  int       L;
  char      infunc[] = "sprintf_alloc";

/*-------------------------------------------------------------------------------*/
  va_start(ap, fmt);
  vsprintf(buffer, fmt, ap);
  va_end(ap);
  L = strlen(buffer);
  S = (char *) qmalloc((L+1), sizeof(char), 0, infunc, "S");
  sprintf(S, "%s", buffer);
  *S_adr = S;

  return(OK);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoTieUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "cartoTieUtils.h"

void rot90(double* tie, int nrot)
{
   int i,j;
   double tmp0,tmp1;
   
   for (i=0;i<nrot;i++)
      {
      tmp0 = tie[0];
      tmp1 = tie[1];
      for (j=0;j<6;j++) tie[j] = tie[j+2];
      tie[6] = tmp0;
      tie[7] = tmp1;
      }
    
   return;
}

void swp90(double* tie, int nrot)
{
   int i;
   double tmp4,tmp5;
   
   /* this is swapping lines for samples for 90 deg rotate */
   
   for (i=0;i<nrot;i++)
      {
      tmp4 = tie[4];
      tmp5 = tie[5];
      tie[2] = tmp5;
      tie[4] = tmp5;
      tie[5] = tmp4;
      tie[7] = tmp4;
      }
    
   return;
}

void flip(double* tie)
{
   double tmp0,tmp1;
   
   tmp0 = tie[0];
   tmp1 = tie[1];
   tie[0] = tie[6];
   tie[1] = tie[7];
   tie[6] = tmp0;
   tie[7] = tmp1;
   
   tmp0 = tie[2];
   tmp1 = tie[3];
   tie[2] = tie[4];
   tie[3] = tie[5];
   tie[4] = tmp0;
   tie[5] = tmp1;
    
   return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibisControlMapper.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdlib.h>
#include <assert.h>

#include "ibisControlMapper.h"
#include "ibishelper.h"

#define MAXCONTROLS 50

/******************************************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getMapper(IBISStruct *ibis, int controlCol)
{
   IBIS_CONTROL_MAPPER *mapper;
   int i, j, ncontrols, found, counts[MAXCONTROLS];
   double controls[MAXCONTROLS];

   if(controlCol <= 0) return IBISCONTROL_getSingleMapper(ibis);
   --controlCol;

   ncontrols = 0;
   for(i = 0; i < ibis->nr; i++)
   {
      found = 0;
      for(j = 0; j < ncontrols; j++)
      {
         if(IBISHELPER_getDouble(ibis, controlCol, i) == controls[j])
         {
            found = 1;
            break;
         }
      }
      if(!found)
      {
         controls[ncontrols] = IBISHELPER_getDouble(ibis, controlCol, i);
         counts[ncontrols++] = 1;
      }
      else
         counts[j]++;
   }

   mapper = (IBIS_CONTROL_MAPPER*)malloc(sizeof(IBIS_CONTROL_MAPPER));
   mapper->nControls = ncontrols;

   mapper->maps = (IBIS_CONTROL_MAP**)malloc(sizeof(IBIS_CONTROL_MAP*)*ncontrols);
   for(i = 0; i < ncontrols; i++)
      mapper->maps[i] = IBISCONTROL_getMap(ibis, controlCol, controls[i], counts[i]);

   return mapper;
}

/******************************************************/
IBIS_CONTROL_MAPPER* IBISCONTROL_getSingleMapper(IBISStruct *ibis)
{
   int i;
   IBIS_CONTROL_MAPPER *mapper;

   mapper = (IBIS_CONTROL_MAPPER*)malloc(sizeof(IBIS_CONTROL_MAPPER));
   mapper->nControls = 1;
   mapper->maps = (IBIS_CONTROL_MAP**)malloc(sizeof(IBIS_CONTROL_MAP*));
   mapper->maps[0] = (IBIS_CONTROL_MAP*)malloc(sizeof(IBIS_CONTROL_MAP));

   mapper->maps[0]->length = ibis->nr;
   mapper->maps[0]->toIbisIndices = (int*)malloc(sizeof(int)*mapper->maps[0]->length);

   for(i = 0; i < ibis->nr; i++)
      mapper->maps[0]->toIbisIndices[i] = i;

   return mapper;
}

/******************************************************/
IBIS_CONTROL_MAP* IBISCONTROL_getMap(IBISStruct *ibis, int controlCol, double control, int count)
{
   int i;
   IBIS_CONTROL_MAP *map;

   map = (IBIS_CONTROL_MAP*)malloc(sizeof(IBIS_CONTROL_MAP));
   map->controlID = control;
   map->toIbisIndices = (int*)calloc(count, sizeof(int));
   map->length = 0;

   for(i = 0; i < ibis->nr; i++)
      if(IBISHELPER_getDouble(ibis, controlCol, i) == control)
         map->toIbisIndices[(map->length)++] = i;

   assert(map->length == count);

   return map;
}

/******************************************************/
void IBISCONTROL_deleteMapper(IBIS_CONTROL_MAPPER** mapper)
{
   int i;

   for(i = 0; i < (*mapper)->nControls; i++)
      IBISCONTROL_deleteMap(&((*mapper)->maps[i]));

   free((*mapper)->maps);
   free(*mapper);
}

/******************************************************/
void IBISCONTROL_deleteMap(IBIS_CONTROL_MAP** map)
{
   free((*map)->toIbisIndices);
   free(*map);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ibishelper.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "zmabend.h"
#include "zvproto.h"
#include "applic.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "ibishelper.h"
#include "cartoLinkedList.h"

/*****************************************************************/
void IBISHELPER_lowerString(char *dest, char *source)
{
   int len, i;

   len = strlen(source);

   for(i = 0; i < len; i++)
      dest[i] = tolower(source[i]);
   dest[i] = 0;

   //   printf("dest: %s source: %s\n", dest, source);
}

/*****************************************************************/
void IBISHELPER_checkModePrecondition(char *mode)
{
   char lowerMode[7] = "";

   IBISHELPER_lowerString(lowerMode, mode);
   if(!strcmp(lowerMode, "read")) return;
   if(!strcmp(lowerMode, "write")) return;
   if(!strcmp(lowerMode, "owrite")) return;
   if(!strcmp(lowerMode, "update")) return;

   printf("Error at IBISHELPER_checkModePrecondition.\n");
   printf("Input mode must be either read, write, owrite, or update.\n");
   zabend();
}

/*****************************************************************/
void IBISHELPER_checkFormatPrecondition(char *fmt)
{
   char lowerMode[6];

   IBISHELPER_lowerString(lowerMode, fmt);
   if(lowerMode[0] == 'a') return;
   if(!strcmp(lowerMode, "full")) return;
   if(!strcmp(lowerMode, "real")) return;
   if(!strcmp(lowerMode, "doub")) return;

   printf("Error at checkFormatPrecondition.\n");
   printf("Column format must be either full, real, or alpha-numeric.\n");
   zabend();
}

/*****************************************************************/
void IBISHELPER_readIBISData(IBISStruct *ibis)
{
   int i, status;

   for(i = 0; i < ibis->nc; i++)
   {
      status = IBISColumnRead(ibis->handle, (ibis->data)[i], i+1, 1, ibis->nr);
      if(status != 1) IBISSignal(ibis->handle, status, 1);
   }
}

/*****************************************************************/
void IBISHELPER_wrongFormatError(IBISStruct *ibis, int col)
{
   printf("Invalid column format found -- col %d as %s.\n", col, (ibis->formats)[col]);
   zabend();
}

/*****************************************************************/
IBISStruct* IBISHELPER_openIBIS(char *name, int instance, char *mode)
{
   int status, i, *lengths;
   IBISStruct *ibis;
   char lowerMode[5], *formats[MAXCOLS];
   char validColFormats[6][5] = {"byte", "half", "full", "real", "doub", "comp"};

   ibis = (IBISStruct *)malloc(sizeof(IBISStruct));

   IBISHELPER_lowerString(lowerMode, mode);
   ibis->totDataSize = 0;
   IBISHELPER_checkModePrecondition(mode);
   strcpy(ibis->mode, mode);
   lengths = ibis->colLens;

   if(!strcmp(name, "inp"))
      status = zvunit(&(ibis->unit), "inp", instance, NULL);
   else
   {
      int dumcnt, dumdef;
      char fname[200];

      status = zvparm("classibis", fname, &dumcnt, &dumdef, 1, 200);
      if(status != 1) zmabend("Error while acquiring classibis file name.\n");
      status = zvunit(&(ibis->unit), "classibis", instance, "u_name", fname, NULL);
   }
   if(status != 1) zmabend("Error while acquiring class ibis file.\n");

   status = IBISFileOpen(ibis->unit, &(ibis->handle), ibis->mode, 0, 0, 0, 0);
   if(status != 1) IBISSignalU(ibis->unit, status, 1);
   IBISFileGet(ibis->handle, "nr", &(ibis->nr), 1, 1, 0);
   IBISFileGet(ibis->handle, "nc", &(ibis->nc), 1, 1, 0);
   IBISFileGet(ibis->handle, "formats", ibis->formats, 1, MAXCOLS, 6);
   for(i = 0; i < ibis->nc; i++)
   {
       IBISHELPER_lowerString((ibis->formats)[i], (ibis->formats)[i]);
       formats[i] = (ibis->formats)[i];
   }

   for(i = 0; i < ibis->nc; i++)
   {
       int j;

       for(j = 0; j < 6; j++)
         if(!strcmp(formats[i], validColFormats[j])) break;

       if(j == 6 && formats[i][0] == 'a')
       {
           sscanf(formats[i], "a%d", &lengths[i]);
           ++(lengths[i]);
           continue;
       }

       if(j == 0) lengths[i] = sizeof(char);
       else if(j == 1) lengths[i] = sizeof(short int);
       else if(j == 2) lengths[i] = sizeof(int);
       else if(j == 3) lengths[i] = sizeof(float);
       else if(j == 4) lengths[i] = sizeof(double);
       else if(j == 5) lengths[i] = 2*sizeof(float);
       else IBISHELPER_wrongFormatError(ibis, i);
   }

   ibis->data = (char **)malloc(sizeof(void *)*(ibis->nc));
   for(i = 0; i < ibis->nc; i++)
   {
       int size;

       size = lengths[i]*ibis->nr;
       ibis->data[i] = (char *)malloc(size);
       ibis->totDataSize += size;
   }

   IBISHELPER_readIBISData(ibis);

   return ibis;
}

/*****************************************************************/
void IBISHELPER_setFormats(IBISStruct *ibis, char **formats)
{
   int i;

   for(i = 0; i < ibis->nc; i++)
   {
      IBISHELPER_checkFormatPrecondition(formats[i]);
      strcpy(ibis->formats[i], formats[i]);
   }
}

/*****************************************************************/
int IBISHELPER_getLenByFormat(char *fmt)
{
   int len;
   char lowerFmt[6];

   IBISHELPER_lowerString(lowerFmt, fmt);
   if(lowerFmt[0] == 'a')
   {
      sscanf(lowerFmt, "a%d", &len);
      ++len;
   }
   else if(!strcmp(lowerFmt, "doub"))
      len = sizeof(double);
   else if(!strcmp(lowerFmt, "real"))
      len = sizeof(float);
   else if(!strcmp(lowerFmt, "byte"))
      len = sizeof(char);
   else if(!strcmp(lowerFmt, "half"))
      len = sizeof(short);
   else if(!strcmp(lowerFmt, "full"))
      len = sizeof(int);
   else
   {
      printf("%s is unsupported column type.", lowerFmt);
      zabend();
   }

   return len;
}


/*****************************************************************/
void IBISHELPER_setColumnWidths(IBISStruct *ibis)
{
   int i;

   for(i = 0; i < ibis->nc; i++)
      ibis->colLens[i] = IBISHELPER_getLenByFormat(ibis->formats[i]);
}

/*****************************************************************/
void IBISHELPER_setTotRecordSize(IBISStruct *ibis)
{
   int i;

   ibis->totRecSize = 0;
   for(i = 0; i < ibis->nc; i++) ibis->totRecSize += ibis->colLens[i];
}

/*****************************************************************/
IBISStruct* IBISHELPER_openIBIS_out(char **format, int inst, int nr, int nc)
{
   int status, i;
   IBISStruct *ibis;

   ibis = (IBISStruct*)malloc(sizeof(IBISStruct));
   ibis->nr = nr;
   ibis->nc = nc;
   IBISHELPER_setFormats(ibis, format);
   IBISHELPER_setColumnWidths(ibis);
   IBISHELPER_setTotRecordSize(ibis);
   ibis->totDataSize = ibis->totRecSize*ibis->nr;
   strcpy(ibis->mode, "write");

   status = zvunit(&(ibis->unit), "out", inst, NULL);
   assert(status == 1);

   ibis->data = (char**)malloc(sizeof(char*)*ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      ibis->data[i] = (char*)calloc(ibis->nr, IBISHELPER_getLenByFormat(ibis->formats[i]));

   return ibis;
}

/*****************************************************************/
void IBISHELPER_writeIBIS(IBISStruct *ibis)
{
   int i, status;
   char *fmts;

   fmts = (char*)malloc(sizeof(char)*6*ibis->nc);
   for(i = 0; i < ibis->nc; i++) strncpy(fmts+6*i, ibis->formats[i], 6);

   if(!strcmp(ibis->mode, "write"))
   {
      status = IBISFileOpen(ibis->unit, &(ibis->handle), ibis->mode, ibis->nc, ibis->nr, fmts, NULL);
      assert(status == 1);
   }

   for(i = 0; i < ibis->nc; i++)
   {
      char *dataPtr;

      dataPtr = IBISHELPER_getBufPtr(ibis, i, 0);

      status = IBISColumnWrite(ibis->handle, dataPtr, i+1, 1, ibis->nr);
      assert(status == 1);
   }

   free(fmts);
}

/*****************************************************************/
void IBISHELPER_closeIBIS(IBISStruct **ibis)
{
   int i, status;

   if(!strcmp((*ibis)->mode, "write") || !strcmp((*ibis)->mode, "update"))
      IBISHELPER_writeIBIS(*ibis);
   status = IBISFileClose((*ibis)->handle, 0);
   if(status != 1) IBISSignal((*ibis)->handle, status, 1);

   for(i = 0; i < (*ibis)->nc; i++)
      if(((*ibis)->data)[i] != NULL) free(((*ibis)->data)[i]);
   free((*ibis)->data);

   free(*ibis);
}

/*****************************************************************/
int IBISHELPER_getMaxColLen(IBISStruct *ibis)
{
   int i, max;

   max = 0;
   for(i = 1; i < ibis->nc; i++)
      if((ibis->colLens)[max] < (ibis->colLens)[i]) max = i;

   return max;
}

/*****************************************************************/
void IBISHELPER_setString(IBISStruct *ibis, int col, int index, char *str)
{
   char *dataPtr;

   dataPtr = IBISHELPER_getBufPtr(ibis, col, index);
   assert(ibis->formats[col][0] == 'a' || ibis->formats[col][0] == 'A');
   strncpy(dataPtr, str, ibis->colLens[col]-1);
   *(dataPtr+(ibis->colLens[col]-1)) = 0;
}

/*****************************************************************/
void IBISHELPER_setDouble(IBISStruct *ibis, int col, int index, double data)
{
   char *dataPtr;

   dataPtr = IBISHELPER_getBufPtr(ibis, col, index);

   switch(tolower(ibis->formats[col][0]))
   {
      case 'a': printf("Please use IBISHELPER_setString function.\n");
                zabend();
                break;
      case 'c': printf("Complex not supported for now.\n");
                zabend();
                break;
      case 'b': ((char*)dataPtr)[0]      = (char)data;
                break;
      case 'h': ((short int*)dataPtr)[0] = (short int)data;
                break;
      case 'f': ((int*)dataPtr)[0]       = (int)data;
                break;
      case 'r': ((float*)dataPtr)[0]     = (float)data;
                break;
      case 'd': ((double*)dataPtr)[0]    = (double)data;
                break;
   }
}

/*****************************************************************/
int IBISHELPER_getInt(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (int)(data[0]);
      case 'h': return (int)(((short int*)data)[0]);
      case 'f': return (int)(((int*)data)[0]);
      case 'r': return (int)(((float*)data)[0]);
      case 'd': return (int)(((double*)data)[0]);
   }

   assert(0);
   return 0;
}

/*****************************************************************/
float IBISHELPER_getFloat(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (float)(data[0]);
      case 'h': return (float)(((short int*)data)[0]);
      case 'f': return (float)(((int*)data)[0]);
      case 'r': return (float)(((float*)data)[0]);
      case 'd': return (float)(((double*)data)[0]);
   }

   assert(0);
   return 0.0;
}

/*****************************************************************/
double IBISHELPER_getDouble(IBISStruct *ibis, int col, int index)
{
   char *format;
   char *data;

   data = IBISHELPER_getBufPtr(ibis, col, index);
   format = (ibis->formats)[col];
   switch(format[0])
   {
      case 'a': printf("Integer requested from a non numeric column.\n");
                printf("Please use IBISHELPER_getString function.\n");
                zabend();
      case 'c': printf("Integer requested from a complex column.\n");
                printf("Complex not supported for now.\n");
                zabend();
      case 'b': return (double)(data[0]);
      case 'h': return (double)(((short int*)data)[0]);
      case 'f': return (double)(((int*)data)[0]);
      case 'r': return (double)(((float*)data)[0]);
      case 'd': return (double)(((double*)data)[0]);
   }

   //   printf("%s\n", format[0]);
   printf("num of columns: %d col requested: %d\n", ibis->nc, col);
   zmabend("IBISHELPER_getDouble - data from outside the number of columns may have been requested.");
   return 0.0;
}

/*****************************************************************/
void IBISHELPER_getString(IBISStruct *ibis, char *buf, int col, int index)
{
   char *format;
   char *data;

   format = (ibis->formats)[col];
   if(format[0] != 'a')
   {
      printf("A string was requested from a non-string column -- col: %d index: %d format: %s\n", col, index, format);
      zabend();
   }

   data = IBISHELPER_getBufPtr(ibis, col, index);

   strcpy(buf, data);
}

/*****************************************************************/
char* IBISHELPER_getBufPtr(IBISStruct *ibis, int col, int index)
{
   if(col > ibis->nc)
   {
      printf("Requested column does not exist in the ibis file.\n");
      printf("col requested: %d  number of columns available: %d\n", col, ibis->nc);
      zabend();
   }
   if(index > ibis->nr)
   {
      printf("Requested index does not exist in the ibis file.\n");
      printf("Requested row: %d  number of rows available: %d\n", index, ibis->nr);
      zabend();
   }

   return (ibis->data)[col] + (index*(ibis->colLens)[col]);
}

/*****************************************************************/
void IBISHELPER_getFormats(IBISStruct *ibis, char formats[MAXCOLS][30])
{
   int i;
   char *type;

   for(i = 0; i < ibis->nc; i++)
   {
      type = (ibis->formats)[i];

      if(type[0] == 'a') {IBISFORMAT(formats[i], 'a', (ibis->colLens)[i] + 1);}
      else if(!strcmp(type, "comp")) {IBISFORMAT(formats[i], 'c', 21);}
      else {IBISFORMAT(formats[i], type[0], 13);}
   }
}

/*****************************************************************/
void IBISHELPER_printIBISPrep(IBISPrep *ibis)
{
   struct node **formatArray;
   struct node **colLenArray;
   int i;

   formatArray = (struct node**)malloc(sizeof(struct node*)*(ibis->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*(ibis->colLens)->size);
   LinkedList_setNodeArray(ibis->formats, formatArray);
   LinkedList_setNodeArray(ibis->colLens, colLenArray);
   printf("unit: %d\n", ibis->unit);
   printf("nr: %d\n", ibis->nr);
   printf("nc: %d\n", ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      printf("column %d: format: %s column length: %d\n", i, (char*)(formatArray[i]->data), *((int*)(colLenArray[i]->data)));
   free(formatArray);
   free(colLenArray);
}

/*****************************************************************/
IBISPrep* IBISHELPER_openIBIS_out2(char *name, int inst, int nr)
{
   int status;
   IBISPrep *ibis2;

   ibis2 = (IBISPrep*)malloc(sizeof(IBISPrep));
   ibis2->nc = 0;
   ibis2->nr = nr;
   strcpy(ibis2->mode, "write");

   if(!strcmp(name, "out"))
      status = zvunit(&(ibis2->unit), "out", inst, NULL);
   else
      status = zvunit(&(ibis2->unit), "outibis", inst, "u_name", name, NULL);
   if(status != 1) zmabend("Error while acquiring output ibis file.\n");

   ibis2->formats = LinkedList_getLinkedList();
   ibis2->colLens = LinkedList_getLinkedList();

   return ibis2;
}

/*****************************************************************/
void IBISHELPER_addColumn(IBISPrep *ibis2, char *format)
{
   int *colLen;
   char *fmt;

   ++(ibis2->nc);
   fmt = (char*)malloc(6);
   strcpy(fmt, format);

   LinkedList_add(ibis2->formats, fmt);

   colLen = (int*)malloc(sizeof(int));
   *colLen = IBISHELPER_getLenByFormat(format);
   LinkedList_add(ibis2->colLens, colLen);
}

/*****************************************************************/
void IBISHELPER_freeIBISPrep(IBISPrep **ibis2)
{
   struct node **formatArray;
   struct node **colLenArray;
   int i;

   formatArray = (struct node**)malloc(sizeof(struct node*)*((*ibis2)->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*((*ibis2)->colLens)->size);
   LinkedList_setNodeArray((*ibis2)->formats, formatArray);
   LinkedList_setNodeArray((*ibis2)->colLens, colLenArray);
   for(i = 0; i < (*ibis2)->nc; i++)
   {
      free(formatArray[i]->data);
      free(colLenArray[i]->data);
   }
   LinkedList_free(&((*ibis2)->formats));
   LinkedList_free(&((*ibis2)->colLens));

   free(formatArray);
   free(colLenArray);
   free(*ibis2);
}

/*****************************************************************/
IBISStruct* IBISHELPER_getIBISStruct(IBISPrep **prep)
{
   int i;
   struct node **formatArray;
   struct node **colLenArray;
   IBISStruct *ibis;

   ibis = (IBISStruct*)malloc(sizeof(IBISStruct));
   ibis->unit = (*prep)->unit;
   ibis->nr = (*prep)->nr;
   ibis->nc = (*prep)->nc;
   strcpy(ibis->mode, (*prep)->mode);
   //   strcpy(ibis->org, (*prep)->org);

   formatArray = (struct node**)malloc(sizeof(struct node*)*((*prep)->formats)->size);
   colLenArray = (struct node**)malloc(sizeof(struct node*)*((*prep)->colLens)->size);
   LinkedList_setNodeArray((*prep)->formats, formatArray);
   LinkedList_setNodeArray((*prep)->colLens, colLenArray);
   for(i = 0; i < ibis->nc; i++)
   {
      strncpy(ibis->formats[i], (char*)(formatArray[i]->data), 6);
      ibis->colLens[i] = *((int*)(colLenArray[i]->data));
   }

   IBISHELPER_setColumnWidths(ibis);
   IBISHELPER_setTotRecordSize(ibis);
   ibis->totDataSize = ibis->totRecSize*ibis->nr;

   ibis->data = (char**)malloc(sizeof(char*)*ibis->nc);
   for(i = 0; i < ibis->nc; i++)
      ibis->data[i] = (char*)calloc(ibis->nr, (ibis->colLens)[i]);

   IBISHELPER_freeIBISPrep(prep);

   free(formatArray);
   free(colLenArray);
   return ibis;
}


/*****************************************************************
char* IBISHELPER_getDataPtr(IBISPrep *ibis2, int row, int col)
{
   int i;
   int colLen;
   char *buf;

   assert(row < ibis2->nr && col < ibis2->nc);
   buf = LinkedList_get(ibis2->data, col);
   colLen = *((int*)(LinkedList_get(ibis2->colLens, col)));

   return (buf + row*colLen);
}
*/

/*****************************************************************
int IBISHELPER_getColLen(IBISPrep *ibis2, int col)
{
   int colLen = *((int*)LinkedList_get(ibis2->colLens, col));

   return colLen;
}
*/

/*****************************************************************
void IBISHELPER_setString(IBISPrep *ibis2, int row, int col, char *str)
{
   int colLen;
   char *ptr, head;

   head = ((char*)(LinkedList_get(ibis2->formats, col)))[0];
   if(head != 'a' && head != 'A')
      zmabend("Attempting to write string into non-string column.\n");
   ptr = IBISHELPER_getDataPtr(ibis2, row, col);
   colLen = IBISHELPER_getColLen(ibis2, col);

   strncpy(ptr, str, colLen);
}
*/

/*****************************************************************
void IBISHELPER_setDouble(IBISPrep *ibis2, int row, int col, double data)
{
   char *ptr, *format;

   format = (char*)LinkedList_get(ibis2->formats, col);
   if(format[0] == 'a')
      zmabend("Attempting to write numeric into string column.\n");

   //   printf("1) data: %lf\n", data);
   ptr = IBISHELPER_getDataPtr(ibis2, row, col);
   //   printf("2) data: %lf %s\n", data, format);
   switch(tolower(format[0]))
   {
      case 'c': printf("Complex not supported for now.\n");
                zabend();
                break;
      case 'b': ((char*)ptr)[0]      = (char)data;
                break;
      case 'h': ((short int*)ptr)[0] = (short int)data;
                break;
      case 'f': ((int*)ptr)[0]       = (int)data;
                break;
      case 'r': ((float*)ptr)[0]     = (float)data;
                break;
      case 'd': ((double*)ptr)[0]    = (double)data;
                break;
   }
}
*/

/*****************************************************************
void IBISHELPER_writeIBIS(IBISPrep *ibis2)
{
   char *formats;
   int i, status;

   formats = (char*)malloc(sizeof(char)*ibis2->nc*6);
   for(i = 0; i < ibis2->nc; i++)
   {
      strcpy(formats+(i*6), (char*)(LinkedList_get(ibis2->formats, i)));
   }

   if(!strcmp(ibis2->mode, "write"))
   {
      status = IBISFileOpen(ibis2->unit, &(ibis2->handle), ibis2->mode, ibis2->nc, ibis2->nr, (char*)formats, NULL);
      assert(status == 1);
   }

   for(i = 0; i < ibis2->nc; i++)
   {
      char *ptr;

      ptr = IBISHELPER_getDataPtr(ibis2, 0, i);

      status = IBISColumnWrite(ibis2->handle, ptr, i+1, 1, ibis2->nr);
      assert(status == 1);
   }

   //   for(i = 0; i < ibis2->nc; i++) free(formats[i]);
   free(formats);
}
*/

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLinkedList.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "cartoLinkedList.h"

/*********************************************/
LINKEDLIST* LinkedList_getLinkedList()
{
   LINKEDLIST *list;
   list = (LINKEDLIST*)malloc(sizeof(LINKEDLIST));
   list->size = 0;

   return list;
}

/*********************************************/
void LinkedList_addNode(LINKEDLIST *list, struct node *n)
{
   if(!(list->size))
   {
      n->next = n;
      n->prev = n;
      list->head = n;
      list->tail = n;
      ++(list->size);

      return;
   }

   n->next = NULL;
   n->prev = list->tail;
   list->tail->next = n;
   list->tail = n;
   n->next = NULL;

   ++(list->size);
}

/*********************************************/
void LinkedList_setNodeArray(LINKEDLIST *list, struct node** array)
{
   int i;

   //   printf("setNodeArray1 %d\n", list->size);
   array[0] = list->head;
   //   printf("setNodeArray2\n");
   for(i = 1; i < list->size; i++)
   {
      array[i] = array[i-1]->next;
      //      printf("setNodeArray3 %d %x %x\n", i, array[i-1], array[i]);
   }
}

/*********************************************/
struct node* LinkedList_addCommon(LINKEDLIST *list, void *data)
{
   struct node *n = (struct node*)malloc(sizeof(struct node));
   n->data = data;

   LinkedList_addNode(list, n);

   return n;
}

/*********************************************/
void LinkedList_addWithRank(LINKEDLIST *list, void *data, int rank)
{
   struct node *n = LinkedList_addCommon(list, data);
   n->rank = rank;
}

/*********************************************/
void LinkedList_add(LINKEDLIST *list, void *data)
{
   struct node *n = LinkedList_addCommon(list, data);
   n->rank = -1;
}

/*********************************************/
void LinkedList_removeNode(LINKEDLIST *list, struct node *n)
{
   //   printf("list: %x %d\n", list, list->size);
   if(list->size == 1)
   {
      list->head = NULL;
      list->tail = NULL;
   }
   else if(list->head == n)
   {
      //      printf("inside removedNode 1\n");
      list->head = list->head->next;
      //      printf("inside removedNode 2\n");
      list->head->prev = list->tail;
      //      printf("inside removedNode 3\n");
      list->tail->next = NULL;
   }
   else if(list->tail == n)
   {
      list->tail = list->tail->prev;
      list->tail->next = NULL;
      list->head->prev = list->tail;
   }
   else
   {
     //      printf("1 inside removeNode: n->prev->next: %x n->next->prev: %x\n", n->prev->next, n->next->prev);
      n->prev->next = n->next;
      n->next->prev = n->prev;
      //      printf("2 inside removeNode: n->prev->next: %x n->next->prev: %x\n", n->prev->next, n->next->prev);
   }

   n->next = NULL;
   n->prev = NULL;
   --(list->size);
}

/*********************************************/
void LinkedList_addLinkedLists(LINKEDLIST *list1, LINKEDLIST *list2)
{
   if(!(list1->size) && list2->size)
   {
      list1->head = list2->head;
      list1->tail = list2->tail;
   }
   else
   {
      list1->tail->next = list2->head;
      if(list2->size)
      {
         list2->head->prev = list1->tail;
         list1->tail = list2->tail;
      }
   }
   list1->size += list2->size;
   list2->head = NULL;
   list2->tail = NULL;
   list2->size = 0;
}

/*********************************************/
void LinkedList_free(LINKEDLIST **list)
{
   struct node *n, *next;

   if((*list)->size)
   {
      n = (*list)->head;
      next = n->next;
      while(next != NULL)
      {
         free(n);
         n = next;
         next = next->next;
      }
      free(n);
   }

   free(*list);
}

/*********************************************/
struct node* LinkedList_getMinNode(LINKEDLIST *list)
{
   int i;
   struct node *minNode;
   struct node *n;

   assert(list->size);

   n = list->head;
   minNode = n;
   assert(minNode->rank > -1);
   for(i = 1; i < list->size; i++)
   {
      n = n->next;
      if(n->rank < minNode->rank)
      {
         minNode = n;
         assert(minNode->rank > -1);
      }
   }

   return minNode;
}

/*********************************************/
LINKEDLIST* LinkedList_sortAscending(LINKEDLIST **list)
{
   int i, size;
   LINKEDLIST *sortedList;

   size = (*list)->size;
   sortedList = LinkedList_getLinkedList();
   for(i = 0; i < size; i++)
   {
      struct node *n = LinkedList_getMinNode(*list);
      LinkedList_removeNode(*list, n);
      LinkedList_addNode(sortedList, n);
   }

   LinkedList_free(list);
   return sortedList;
}

/*********************************************/
LINKEDLIST* LinkedList_bigMemSortAscending(LINKEDLIST **list)
{
   int i, listSize;
   int minRank, maxRank, nRank;
   LINKEDLIST **lists;
   LINKEDLIST *sortedList;
   struct node *n;

   n = (*list)->head;
   minRank = n->rank;
   maxRank = n->rank;
   for(i = 1; i < (*list)->size; i++)
   {
      int rank;

      n = n->next;
      rank = n->rank;

      if(minRank > rank) minRank = rank;
      if(maxRank < rank) maxRank = rank;
   }
   nRank = maxRank - minRank + 1;

   lists = (LINKEDLIST**)malloc(sizeof(LINKEDLIST*)*nRank);
   for(i = 0; i < nRank; i++) lists[i] = LinkedList_getLinkedList();

   listSize = (*list)->size;
   for(i = 0; i < listSize; i++)
   {
      struct node *n = (*list)->head;
      LinkedList_removeNode((*list), n);

      LinkedList_addNode(lists[n->rank - minRank], n);
   }

   sortedList = LinkedList_getLinkedList();
   for(i = 0; i < nRank; i++)
   {
      LinkedList_addLinkedLists(sortedList, lists[i]);
      LinkedList_free(&(lists[i]));
   }

   LinkedList_free(list);
   sortedList->tail->next = NULL;
   return sortedList;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <zvproto.h>
#include <defines.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>

#include "ImageUtils.h"

/******************************************************************************/
void readError(char *fname, int line, int status)
{
   printf("Problem reading line %d in file %s -- error status: %d.\n",
             line, fname, status);
   assert(0);
}

/******************************************************************************/
VICAR_IMAGE* getImage(int unit)
{
   int status;
   VICAR_IMAGE* vi;

   vi = (VICAR_IMAGE*)malloc(sizeof(VICAR_IMAGE));
   assert(unit > -1);
   vi->unit = unit;

   status = zvget(unit, "NAME", (vi->fname), "NL", &(vi->nl), "NS", &(vi->ns),
                  "PIX_SIZE", &(vi->pixsize), "FORMAT", &(vi->format), NULL);
   assert(status == 1);

   vi->buffer = (double*)malloc(sizeof(double)*(vi->ns));
   vi->curr_line_in_buff = -1;

   vi->valid = NULL;
   vi->valid_cnt = -1;

   return vi;
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp(int inst)
{
   int status, unit;

   status = zvunit(&unit, "inp", inst, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_fname(char *fname, int inst)
{
   int status, unit;

   status = zvunit(&unit, "VI_INP", inst, "U_NAME", fname, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_inp_by_parmName(char *parmName, int inst)
{
   int status, dumdef, cnt;
   char fname[IU_MAX_PARM][IU_MAX_FNAME_LEN];

   status = zvparm(parmName, fname, &cnt, &dumdef, IU_MAX_PARM, IU_MAX_FNAME_LEN);
   assert(status == 1);

   if(cnt < inst) return NULL;

   return getVI_inp_by_fname(fname[inst-1], inst);
}

/******************************************************************************/
VICAR_IMAGE* getVI_out(char *format, int inst, int nl, int ns)
{
   int status, unit;

   status = zvunit(&unit, "out", inst, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "WRITE", "O_FORMAT", format, "U_FORMAT", "DOUB",
                   "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
void getValidMask(VICAR_IMAGE **vi)
{
   int i, j;

   if((*vi)->valid != NULL) return;

   (*vi)->valid_cnt = 0;
   (*vi)->valid = (unsigned char**)malloc(sizeof(unsigned char*)*(*vi)->nl);
   for(i = 0; i < (*vi)->nl; i++)
   {
      readVicarImageLine(*vi, i);
      (*vi)->valid[i] = (unsigned char*)malloc(sizeof(unsigned char)*(*vi)->ns);
      for(j = 0; j < (*vi)->ns; j++)
      {
         if(fabs((*vi)->buffer[j]) > 10E-10)
         {
            (*vi)->valid[i][j] = 1;
            (*vi)->valid_cnt++;
         }
         else (*vi)->valid[i][j] = 0;
      }
   }
}

/******************************************************************************/
VICAR_IMAGE* getVI_out_by_fname(char *fname, char *type, char *format, int inst, int nl, int ns)
{
   int status, unit;

   status = zvunit(&unit, type, inst, "U_NAME", fname, NULL);
   assert(status == 1);
   status = zvopen(unit, "OP", "WRITE", "O_FORMAT", format, "U_FORMAT", "DOUB",
                   "U_NL", nl, "U_NS", ns, NULL);
   assert(status == 1);

   return getImage(unit);
}

/******************************************************************************/
VICAR_IMAGE* getVI_out_by_parmName(char *parmName, char *format, int inst, int nl, int ns)
{
   int status, dumdef, cnt;
   char fname[IU_MAX_PARM][IU_MAX_FNAME_LEN];

   status = zvparm(parmName, fname, &cnt, &dumdef, IU_MAX_PARM, IU_MAX_FNAME_LEN);
   assert(status == 1);

   if(cnt < inst) return NULL;

   return getVI_out_by_fname(fname[inst-1], parmName, format, inst, nl, ns);
}

/******************************************************************************/
void deleteImage(VICAR_IMAGE **vi)
{
   int i;

   if(*vi != NULL)
   {
      // free image line buffer
      if((*vi)->buffer != NULL) free((*vi)->buffer);
      (*vi)->buffer = NULL;

      // free image valid buffer
      if((*vi)->valid != NULL)
      {
        for(i = 0; i < (*vi)->nl; i++) free((*vi)->valid[i]);
        free((*vi)->valid);
        (*vi)->valid = NULL;
      }

      free(*vi);
      *vi = NULL;
   }
}

/******************************************************************************/
VICAR_TILE_IMAGE* getVTI(VICAR_IMAGE *vi, int tile_nl, int tile_ns)
{
   int i;
   VICAR_TILE_IMAGE *vti;

   // check to see that requested tile size is smaller than image size
   assert(tile_nl <= vi->nl && tile_ns <= vi->ns);

   vti = (VICAR_TILE_IMAGE*)malloc(sizeof(VICAR_TILE_IMAGE));
   vti->vi = vi;
   vti->tile_nl = tile_nl;
   vti->tile_ns = tile_ns;
   vti->buffer_size = vti->vi->ns + tile_ns;
   vti->startPos = (tile_ns-1)/2;

   vti->buffer = (double**)malloc(sizeof(double*)*tile_nl);
   vti->tile = (double**)malloc(sizeof(double*)*tile_nl);
   for(i = 0; i < tile_nl; i++)
      vti->buffer[i] = (double*)calloc(vti->buffer_size, sizeof(double));


   vti->last_line_requested = -1;

   return vti;
}

/******************************************************************************/
void readAllVTIBuffers(VICAR_TILE_IMAGE *vti, int line)
{
   int i, status, startline;

   startline = line - (vti->tile_nl-1)/2;

   for(i = 0; i < vti->tile_nl; i++)
   {
      if(startline+i < 0 || startline+i >= vti->vi->nl)
      {
         memset(vti->buffer[i], 0, sizeof(double)*vti->buffer_size);
         continue;
      }

      //      printf("reading all %d -- reading line: %d\n", line, startline+i+1);
      status = zvread(vti->vi->unit, vti->buffer[i]+vti->startPos, "LINE", startline+i+1, NULL);
      assert(status == 1);
   }
}

/******************************************************************************/
void rollVTIBuffers(VICAR_TILE_IMAGE **vti, int line)
{
   int i, status, lineToRead;
   double *tmpbuf;

   tmpbuf = (*vti)->buffer[0];
   for(i = 0; i < (*vti)->tile_nl-1; i++)
      (*vti)->buffer[i] = (*vti)->buffer[i+1];
   (*vti)->buffer[i] = tmpbuf;

   lineToRead = line + (*vti)->tile_nl/2 + 1;

   //   printf("rolling buff %d -- reading line: %d\n", line, lineToRead);

   if(lineToRead > (*vti)->vi->nl)
      memset((*vti)->buffer[i], 0, sizeof(double)*(*vti)->buffer_size);
   else
   {
      status = zvread((*vti)->vi->unit, (*vti)->buffer[i]+(*vti)->startPos, "LINE", lineToRead, NULL);
      assert(status == 1);
   }
}

/******************************************************************************/
void readVicarTileImage(VICAR_TILE_IMAGE *vti, int line, int samp)
{
   int i, startsamp;

   // check to see if current lines are in the buffer
   if(vti->last_line_requested == -1 || abs(vti->last_line_requested - line) > 1)
      readAllVTIBuffers(vti, line);
   else if(abs(vti->last_line_requested - line) == 1)
      rollVTIBuffers(&vti, line);
   vti->last_line_requested = line;

   startsamp = vti->startPos + samp-(vti->tile_ns-1)/2;
   for(i = 0; i < vti->tile_nl; i++)
      vti->tile[i] = vti->buffer[i]+startsamp;
}

/******************************************************************************/
void deleteVTI(VICAR_TILE_IMAGE **vti)
{
   int i;

   for(i = 0; i < (*vti)->tile_nl; i++)
      free((*vti)->buffer[i]);
   free((*vti)->buffer);
   free((*vti)->tile);

   free(*vti);
}

/******************************************************************************/
VICAR_RESAMPLE_IMAGE* getVRI(VICAR_IMAGE *from, VICAR_IMAGE *to, int resample_mode)
{
   int i;
   VICAR_RESAMPLE_IMAGE *vri;

   if(to == NULL) return NULL;
   vri = (VICAR_RESAMPLE_IMAGE*)malloc(sizeof(VICAR_RESAMPLE_IMAGE));

   vri->from = from;
   vri->to = to;

   vri->buffer = (double**)malloc(sizeof(double*)*4);
   for(i = 0; i < 4; i++)
   {
      vri->buffer[i] = (double*)calloc(from->ns, sizeof(double));
      vri->curr_lines_in_buff[i] = -1;
   }

   vri->resample_mode = resample_mode;

   return vri;
}

/******************************************************************************/
void deleteVRI(VICAR_RESAMPLE_IMAGE **vri)
{
   int i;

   for(i = 0; i < 4; i++) free((*vri)->buffer[i]);
   free((*vri)->buffer);

   free(*vri);
}

/******************************************************************************/
void getSkippedLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int to_line)
{
   int i, sskip, lskip, line;

   sskip = vri->from->ns/vri->to->ns;
   lskip = vri->from->nl/vri->to->nl;
   line = to_line*lskip;
   readVicarResampleImageLine(vri, 0, line);
   /*
   if(line != vri->curr_lines_in_buff[0])
   {
      status = zvread(vri->from->unit, vri->buffer[0], "LINE", line+1, NULL);
      if(status != 1) zabend();
      vri->curr_lines_in_buff[0] = line;
   }
   */

   for(i = 0; i < vri->to->ns; i++)
      buf[i] = vri->buffer[0][i*sskip];
}

/******************************************************************************/
/* Helper function called by prepareBuffer                                    */
/******************************************************************************/
void swapVRIBuffer(VICAR_RESAMPLE_IMAGE *vri, int ind1, int ind2)
{
   double *tmp;
   int tmpindex;

   assert(ind1 >= 0 && ind1 < 4 && ind2 >= 0 && ind2 < 4);

   tmp = vri->buffer[ind1];
   tmpindex = vri->curr_lines_in_buff[ind1];

   vri->buffer[ind1] = vri->buffer[ind2];
   vri->curr_lines_in_buff[ind1] = vri->curr_lines_in_buff[ind2];

   vri->buffer[ind2] = tmp;
   vri->curr_lines_in_buff[ind2] = tmpindex;
}

/******************************************************************************/
void prepareBilinInterpBuffer(VICAR_RESAMPLE_IMAGE *vri, double centerline)
{
   int lines[2];

   lines[0] = (int)centerline;
   lines[1] = (int)(centerline + 1);

   assert(lines[0] < vri->from->nl);
   if(lines[1] >= vri->from->nl) lines[1] = vri->from->nl - 1;

   // if already in buffer then just return
   if(vri->curr_lines_in_buff[0] == lines[0] && vri->curr_lines_in_buff[1] == lines[1])
      return;

   // swap condition: none of the buffers are in correct order and
   // there are buffers that can be swapped.
   if(lines[0] != vri->curr_lines_in_buff[0] &&
      lines[1] != vri->curr_lines_in_buff[1] &&
      (lines[0] == vri->curr_lines_in_buff[1] ||
      lines[1] == vri->curr_lines_in_buff[0])) swapVRIBuffer(vri, 1, 0);

   // if after swap, it's still not in buffer then read
   if(lines[0] != vri->curr_lines_in_buff[0])
      readVicarResampleImageLine(vri, 0, lines[0]);
   if(lines[1] != vri->curr_lines_in_buff[1])
      readVicarResampleImageLine(vri, 1, lines[1]);
}

/******************************************************************************/
double getBilinInterpSamp(VICAR_RESAMPLE_IMAGE *vri, int samp, double y, int y1, int y2)
{
   double x, f1, f2, f3, f4;
   double term1, term2, term3, term4;
   int x1, x2;

   assert(samp >= 0 && samp < vri->to->ns);

   /*pk change*/
   //   x = vri->from->ns/(double)vri->to->ns*(samp+0.5);
   x = vri->from->ns/(double)vri->to->ns*samp;
   x1 = (int)x;
   x2 = (int)(x+1);
   if(x1 <= 0) x1 = 0;
   if(x2 <= 0) x2 = 0;
   if(x1 >= vri->from->ns) x1 = vri->from->ns - 1;
   if(x2 >= vri->from->ns) x2 = vri->from->ns - 1;

   f1 = vri->buffer[0][x1];
   f2 = vri->buffer[0][x2];
   f3 = vri->buffer[1][x1];
   f4 = vri->buffer[1][x2];

   if(((x2-x1)*(y2-y1))*(x2-x)*(y2-y) == 0) term1 = 0.;
   else term1 = f1/((x2-x1)*(y2-y1))*(x2-x)*(y2-y);

   if(((x2-x1)*(y2-y1))*(x-x1)*(y2-y) == 0) term2 = 0.;
   else term2 = f2/((x2-x1)*(y2-y1))*(x-x1)*(y2-y);

   if(((x2-x1)*(y2-y1))*(x2-x)*(y-y1) == 0) term3 = 0.;
   else term3 = f3/((x2-x1)*(y2-y1))*(x2-x)*(y-y1);

   if(((x2-x1)*(y2-y1))*(x-x1)*(y-y1) == 0) term4 = 0.;
   else term4 = f4/((x2-x1)*(y2-y1))*(x-x1)*(y-y1);

   return term1 + term2 + term3 + term4;
}

/******************************************************************************/
void getBilinInterpLine(VICAR_RESAMPLE_IMAGE *vri, double *buf, int line)
{
   int i;
   double centerline;

   // initialize buffers
   /*pk change*/
   //   centerline = vri->from->nl/(double)vri->to->nl*(line+0.5);
   centerline = vri->from->nl/(double)vri->to->nl*line;
   prepareBilinInterpBuffer(vri, centerline);

   for(i = 0; i < vri->to->ns; i++)
   {
      vri->to->buffer[i] = getBilinInterpSamp(vri, i, centerline, (int)centerline, (int)(centerline+1));

      // debugging
      /*
      if(vri->to->buffer[i] < 0. || vri->to->buffer[i] > 1)
         printf("!!!error here: %d %d\n", line, i);
      */
   }
}

/******************************************************************************/
void getDownSampledLine(VICAR_RESAMPLE_IMAGE *vri, double *ds_buf, int line)
{
  if(vri->resample_mode == IU_SKIP_LINES) {
    getSkippedLine(vri, ds_buf, line);
    return;
  }
  else if(vri->resample_mode == IU_BILINEAR_INTERP) {
    getBilinInterpLine(vri, ds_buf, line);
    return;
  }
  else
    {
      printf("\n!!!RESAMPLING FROM %s TO %s: INVALID RESAMPLE MODE SPECIFIED!!!\n",
             vri->from->fname, vri->to->fname);
      zabend();
    }
}

/******************************************************************************/
void writeVicarImageLine(VICAR_IMAGE *vi, int line)
{
   int status;

   status = zvwrit(vi->unit, vi->buffer, "LINE", line+1, NULL);
   if(status != 1)
   {
      printf("Problem reading writing %d in file %s.\n", line, vi->fname);
      zabend();
   }
}

/******************************************************************************/
void readVicarImageLine(VICAR_IMAGE *vi, int line)
{
   int status;

   if(vi->curr_line_in_buff != line)
   {
      status = zvread(vi->unit, vi->buffer, "LINE", line+1, NULL);
      if(status != 1) readError(vi->fname, line+1, status);
   }

   vi->curr_line_in_buff = line;
}

/******************************************************************************/
void readVicarResampleImageLine(VICAR_RESAMPLE_IMAGE *vri, int buffer_index, int line)
{
   int status;

   if(vri->curr_lines_in_buff[buffer_index] != line)
   {
      status = zvread(vri->from->unit, vri->buffer[buffer_index], "LINE", line+1, NULL);
      if(status != 1) readError(vri->from->fname, line+1, status);
   }

   vri->curr_lines_in_buff[buffer_index] = line;
}

/******************************************************************************/
void createDownSampledImage(VICAR_RESAMPLE_IMAGE *vri)
{
   int i;

   for(i = 0; i < vri->to->nl; i++)
   {
      getDownSampledLine(vri, vri->to->buffer, i);
      writeVicarImageLine(vri->to, i);
   }
}

/******************************************************************************/
void deleteAndCloseImage(VICAR_IMAGE **vi)
{
   int status;

   status = zvclose((*vi)->unit, NULL);
   assert(status == 1);

   deleteImage(vi);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cloud_masks.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <stdlib.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

#include "cloud_masks.h"

/******************************************************************************/
CLOUD_MASKS* get_CLOUD_MASKS(int nl, int ns)
{
   int i;
   CLOUD_MASKS* masks;

   masks = (CLOUD_MASKS*)malloc(sizeof(CLOUD_MASKS));

   masks->vars     = (ALGORITHM_VARS*)malloc(sizeof(ALGORITHM_VARS));
   masks->vars->b3_noncloud = 0;
   masks->vars->b6_noncloud = 0;
   masks->vars->snowCnt = 0;
   masks->vars->ndsi_noncloud = 0;
   masks->vars->pass2_revisit = 0;
   masks->vars->b5_noncloud = 0;
   masks->vars->b42ratio_tally = 0;
   masks->vars->b45ratio_tally = 0;
   masks->vars->warmcloud = 0;
   masks->vars->coldcloud = 0;
   masks->vars->scenePixels = 0;
   masks->vars->enterdesert = 0;
   masks->vars->exitdesert = 0;
   masks->vars->snowpresent = 0;

   masks->vars->nl = nl;
   masks->vars->ns = ns;

   masks->CMcloud       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMsnow        = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMdesert      = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMcloud_warm  = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->CMcloud_cold  = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->ice           = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter_cirrus = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->ambig         = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->valid         = (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   masks->filter1       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter2       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter3       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter4       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter5       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter6       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter7       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter8       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter9       = (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->filter10      = (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   masks->tambig_warm_mask =  (unsigned char**)malloc(sizeof(unsigned char*)*nl);
   masks->tambig_cold_mask =  (unsigned char**)malloc(sizeof(unsigned char*)*nl);

   for(i = 0; i < nl; i++)
   {
      masks->CMcloud[i]       = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMsnow[i]        = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMdesert[i]      = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMcloud_warm[i]  = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->CMcloud_cold[i]  = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->ice[i]           = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->filter_cirrus[i] = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->ambig[i]         = (unsigned char*)calloc(sizeof(unsigned char), ns);
      masks->valid[i]         = (unsigned char*)calloc(sizeof(unsigned char), ns);

      masks->filter1[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter2[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter3[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter4[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter5[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter6[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter7[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter8[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter9[i]       = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->filter10[i]      = (unsigned char*)malloc(sizeof(unsigned char)*ns);

      masks->tambig_warm_mask[i] = (unsigned char*)malloc(sizeof(unsigned char)*ns);
      masks->tambig_cold_mask[i] = (unsigned char*)malloc(sizeof(unsigned char)*ns);
   }

   return masks;
}

/******************************************************************************/
void delete_CLOUD_MASKS(CLOUD_MASKS **masks)
{
   int i;

   for(i = 0; i < (*masks)->vars->nl; i++)
   {
      free((*masks)->CMcloud[i]);
      free((*masks)->CMsnow[i]);
      free((*masks)->CMdesert[i]);
      free((*masks)->CMcloud_warm[i]);
      free((*masks)->CMcloud_cold[i]);
      free((*masks)->ice[i]);
      free((*masks)->filter_cirrus[i]);
      free((*masks)->ambig[i]);
      free((*masks)->valid[i]);

      free((*masks)->filter1[i]);
      free((*masks)->filter2[i]);
      free((*masks)->filter3[i]);
      free((*masks)->filter4[i]);
      free((*masks)->filter5[i]);
      free((*masks)->filter6[i]);
      free((*masks)->filter7[i]);
      free((*masks)->filter8[i]);
      free((*masks)->filter9[i]);
      free((*masks)->filter10[i]);

      free((*masks)->tambig_warm_mask[i]);
      free((*masks)->tambig_cold_mask[i]);
   }

   free((*masks)->CMcloud);
   free((*masks)->CMsnow);
   free((*masks)->CMdesert);
   free((*masks)->CMcloud_warm);
   free((*masks)->CMcloud_cold);
   free((*masks)->ice);
   free((*masks)->filter_cirrus);
   free((*masks)->ambig);
   free((*masks)->valid);

   free((*masks)->filter1);
   free((*masks)->filter2);
   free((*masks)->filter3);
   free((*masks)->filter4);
   free((*masks)->filter5);
   free((*masks)->filter6);
   free((*masks)->filter7);
   free((*masks)->filter8);
   free((*masks)->filter9);
   free((*masks)->filter10);

   free((*masks)->tambig_warm_mask);
   free((*masks)->tambig_cold_mask);

   free((*masks)->band_files);

   free(*masks);
}

/******************************************************************************/
void setBandImages(CLOUD_MASKS **cm, VICAR_IMAGE *green, VICAR_IMAGE *red, VICAR_IMAGE *nir,
                   VICAR_IMAGE *swir1, VICAR_IMAGE *tir1)
{
   (*cm)->band_files = (BAND_FILES*)malloc(sizeof(BAND_FILES));

   (*cm)->band_files->green_band = green;
   (*cm)->band_files->red_band   = red;
   (*cm)->band_files->nir_band   = nir;
   (*cm)->band_files->swir1_band  = swir1;
   (*cm)->band_files->tir_band1  = tir1;
}

/******************************************************************************/
void setWorkspaceImages(CLOUD_MASKS **cm, VICAR_IMAGE *ndsi, VICAR_IMAGE *bTempComp,
                        VICAR_IMAGE *gv, VICAR_IMAGE *sv, VICAR_IMAGE *rs)
{
   (*cm)->ws_files = (WORKSPACE_FILES*)malloc(sizeof(WORKSPACE_FILES));

   (*cm)->ws_files->ndsi_file      = ndsi;
   (*cm)->ws_files->bTempComp_file = bTempComp;
   (*cm)->ws_files->gv_file        = gv;
   (*cm)->ws_files->sv_file        = sv;
   (*cm)->ws_files->rs_file        = rs;
}


/******************************************************************************/
void init_CM_WORKSPACE(CLOUD_MASKS **cm)
{
   int ns;

   ns = (*cm)->vars->ns;

   (*cm)->ws = (CM_WORKSPACE*)malloc(sizeof(CM_WORKSPACE));

   if((*cm)->ws_files->ndsi_file == NULL)
      (*cm)->ws->ndsi = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->ndsi = (*cm)->ws_files->ndsi_file->buffer;

   if((*cm)->ws_files->bTempComp_file == NULL)
      (*cm)->ws->bTempComp = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->bTempComp = (*cm)->ws_files->bTempComp_file->buffer;

   if((*cm)->ws_files->gv_file == NULL)
      (*cm)->ws->gv = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->gv = (*cm)->ws_files->gv_file->buffer;

   if((*cm)->ws_files->sv_file == NULL)
      (*cm)->ws->sv = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->sv = (*cm)->ws_files->sv_file->buffer;

   if((*cm)->ws_files->rs_file == NULL)
      (*cm)->ws->rs = (double*)malloc(sizeof(double)*ns);
   else
      (*cm)->ws->rs = (*cm)->ws_files->rs_file->buffer;
}

/******************************************************************************/
void delete_CM_WORKSPACE(CLOUD_MASKS **cm)
{
   if((*cm)->ws_files->ndsi_file == NULL) free((*cm)->ws->ndsi);
   if((*cm)->ws_files->bTempComp_file == NULL) free((*cm)->ws->bTempComp);
   if((*cm)->ws_files->gv_file == NULL) free((*cm)->ws->gv);
   if((*cm)->ws_files->sv_file == NULL) free((*cm)->ws->sv);
   if((*cm)->ws_files->rs_file == NULL) free((*cm)->ws->rs);

   free((*cm)->ws);
   free((*cm)->ws_files);
}

/******************************************************************************/
void readAllBands(CLOUD_MASKS *cm, int line)
{
  if(cm->band_files->green_band != NULL) readVicarImageLine(cm->band_files->green_band, line);
  if(cm->band_files->red_band   != NULL) readVicarImageLine(cm->band_files->red_band, line);
  if(cm->band_files->nir_band   != NULL) readVicarImageLine(cm->band_files->nir_band, line);
  if(cm->band_files->swir1_band != NULL) readVicarImageLine(cm->band_files->swir1_band, line);
  if(cm->band_files->tir_band1  != NULL) readVicarImageLine(cm->band_files->tir_band1, line);
}

/******************************************************************************/
void filter1(double *red_ref, unsigned char *finalmask, unsigned char* ambMask,
             unsigned char *valid, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
      if(valid[i] && red_ref[i] > 0.2) finalmask[i] = CM_CLOUD;
      else
      {
         finalmask[i] = CM_NONCLOUD;

         if(valid[i] && red_ref[i] > 0.07 && red_ref[i] < 0.5)
            ambMask[i] = CM_AMBIG;
      }
}

/******************************************************************************/
void filter2(double* ndsi, double *near_ref, unsigned char *finalmask,
             unsigned char *snow, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(ndsi[i] < 0.7) finalmask[i] = CM_CLOUD;
         else
         {
            finalmask[i] = CM_NONCLOUD;

            if(ndsi[i] > 0.7 && near_ref[i] > 0.25) snow[i] = CM_SNOW;
         }
      }
   }
}

/******************************************************************************/
void filter3(double *b_temp, unsigned char *finalmask, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(b_temp[i] < 300) finalmask[i] = CM_CLOUD;
         else finalmask[i] = CM_NONCLOUD;
      }
   }
}

/******************************************************************************/
void filter4(double *bTempcomp, double *shortwave_ref, double *b_temp, unsigned char *finalmask,
             unsigned char *ambig, unsigned char *ice, int ns, double thresh)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(bTempcomp[i] < thresh) finalmask[i] = CM_CLOUD;
         else if(shortwave_ref[i] > 0.2 && b_temp[i] < 300)
         {
            finalmask[i] = CM_NONCLOUD;
            ambig[i] = CM_AMBIG;
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            ice[i] = CM_ICE;
         }
      }
   }
}

/******************************************************************************/
void filter5(double *gv, unsigned char *finalmask, unsigned char *ambig, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
        if(gv[i] < 2.0)
           finalmask[i] = CM_CLOUD;
        else
        {
           finalmask[i] = CM_NONCLOUD;
           ambig[i] = CM_AMBIG;
        }
      }
   }
}

/******************************************************************************/
void filter6(double *sv, unsigned char *finalmask, unsigned char *ambig,
             long long int *enterdesert, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(sv[i] < 2.0)
         {
            finalmask[i] = CM_CLOUD;
            ++(*enterdesert);
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            ambig[i] = CM_AMBIG;
         }
      }
   }
}

/******************************************************************************/
void filter7(double *rs, unsigned char *finalmask, unsigned char *CMdesert,
             unsigned char *ambig, long long int *exitdesert, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(rs[i] > 0.8)
         {
            finalmask[i] = CM_CLOUD;
            ++(*exitdesert);
         }
         else
         {
            finalmask[i] = CM_NONCLOUD;
            CMdesert[i] = CM_DESERT;
            ambig[i] = CM_AMBIG;
         }
      }
   }
}

/******************************************************************************/
void filter8(double *bTempComp, unsigned char *finalmask, unsigned char *CMcloud_cold,
             unsigned char *CMcloud_warm, int ns, double thresh,
             long long int *coldcloud, long long int *warmcloud)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(finalmask[i] == CM_CLOUD)
      {
         if(bTempComp[i] < thresh)
         {
            CMcloud_cold[i] = CM_COLD_CLOUD;
            ++(*coldcloud);
         }
         else
         {
            CMcloud_warm[i] = CM_WARM_CLOUD;
            ++(*warmcloud);
         }
      }
   }
}

/******************************************************************************/
void getNDSI(double *green_ref, double *shortwave_ref, double *ndsi, int ns)
{    
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(green_ref[i]) > 10E-10 && fabs(shortwave_ref[i]) > 10E-10)
         ndsi[i] = (green_ref[i] - shortwave_ref[i])/(green_ref[i] + shortwave_ref[i]);
      else
         ndsi[i] = 0.;
   }
}

/******************************************************************************/
void getBTempComp(double *b_temp, double *shortwave_ref, double *bTempComp, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
      bTempComp[i] = (1 - shortwave_ref[i])*b_temp[i];
}

/******************************************************************************/
void getGV(double *near_ref, double *red_ref, double *gv, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(red_ref[i]) < 10E-10) gv[i] = 0.;
      else gv[i] = near_ref[i]/red_ref[i];
   }
}

/******************************************************************************/
void getSV(double *near_ref, double *green_ref, double *sv, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(green_ref[i]) < 10E-10) sv[i] = 0.;
      else sv[i] = near_ref[i]/green_ref[i];
   }
}

/******************************************************************************/
void getRS(double *near_ref, double *shortwave_ref, double *rs, int ns)
{
   int i;

   for(i = 0; i < ns; i++)
   {
      if(fabs(near_ref[i]) < 10E-10 || fabs(shortwave_ref[i]) < 10E-10) rs[i] = 0.;
      else rs[i] = near_ref[i]/shortwave_ref[i];
   }
}

/******************************************************************************/
void convertToAtSatTemp(double *thermal_rad, double *atSatTemp, int ns, int *scenePixels)
{
   int i;

   for(i = 0; i < ns; i++)
      atSatTemp[i] = 1282.71/log((666.09/thermal_rad[i])+1);
}

/******************************************************************************/
void CM_getValidMask(CLOUD_MASKS *cm)
{
   int i, j;
   char **tmpvalid;

   cm->vars->scenePixels = 0;
   tmpvalid = (char **)malloc(sizeof(char*)*cm->vars->nl);

   for(i = 0; i < cm->vars->nl; i++)
   {
      tmpvalid[i] = (char *)malloc(sizeof(char)*cm->vars->ns);
      readAllBands(cm, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         if(CM_GREEN_REF[j] > 10E-10 &&
            CM_RED_REF[j] > 10E-10 &&
            CM_NIR_REF[j] > 10E-10 &&
            CM_SWIR1_REF[j] > 10E-10 &&
            CM_TIR1_REF[j] > 10E-10)
         {
            tmpvalid[i][j] = CM_VALID;
         }
         else
            tmpvalid[i][j] = CM_NONVALID;
      }
   }

   for(i = 0; i < cm->vars->nl; i++)
   {
      for(j = 0; j < cm->vars->ns; j++)
      {
         if(i < 2 || i >= cm->vars->nl-2) cm->valid[i][j] = CM_NONVALID;
         else if(j < 2 || j >= cm->vars->ns-2) cm->valid[i][j] = CM_NONVALID;
         else if(tmpvalid[i-2][j-1] == CM_NONVALID ||
                 tmpvalid[i-2][j] == CM_NONVALID ||
                 tmpvalid[i-2][j+1] == CM_NONVALID ||
                 tmpvalid[i-1][j-1] == CM_NONVALID ||
                 tmpvalid[i-1][j] == CM_NONVALID ||
                 tmpvalid[i-1][j+1] == CM_NONVALID ||
                 tmpvalid[i][j-2] == CM_NONVALID ||
                 tmpvalid[i][j-1] == CM_NONVALID ||
                 tmpvalid[i][j] == CM_NONVALID ||
                 tmpvalid[i][j+1] == CM_NONVALID ||
                 tmpvalid[i][j+2] == CM_NONVALID ||
                 tmpvalid[i+1][j-1] == CM_NONVALID ||
                 tmpvalid[i+1][j] == CM_NONVALID ||
                 tmpvalid[i+1][j+1] == CM_NONVALID ||
                 tmpvalid[i+2][j-1] == CM_NONVALID ||
                 tmpvalid[i+2][j] == CM_NONVALID ||
                 tmpvalid[i+2][j+1] == CM_NONVALID)
            cm->valid[i][j] = CM_NONVALID;
         else
         {
            cm->valid[i][j] = CM_VALID;
            cm->vars->scenePixels++;
         }
      }
   }

   for(i = 0; i < cm->vars->nl; i++) free(tmpvalid[i]);
   free(tmpvalid);
}

/******************************************************************************/
void getSnowPercentage(CLOUD_MASKS *cm)
{
   int i, j;

   cm->vars->snowCnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->green_band, i);
      readVicarImageLine(cm->band_files->swir1_band, i);
      readVicarImageLine(cm->band_files->nir_band, i);

      getNDSI(CM_GREEN_REF, CM_SWIR1_REF, cm->ws->ndsi, cm->vars->ns);
      if(cm->ws_files->ndsi_file != NULL) writeVicarImageLine(cm->ws_files->ndsi_file, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         if((cm->ws->ndsi)[j] > 0.4 && CM_NIR_REF[j] > 0.25)
         {
            cm->CMsnow[i][j] = 1;
            ++(cm->vars->snowCnt);
         }
      }
   }

   printf("snow count: %lld\n", cm->vars->snowCnt);
}

/******************************************************************************/
double interp1(double *Tcloud, int len_Tcloud, double x)
{
   int x1, x2;
   double y1, y2;

   x1 = (int)x;
   x2 = (int)(x + 1);
   y1 = Tcloud[x1];
   y2 = Tcloud[x2];

   return y1 + ((x - x1)*(y2 - y1))/(double)(x2 - x1);
}

/******************************************************************************/
double interpThresh(double *xarray, double *yarray, double x)
{
   double x1, x2;
   double y1, y2;

   x1 = xarray[0];
   x2 = xarray[1];
   y1 = yarray[0];
   y2 = yarray[1];

   return y1 + ((x - x1)*(y2 - y1))/(x2-x1);

}

/******************************************************************************/
void getThresholds(CLOUD_MASKS *cm)
{
   double snowPer;
   double xarray[2] = {0., 5.};
   double yarray4[2] = {250., 220.};
   double yarray8[2] = {235., 205.};

   snowPer = cm->vars->snowCnt/(double)cm->vars->scenePixels*100;

   if(snowPer > 5.)
   {
      cm->vars->filter4Thresh = 220;
      cm->vars->filter8Thresh = 205;
   }
   else
   {
      cm->vars->filter4Thresh = interpThresh(xarray, yarray4, snowPer);
      cm->vars->filter8Thresh = interpThresh(xarray, yarray8, snowPer);
   }
}

/******************************************************************************/
void doPass1(CLOUD_MASKS *cm)
{
   int i, nl, ns;

   nl = cm->vars->nl;
   ns = cm->vars->ns;

   CM_getValidMask(cm);
   getSnowPercentage(cm);
   getThresholds(cm);
   for(i = 0; i < cm->vars->nl; i++)
   {
      readAllBands(cm, i);

      filter1(CM_RED_REF, cm->CMcloud[i], cm->ambig[i], cm->valid[i], ns);
      memcpy(cm->filter1[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getNDSI(CM_GREEN_REF, CM_SWIR1_REF, cm->ws->ndsi, ns);
      filter2(cm->ws->ndsi, CM_NIR_REF, cm->CMcloud[i], cm->CMsnow[i], ns);
      memcpy(cm->filter2[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      filter3(CM_TIR1_REF, cm->CMcloud[i], ns);
      memcpy(cm->filter3[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getBTempComp(CM_TIR1_REF, CM_SWIR1_REF, cm->ws->bTempComp, ns);
      if(cm->ws_files->bTempComp_file != NULL) writeVicarImageLine(cm->ws_files->bTempComp_file, i);
      filter4(cm->ws->bTempComp, CM_SWIR1_REF, CM_TIR1_REF, cm->CMcloud[i], cm->ambig[i], cm->ice[i], ns, cm->vars->filter4Thresh);
      memcpy(cm->filter4[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getGV(CM_NIR_REF, CM_RED_REF, cm->ws->gv, ns);
      if(cm->ws_files->gv_file != NULL) writeVicarImageLine(cm->ws_files->gv_file, i);
      filter5(cm->ws->gv, cm->CMcloud[i], cm->ambig[i], ns);
      memcpy(cm->filter5[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getSV(CM_NIR_REF, CM_GREEN_REF, cm->ws->sv, ns);
      if(cm->ws_files->sv_file != NULL) writeVicarImageLine(cm->ws_files->sv_file, i);
      filter6(cm->ws->sv, cm->CMcloud[i], cm->ambig[i], &(cm->vars->enterdesert), ns);
      memcpy(cm->filter6[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      getRS(CM_NIR_REF, CM_SWIR1_REF, cm->ws->rs, ns);
      if(cm->ws_files->rs_file != NULL) writeVicarImageLine(cm->ws_files->rs_file, i);
      filter7(cm->ws->rs, cm->CMcloud[i], cm->CMdesert[i], cm->ambig[i], &(cm->vars->exitdesert), ns);
      memcpy(cm->filter7[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);

      filter8(cm->ws->bTempComp, cm->CMcloud[i], cm->CMcloud_cold[i], cm->CMcloud_warm[i], ns, cm->vars->filter8Thresh, &(cm->vars->coldcloud), &(cm->vars->warmcloud));
      memcpy(cm->filter8[i], cm->CMcloud[i], sizeof(unsigned char)*cm->vars->ns);
   }
}

/******************************************************************************/
long long int countMaskPixels(CLOUD_MASKS *cm, unsigned char **mask)
{
   long long int cnt;
   int i, j;

   cnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
       for(j = 0; j < cm->vars->ns; j++)
          if(mask[i][j]) ++cnt;

   return cnt;
}

/***********************************************************/
/* helper function for the qsort function                  */
/***********************************************************/
int compare_doubles(const double *x1, const double *x2)
{
   int result;

   if(*x1 <= *x2) result = -1;
   else result = 1;

   return result;
}

/******************************************************************************/
double getAvg(double *buf, int len_buf)
{
   int i;
   double sum;

   sum = 0.0;
   for(i = 0; i < len_buf; i++)
      sum += buf[i];

   return sum/len_buf;
}

/******************************************************************************/
void getTcloudStats(double *Tcloud, double *sd, double *avg, int len_Tcloud)
{
   int i;

   *avg = getAvg(Tcloud, len_Tcloud);

   *sd = 0.0;
   for(i = 0; i < len_Tcloud; i++)
      *sd += pow(Tcloud[i] - *avg, 2);

   *sd /= len_Tcloud - 1;
   *sd = pow(*sd, 0.5);
}

/******************************************************************************/
double getSkew(CLOUD_MASKS* cm, double *Tcloud, double stdev, double avg, int len_Tcloud)
{
   int i;
   double *sk;

   sk = (double*)malloc(sizeof(double)*len_Tcloud);
   for(i = 0; i < len_Tcloud; i++)
   {
      sk[i] = pow((Tcloud[i] - avg)/stdev, 3);
      //      printf("Tcloud: %f sk: %f\n", Tcloud[i], sk[i]);
   }

   return getAvg(sk, len_Tcloud);
}

/******************************************************************************/
void zeroCloud(CLOUD_MASKS *cm)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         cm->CMcloud[i][j] = 0;
}

/******************************************************************************/
void getTempByMask(CLOUD_MASKS *cm, unsigned char** mask, double *temp, long long int *len)
{
   int i, j;
   long long int cnt;

   cnt = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->tir_band1, i);

      for(j = 0; j < cm->vars->ns; j++)
         if(mask[i][j]) temp[cnt++] = CM_TIR1_REF[j];
   }

   *len = cnt;
}

/******************************************************************************/
void setCMcloudMask(CLOUD_MASKS *cm, unsigned char **mask)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         cm->CMcloud[i][j] = mask[i][j];
}

/******************************************************************************/
void getTambigWarmColdMask(CLOUD_MASKS *cm, unsigned char **warm, long long int *warm_len,
                           unsigned char **cold, long long int *cold_len, double pmax, double pmin)
{
   int i, j;

   *warm_len = *cold_len = 0;
   for(i = 0; i < cm->vars->nl; i++)
   {
      readVicarImageLine(cm->band_files->tir_band1, i);

      for(j = 0; j < cm->vars->ns; j++)
      {
         warm[i][j] = cold[i][j] = 0;
         if(!cm->ambig[i][j]) continue;
         //         printf("tir ref: %lf pmin: %lf pmax: %lf\n", CM_TIR1_REF[j], pmin, pmax);
         if(CM_TIR1_REF[j] <= pmax && CM_TIR1_REF[j] >= pmin)
         {
            warm[i][j] = 1;
            (*warm_len)++;
            //            printf("---> WARM\n");
         }
         else if(CM_TIR1_REF[j] < pmin)
         {
            cold[i][j] = 1;
            (*cold_len)++;
            //            printf("---> COLD\n");
         }
      }
   }
}

/******************************************************************************/
void getTambigStats(CLOUD_MASKS *cm, unsigned char **mask, double *percent, double *avg, long long int len)
{
   int i;
   double *temp;

   if(len == 0)
   {
      *percent = 0;
      *avg = 0;

      return;
   }

   temp = (double*)malloc(sizeof(double)*len);
   getTempByMask(cm, mask, temp, &len);

   *percent = len/(double)cm->vars->scenePixels*100;

   *avg = 0.;
   for(i = 0; i < len; i++) *avg += temp[i];
   *avg /= len;

   free(temp);
}

/******************************************************************************/
void addCMcloudMask(CLOUD_MASKS *cm, unsigned char **mask)
{
   int i, j;

   for(i = 0; i < cm->vars->nl; i++)
      for(j = 0; j < cm->vars->ns; j++)
         if(cm->CMcloud[i][j] == 0 && mask[i][j] == 1) cm->CMcloud[i][j] = mask[i][j];
}

/******************************************************************************/
void doPass2(CLOUD_MASKS *cm)
{
   long long int pass1Cnt, tcloud_len, tambig_len, tambig_warm_len, tambig_cold_len;
   double desratio, snowper, tcloud_avg, *tcloud, tcloud_skew, tcloud_stdev;
   double *tambig, tambig_warm_percent, tambig_cold_percent;
   double tambig_warm_avg, tambig_cold_avg;
   double tmax, tmin, pmax, pmin, skewfactor, tcloud_test, ptest, shift;

   desratio = 100 - (cm->vars->exitdesert/(double)cm->vars->enterdesert)*100;
   snowper = cm->vars->snowCnt/(double)cm->vars->scenePixels*100;
   pass1Cnt = countMaskPixels(cm, cm->CMcloud);

   // allocate tcloud array: size can't be bigger than pass1 clouds
   tcloud = (double *)malloc(sizeof(double)*pass1Cnt);

   printf("desratio: %lf snowper: %lf\n", desratio, snowper);
   if(pass1Cnt/(double)cm->vars->scenePixels*100 < 0.02)
   {
      printf("pass1 < 0.02 percent clouds -- a and b\n");
      zeroCloud(cm);
      return;
   }

   if(desratio > 50. || snowper > 1.)
   {
      if(cm->vars->coldcloud <= 1)
      {
         printf("no cold clouds\n");
         zeroCloud(cm);
         return;
      }

      printf("only cold clouds are clouds, reanalyzing warm clouds\n");
      getTempByMask(cm, cm->CMcloud_cold, tcloud, &tcloud_len);
   }
   else
   {
      printf("both warm and cold clouds are clouds -- a\n");
      getTempByMask(cm, cm->CMcloud, tcloud, &tcloud_len);
   }

   getTcloudStats(tcloud, &tcloud_stdev, &tcloud_avg, tcloud_len);
   if(tcloud_avg >= 295 || desratio >= 50)
   {
      if(countMaskPixels(cm, cm->CMcloud_cold) == 0)
      {
         printf("no cold clouds\n");
         zeroCloud(cm);
         return;
      }

      getTempByMask(cm, cm->CMcloud_cold, tcloud, &tcloud_len);
      getTcloudStats(tcloud, &tcloud_stdev, &tcloud_avg, tcloud_len);

      if(tcloud_avg < 295.)
      {
         printf("final clouds are cold clouds\n");
         setCMcloudMask(cm, cm->CMcloud_cold);
         return;
      }

      printf("tclouds_avg > 295\n");
      zeroCloud(cm);
      return;
   }

   tcloud_skew = getSkew(cm, tcloud, tcloud_stdev, tcloud_avg, tcloud_len);
   printf("tcloud_skew: %lf\n", tcloud_skew);

   qsort(tcloud, tcloud_len, sizeof(double), (int (*)(const void *, const void *)) compare_doubles);
   tmax = tcloud_len*0.975;
   tmin = tcloud_len*0.835;
   pmax = interp1(tcloud, tcloud_len, tmax);
   pmin = interp1(tcloud, tcloud_len, tmin);
   printf("pmin: %lf pmax: %lf\n", pmin, pmax);

   if(tcloud_skew > 0)
   {
      printf("positive skew\n");
      if(tcloud_skew > 1) skewfactor = 1;
      else skewfactor = tcloud_skew;

      shift = skewfactor*tcloud_stdev;
      pmax += shift;
      pmin += shift;

      tcloud_test = tcloud_len*0.9875;
      ptest = interp1(tcloud, tcloud_len, tcloud_test);

      printf("1 - skew: %lf skewfact: %lf pmax: %lf pmin: %lf ptest: %lf\n",
             tcloud_skew, skewfactor, pmax, pmin, ptest);
      if(pmax > ptest)
      {
         pmax = ptest;
         pmin -= pmax-ptest;
      }
      printf("2 - ptest: %lf\n", ptest);
   }
   else
   {
      printf("negative skew\n");
      skewfactor = 0;
   }

   tambig = (double*)malloc(sizeof(double)*cm->vars->scenePixels);
   getTempByMask(cm, cm->ambig, tambig, &tambig_len);
   getTambigWarmColdMask(cm, cm->tambig_warm_mask, &tambig_warm_len, cm->tambig_cold_mask, &tambig_cold_len, pmax, pmin);

   printf("tambig_len: %lld\n", tambig_len);
   printf("warm avg: %lf warm len: %lld\n", tambig_warm_avg, tambig_warm_len);
   getTambigStats(cm, cm->tambig_warm_mask, &tambig_warm_percent, &tambig_warm_avg, tambig_warm_len);
   printf("warm avg: %lf warm len: %lld\n", tambig_warm_avg, tambig_warm_len);
   getTambigStats(cm, cm->tambig_cold_mask, &tambig_cold_percent, &tambig_cold_avg, tambig_cold_len);
   printf("cold avg: %lf cold len: %lld\n", tambig_cold_avg, tambig_cold_len);

   printf("warm: %lf cold: %lf\n", tambig_warm_avg, tambig_cold_avg);

   if(tambig_warm_percent > 40. || tambig_warm_avg > 295. || snowper > 1.)
   {
      if(tambig_cold_percent > 40. || tambig_cold_avg > 295.)
      {
         printf("pass2 analysis is invalid\n");
         return;
      }
      else
      {
         printf("cold clouds are added to pass1\n");
         addCMcloudMask(cm, cm->tambig_cold_mask);
         return;
      }
   }
   else
   {
      printf("cold and warm clouds are added to pass1\n");
      addCMcloudMask(cm, cm->tambig_warm_mask);
      addCMcloudMask(cm, cm->tambig_cold_mask);
   }

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create cartoLoggerUtils.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdlib.h>

/* need these for stat */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include "zmabend.h"
#include "zifmessage.h"
#include "applic.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoLoggerUtils.h"

int labelRaw16bitImage (char * inpath, char * outpath, int nl, int ns) {
  int vunit;
  char * lineBuf = malloc (2 * ns);
  int lineNum;
  FILE * infile = fopen (inpath, "r");
  char msg [100];
  int readFailed = 0;

  if (! lineBuf)
    zmabend ("malloc failed in labelRaw16bitImage");
  if (! infile)
    zmabend ("fopen failed in labelRaw16bitImage");
  
  if (zvunit (& vunit, "U_NAME", 1, "U_NAME", outpath, NULL) != 1)
    zmabend ("zvunit failed for out image");

  if (zvopen (vunit, "U_NL", nl, "U_NS", ns, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "HALF", NULL) != 1)
    zmabend ("zvopen failed for out image");

  for (lineNum = 0; lineNum < nl; lineNum ++) {
    if (readFailed || fread (lineBuf, 2, ns, infile) != ns) {
      if (! readFailed) {
	sprintf (msg, "fread failed at line %d in labelRaw16bitImage; padding image", lineNum + 1);
	zifmessage (msg);
	memset (lineBuf, 0, 2 * ns);
	readFailed = 1;
      }
    }

    zvwrit (vunit,
	    lineBuf,
	    "LINE", lineNum + 1,
	    "SAMP", 1,
	    "NSAMPS", ns, NULL);
  }

  free (lineBuf);

  return vunit;
}

short shortSwapBytes (short s) {
  unsigned char left = ((unsigned char *) & s) [0];
  unsigned char right = ((unsigned char *) & s) [1];
  ((unsigned char *) & s) [0] = right;
  ((unsigned char *) & s) [1] = left;
  return s;
}

void * checkedMalloc (size_t size, char * description) {
  void * p = malloc (size);

  if (! p) {
    char * prefix = "checkedMalloc failed on ";
    char * buf = (char *) malloc (strlen (prefix) + strlen (description) + 1);

    if (! buf)
      zmabend ("checkedMalloc failed allocating message buffer");

    sprintf (buf, "%s%s", prefix, description);

    zmabend (buf);
  }

  return p;
}

/* This routine mallocs space for the contents of a file and reads it
   into the malloced buffer. The caller should free the buffer when no
   longer needed. The read data is null terminated. */
char * mallocAndRead (char * path) {
  FILE * file;
  char msgBuf [100];
  struct stat statBuf;
  char * buf = 0;
  int status;

  /* stat the file to determine size, so we can read it all at once */
  if (stat (path, & statBuf)) {
    sprintf (msgBuf, "error stating \"%s\"", path);
    zmabend (msgBuf);
  }

  if (! (buf = (char *) malloc (statBuf.st_size + 1)))
    zmabend ("error mallocing file buffer");

  /* open file */
  if (! (file = fopen (path, "r"))) {
    sprintf (msgBuf, "error opening %s for input", path);
    zmabend (msgBuf);
  }

  /* read the data */
  if ((status = fread (buf, 1, statBuf.st_size, file)) != statBuf.st_size) {
    sprintf (msgBuf, "error reading %s", path);
    zmabend (msgBuf);
  }
  
  buf [statBuf.st_size] = 0;	/* null terminate the data */

  fclose (file);		/* done with data */

  return buf;
}

void getDataSetTime (char * dataSetName, char * times, int * utcTimeInMinutes, char * date, char * time) {
  char * p = strstr (times, dataSetName);
  int hours, minutes;
  char msgBuf [1000];

  if (! p || p - 40 < times) {
    sprintf (msgBuf, "Data set %s not found in SAA times data", dataSetName);
    zmabend (msgBuf);
  }

  strncpy (date, p - 40, 10);
  date [10] = 0;
  strncpy (time, p - 40 + 11, 12);
  time [12] = 0;

  sscanf (time, "%2d:%2d:", & hours, & minutes);
  * utcTimeInMinutes = hours * 60 + minutes;
}

void forceSubAreaSanity (int * sl, int * ss, int * nl, int * ns, int lines, int samples) {
  if (* sl < 1)
    * sl = 1;
  if (* sl > lines)
    * sl = lines;

  if (* ss < 1) 
    * ss = 1;
  if (* ss > samples)
    * ss = samples;

  if (* nl < 1 || * nl > lines - * sl + 1)
    * nl = lines - * sl + 1;
  if (* ns < 1 || * ns > samples - * ss + 1)
    * ns = samples - * ss + 1;
}

int usingLatLonSubArea (int imageLines, int imageSamples,
		       double * minLat, double * maxLat, double * minLon, double * maxLon,
		       int * sl, int * ss, int * nl, int * ns, int allow) {
  int parmct, pcount, pdef;

  /* fetch line/sample params */
  zvp ("sl", sl, & parmct);
  zvp ("ss", ss, & parmct);
  zvp ("nl", nl, & parmct);
  zvp ("ns", ns, & parmct);

  /* fetch lat/lon params */
  zvparmd ("minlat", minLat, & pcount, & pdef, 1, 0);
  zvparmd ("maxLat", maxLat, & pcount, & pdef, 1, 0);
  zvparmd ("minLon", minLon, & pcount, & pdef, 1, 0);
  zvparmd ("maxLon", maxLon, & pcount, & pdef, 1, 0);

  /* if a line/sample value was specified */
  if (* sl != -999 || * ss != -999 || * nl != -999 || * ns != -999) {
    /* ensure they all were specified */
    if (* sl == -999 || * ss == -999 || * nl == -999 || * ns == -999)
      zmabend ("some but not all of (sl, ss, nl, ns) sub-area parameters were specified");

    /* ensure no lat/lon values were specified */
    if (* minLat != -999.0 || * maxLat != -999.0 || * minLon != -999.0 || * maxLat != -999.0)
      zmabend ("both line/sample and lat/lon sub-area parameters were specified");

    /* ensure line/sample values are within image */
    if (! BETWEEN (1, * sl, imageLines) ||
	! BETWEEN (1, * ss, imageSamples) ||
	! BETWEEN (1, * sl + * nl - 1, imageLines) ||
	! BETWEEN (1, * ss + * ns - 1, imageSamples))
      zmabend ("line/sample sub-area parameter(s) outside of image");

  } else if (allow &&
	     (* minLat != -999.0 || * maxLat != -999.0 || * minLon != -999.0 || * maxLat != -999.0)) {
    /* a lat/lon value was specified */
    /* ensure they all are specified */
    if (* minLat == -999.0 || * maxLat == -999.0 || * minLon == -999.0 || * maxLat == -999.0) {
      if (* minLat == -999.0)
	* minLat = -90.0;
      if (* maxLat == -999.0)
	* maxLat = 90.0;
      if (* minLon == -999.0)
	* minLon = -180.0;
      if (* maxLon == -999.0)
	* maxLon = 360.0;
    }

    /* ensure no line/sample values were specified */
    if (* sl != -999 || * ss != -999 || * nl != -999 || * ns != -999)
      zmabend ("both line/sample and lat/lon sub-area parameters were specified");

    return 1;

  } else {
    /* no sub-area was specified; use whole image */

    * sl = 1;
    * ss = 1;
    * nl = imageLines;
    * ns = imageSamples;
  }

  return 0;
}

void initMetaData (char * path) {
  FILE * f;
  time_t now;
  char msgBuf [100];

  if (! (f = fopen (path, "w"))) {
    sprintf (msgBuf, "error opening parm file \"%s\" for writing", path);
    zmabend (msgBuf);
  }

  if (time (& now) < 0)
    zmabend ("error reading current time");

  strcpy (msgBuf, ctime (& now));
  msgBuf [strlen (msgBuf) - 1] = 0; /* trim the trailing newline */
  fprintf (f, "LOG_TIME=\"%s\"\n", msgBuf);

  fclose (f);    
}

#define metaDataLabelProperty "LOGGER_META_DATA"

void metaToLabel (char * metaName, int vunit) {
  int status;
  char * buf = mallocAndRead (metaName);
  int size = strlen (buf);

  if ((status = zladd (vunit, "PROPERTY",
		       "ALL_META_DATA_SIZE", & size,
		       "PROPERTY", metaDataLabelProperty,
		       "FORMAT", "INT", NULL)) != 1)
    zmabend ("zladd failed to add a label for ALL_META_DATA_SIZE");

  if ((status = zladd (vunit, "PROPERTY",
		       "ALL_META_DATA", buf,
		       "PROPERTY", metaDataLabelProperty,
		       "FORMAT", "STRING", NULL)) != 1)
    zmabend ("zladd failed to add a label for ALL_META_DATA");
  free (buf);
}

void logMetaString (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits) {
  logMetaStringToProperty (echoMeta, path, name, value, unitCount, vunits, 0, 0);
}

void logMetaStringToProperty (int echoMeta, char * path, char * name, char * value, int unitCount, int * vunits, char * property, int noQuotes) {
  FILE * f;
  char msgBuf [100];
  int status;
  int vunit;
  
  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    if (noQuotes)
      fprintf (f, "%s=%s\n", name, value);
    else
      fprintf (f, "%s=\"%s\"\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + strlen (value) + 2, /* + 2 for = and null characters */
				"buf for meta string echo");
    sprintf (buf, "%s=%s", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, value,
			 "PROPERTY", property?property:metaDataLabelProperty,
			 "FORMAT", "STRING", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaString");
}

void logMetaInt (int echoMeta, char * path, char * name, int value, int unitCount, int * vunits) {
  FILE * f;
  char msgBuf [100];
  int vunit;
  int status;

  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    fprintf (f, "%s=%d\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + 20, "buf for meta int echo");
    sprintf (buf, "%s=%d", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, & value,
			 "PROPERTY", metaDataLabelProperty,
			 "FORMAT", "INT", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaInt");
}

void logMetaDouble (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits) {
  logMetaDoubleToProperty (echoMeta, path, name, value, unitCount, vunits, 0);
}

void logMetaDoubleToProperty (int echoMeta, char * path, char * name, double value, int unitCount, int * vunits, char * property) {
  FILE * f;
  char msgBuf [100];
  int vunit;
  int status;

  if (path) {
    if (! (f = fopen (path, "a"))) {
      sprintf (msgBuf, "error opening parm file \"%s\" for appending", path);
      zmabend (msgBuf);
    }

    fprintf (f, "%s=%.16lf\n", name, value);

    fclose (f);    
  }

  if (echoMeta) {
    char * buf = checkedMalloc (strlen (name) + 35, "buf for double string echo");
    sprintf (buf, "%s=%18.16lf", name, value);
    zifmessage (buf);
    free (buf);
  }

  for (vunit = 0; vunit < unitCount; vunit ++)
    if ((status = zladd (vunits [vunit], "PROPERTY",
			 name, & value,
			 "PROPERTY", property?property:metaDataLabelProperty,
			 "FORMAT", "DOUB", NULL)) != 1)
      zmabend ("zladd failed to add a label in logMetaDouble");
}

void checkLoggerUtilsVersion (int minimum) {
  char msgBuf [100];

  sprintf (msgBuf, "expected loggerutils version >= %d, but using %d\n",
	   minimum, _loggerutils_version_);

  if (_loggerutils_version_ < minimum)
    zmabend (msgBuf);
}

#define IBISNCol 4
int logNavDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
		      int numRows,
		      double * lineColumn, double * sampleColumn,
		      double * latColumn, double * lonColumn) {
  char IBISColumnFormats [IBISNCol] [6];
  int vunit, iunit;
  int i, status;

   /* set up IBIS column format labels */
  for (i = 0; i < IBISNCol; i ++)
    strcpy (IBISColumnFormats [i], "DOUB");

   /* open/setup IBIS navigation output */
  if (! strcmp (zvunitParmName, "OUT")){
    if ((status = zvunit (& vunit, zvunitParmName, zvunitParmIndex, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  } else {			/* custom name */
    if ((status = zvunit (& vunit, "U_NAME", zvunitParmIndex, "U_NAME", zvunitParmName, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  }
  if ((status = IBISFileUnit (vunit, &iunit, "write", IBISNCol, numRows, (char *) IBISColumnFormats, "column")) != 1)
    IBISSignal (iunit, status, 1);
  if ((status = IBISFileUnitOpen (iunit)) != 1)
    IBISSignal (iunit, status, 1);
  for (i = 0; i < IBISNCol; i ++)
    if ((status = IBISColumnSet (iunit, "U_FORMAT", "DOUB", i + 1 /* one based value*/)) != 1)
      IBISSignal (iunit, status, 1);
			  
  /* write columns LINE, SAMPLE, LAT, LON */
  if ((status = IBISColumnWrite (iunit,
				 (char*)lineColumn,       /* column data */
				 1,	           /* one based column number (line column)*/
				 1,                /* one based line number */
				 numRows)) != 1 || /* one IBIS row for each image pixel */
      (status = IBISColumnWrite (iunit,
				 (char*)sampleColumn,
				 2,	           /* sample column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)latColumn,   
				 3,	           /* lat column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)lonColumn,   
				 4,	           /* lon column */
				 1,           
				 numRows)) != 1)
    IBISSignal (iunit, status, 1);

  /* done with IBIS navigation output */
  if ((status = IBISFileClose (iunit, 0)) != 1)
    IBISSignal (iunit, status, 1);

  return 1;
}

#undef IBISNCol
#define IBISNCol 6
int logNavAndAnglesDataToIBIS (char * zvunitParmName, int zvunitParmIndex,
			       int numRows,
			       double * lineColumn, double * sampleColumn,
			       double * latColumn, double * lonColumn,
			       double * zenithColumn, double * azimuthColumn) {
  char IBISColumnFormats [IBISNCol] [6];
  int vunit, iunit;
  int i, status;

   /* set up IBIS column format labels */
  for (i = 0; i < IBISNCol; i ++)
    strcpy (IBISColumnFormats [i], "DOUB");

   /* open/setup IBIS navigation output */
  if (! strcmp (zvunitParmName, "OUT")){
    if ((status = zvunit (& vunit, zvunitParmName, zvunitParmIndex, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  } else {			/* custom name */
    if ((status = zvunit (& vunit, "U_NAME", zvunitParmIndex, "U_NAME", zvunitParmName, NULL)) != 1)
      zmabend ("zvunit failed in logNavDataToIBIS");
  }
  if ((status = IBISFileUnit (vunit, &iunit, "write", IBISNCol, numRows, (char *) IBISColumnFormats, 0)) != 1)
    IBISSignal (iunit, status, 1);
  if ((status = IBISFileUnitOpen (iunit)) != 1)
    IBISSignal (iunit, status, 1);
  for (i = 0; i < IBISNCol; i ++)
    if ((status = IBISColumnSet (iunit, "U_FORMAT", "DOUB", i + 1 /* one based value*/)) != 1)
      IBISSignal (iunit, status, 1);
			  
  /* write columns LINE, SAMPLE, LAT, LON */
  if ((status = IBISColumnWrite (iunit,
				 (char*) lineColumn,       /* column data */
				 1,	           /* one based column number (line column)*/
				 1,                /* one based line number */
				 numRows)) != 1 || /* one IBIS row for each image pixel */
      (status = IBISColumnWrite (iunit,
				 (char*)sampleColumn,
				 2,	           /* sample column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)latColumn,   
				 3,	           /* lat column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)lonColumn,   
				 4,	           /* lon column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)zenithColumn,   
				 5,	           /* zenith column */
				 1,           
				 numRows)) != 1 ||
      (status = IBISColumnWrite (iunit,
				 (char*)azimuthColumn,   
				 6,	           /* azimuth column */
				 1,           
				 numRows)) != 1)
    IBISSignal (iunit, status, 1);

  /* done with IBIS navigation output */
  if ((status = IBISFileClose (iunit, 0)) != 1)
    IBISSignal (iunit, status, 1);

  return 1;
}

/* To convert between geocentric latitude (gc) and geodetic latitude (gd), use the formula 
   tan(gc) = tan(gd) * (1-f)^2
   where f is the flattening parameter =  1 / 298.257223563
*/
#define degrad (3.14159265359 / 180.0)
#define f (1.0 / 298.257223563)
#define divisor ((1.0 - f) * (1.0 - f))
double geocentricToGeodetic (double latitude) {
  return atan (tan (latitude * degrad) / divisor) / degrad;
}

void addGTKey (int vunit, char * key, char * value) {
  char msgBuf [200];

  if (zladd (vunit, "PROPERTY",
	     key, value,
	     "PROPERTY", "GEOTIFF",
	     "FORMAT", "STRING", NULL) != 1) {
    sprintf (msgBuf, "addGTKey failed to add a label for key %s, value %s", key, value);
    zmabend (msgBuf);
  }
}

static int leapYear (int year) {
  int leap = 0;

  if (year % 4 == 0)
    leap = 1;
  if (year % 100 == 0)
    leap = 0;
  if (year % 400 == 0)
    leap = 1;

  return leap;
}

int daysInMonth (int year, int month) {
  switch (month) {
  case 2:
    return 28 + leapYear (year);
  case 4: case 6: case 9: case 11:
    return 30;
  default:
    return 31;
  }
}

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
int dateToDayOfYear (int year, int month, int day) {
  int thisMonth;
  int dayOfYear = day;

  for (thisMonth = 1; thisMonth < month; thisMonth ++)
    dayOfYear += daysInMonth (year, thisMonth);
  
  return dayOfYear;
}

/* dayOfYear is 1-based, e.g. dayOfYear==1 => January 1 */
void dayOfYearToDate (int dayOfYear, int year, int * month, int * day) {
  int daysSoFar = 0;

  for (* month = 1; * month <= 12; (* month) ++) {
    if (dayOfYear <= daysSoFar + daysInMonth (year, * month)) {
      * day = dayOfYear - daysSoFar;
      return;
    }

    daysSoFar += daysInMonth (year, * month);
  }
}

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
                    cartoMemUtils.c cartoSortUtils.c cartoStrUtils.c cartoTaeUtils.c \
                    astroreference_camera.c ephreference_camera.c eos_coords.c \
                    mat33.c qmalloc.c quaternion.c rodrigues.c time_conversion.c \
                    safe_sqrt.c time_utils.c strsel.c count_lines.c tokenize.c \
                    fgetl.c sprintf_alloc.c cartoTieUtils.c ibisControlMapper.c \
                    ibishelper.c cartoLinkedList.c ImageUtils.c cloud_masks.c \
                    cartoLoggerUtils.c

#define USES_ANSI_C

#define LIB_TAE

/*#define LIB_LOCAL       /* for development, remove on delivery */ 
/*#define DEBUG           /* for development, remove on delivery */ 

$ Return
$!#############################################################################
