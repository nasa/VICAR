$!****************************************************************************
$!
$! Build proc for MIPL module tieconv
$! VPACK Version 1.9, Wednesday, December 21, 2011, 16:15:09
$!
$! Execute by entering:		$ @tieconv
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
$ write sys$output "*** module tieconv ***"
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
$ write sys$output "Invalid argument given to tieconv.com file -- ", primary
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
$   if F$SEARCH("tieconv.imake") .nes. ""
$   then
$      vimake tieconv
$      purge tieconv.bld
$   else
$      if F$SEARCH("tieconv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake tieconv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @tieconv.bld "STD"
$   else
$      @tieconv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create tieconv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack tieconv.com -mixed -
	-s tieconv.c -
	-i tieconv.imake -
	-p tieconv.pdf -
	-t tsttieconv.pdf devtieconv.pdf tsttieconv.log_solos
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create tieconv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

/*#include "cartoMatUtils.h"
#include "cartoSortUtils.h"
#include "cartoLsqUtils.h"
#include "cartoMemUtils.h"
#include "cartoVicarProtos.h"
#include "cartoGridUtils.h"*/

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
#define MAXLSQ          400
#define MAXLSQD         MAXLSQ+1
#define MAXLSQ2         2*MAXLSQD
#define MAXLSQ10        10*MAXLSQD
#define MAXTIE          10000000

/************************************************************************/
/* program tieconv                                                      */
/************************************************************************/
/*  83-10 ...alz... initial version, new algorithm for thiessen         */
/*                  triangulation, converted to c,                      */
/************************************************************************/

short int *vgroup;
int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
      cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
      ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
double gridtol;
typedef unsigned char  byte;

/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1(s) unsigned char *s; { return(*s); }
short int ct2(s) short int *s; { return(*s); }
int ct4(s) int *s; { return(*s); }
float ct7(s) float *s; { return(*s); }
double ct8(s) double *s; { return(*s); }
void st1(v,s) unsigned char v,*s; { *s = v; return; }
void st2(v,s) short int v,*s; { *s = v; return; }
void st4(v,s) int v,*s; { *s = v; return; }
void st7(v,s) float v,*s; { *s = v; return; }
void st8(v,s) double v,*s; { *s = v; return; }

void tgridinf(double* ptx,
	   double* pty,
	   double dist,
	   int ptr1,
	   int ptr2,
	   int ptr3)
{
   double x1,x2,y1,y2,dx,dy,ds,vx,vy;

   x1 = ptx[ptr1];
   x2 = ptx[ptr2];
   y1 = pty[ptr1];
   y2 = pty[ptr2];

   dx = x2-x1;
   dy = y2-y1;
   ds = sqrt(dx*dx+dy*dy);
   vx = (y1-y2)/ds;
   vy = (x2-x1)/ds;
   ptx[ptr3] = vx*dist+0.5*(x1+x2);
   pty[ptr3] = vy*dist+0.5*(y1+y2);
   return;
}

void tgridchk(int npoints,
	   double* ptx,
	   double* pty,
	   int** tcon1,
	   int** tcon2,
	   int** tcon3,
	   int gridnah,
           int ptr)
{
   int i,nah,nav,nahp,navp,n;
   double xtst,ytst;
   
   n = npoints-4;
   nah = gridnah;
   nahp = nah+1;
   navp = n/nahp;
   nav = navp-1;

   for (i=1;i<nah-1;i++)
      {
      xtst = ptx[nah+i+1];
      ytst = pty[nah+i+1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, top row");
            }

      xtst = ptx[n-2*nah+i-2];
      ytst = pty[n-2*nah+i-2];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, bottom row");
            }
      }

   for (i=1;i<nav-1;i++)
      {
      xtst = ptx[i*nahp+1];
      ytst = pty[i*nahp+1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, left column");
            }
      xtst = ptx[i*nahp+nah-1];
      ytst = pty[i*nahp+nah-1];
      if (insidetri(xtst,ytst,
         ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1],
         ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1],
         ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]))
            { printf("IN\n");
            printf("pt: %f %f ptr=%d\n",xtst,ytst,ptr);
            printf("tri: ptx[(*tcon1)[ptr-1]-1] %f %f\n",ptx[(*tcon1)[ptr-1]-1],pty[(*tcon1)[ptr-1]-1]);
            printf("tri: ptx[(*tcon2)[ptr-1]-1] %f %f\n",ptx[(*tcon2)[ptr-1]-1],pty[(*tcon2)[ptr-1]-1]);
            printf("tri: ptx[(*tcon3)[ptr-1]-1] %f %f\n",ptx[(*tcon3)[ptr-1]-1],pty[(*tcon3)[ptr-1]-1]);
            zmabend("bad triangulation of grid, right column");
            }
      }
   return;
}

/*  subroutine tgrid:
triangulates a grid, and the four extrapolated points in the last four
positions as generated by polygeov main */

void tgridxxx(int npoints,
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
   double bigdist,x,y,fac;
   
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
  
   bigdist = 0.6*(abs(ptx[n-1]-ptx[0])+abs(pty[n-1]-pty[0])+
      abs(ptx[n-nah-1]-ptx[nah])+abs(pty[n-nah-1]-pty[nah]));
   tgridinf(ptx,pty,bigdist,0,nah,n);

   /*printf("ptx[0] %f %f\n",ptx[0],pty[0]);
   printf("ptx[nah] %f %f\n",ptx[nah],pty[nah]);
   printf("ptx[n] %f %f\n\n",ptx[n],pty[n]);*/

   tgridinf(ptx,pty,bigdist,nah,n-1,n+1);

   /*printf("ptx[nah] %f %f\n",ptx[nah],pty[nah]);
   printf("ptx[n-1] %f %f\n",ptx[n-1],pty[n-1]);
   printf("ptx[n+1] %f %f\n\n",ptx[n+1],pty[n+1]);*/

   tgridinf(ptx,pty,bigdist,n-1,n-nah-1,n+3);

   /*printf("ptx[n-1] %f %f\n",ptx[n-1],pty[n-1]);
   printf("ptx[n-nah-1] %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
   printf("ptx[n+3] %f %f\n\n",ptx[n+3],pty[n+3]);*/

   tgridinf(ptx,pty,bigdist,n-nah-1,0,n+2);

   /*printf("ptx[n-nah-1] %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
   printf("ptx[0] %f %f\n",ptx[0],pty[0]);
   printf("ptx[n+2] %f %f\n\n",ptx[n+2],pty[n+2]);*/

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
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with top side\n");*/
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = opoint+1;
   (*tcon2)[ptr++] = nahp;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with top corner 1\n");*/
   (*tcon1)[ptr] = opoint;
   (*tcon3)[ptr] = 1;
   (*tcon2)[ptr++] = opoint+2;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with top corner 2\n");*/

            /*printf("ptx[0] %f %f\n",ptx[0],pty[0]);
            printf("ptx[nah+1] %f %f\n",ptx[nah+1],pty[nah+1]);
            printf("ptx[nah+2] %f %f\n\n",ptx[nah+2],pty[nah+2]);

            printf("ptx[nah] %f %f\n",ptx[nah+2],pty[nah]);
            printf("ptx[2*nah+1]  %f %f\n",ptx[2*nah+1],pty[2*nah+1]);
            printf("ptx[2*nah]  %f %f\n\n",ptx[2*nah],pty[2*nah]);

            printf("ptx[n-nah-1]  %f %f\n",ptx[n-nah-1],pty[n-nah-1]);
            printf("ptx[n-2*nah-2]  %f %f\n",ptx[n-2*nah-2],pty[n-2*nah-2]);
            printf("ptx[n-2*nah-1]  %f %f\n\n",ptx[n-2*nah-1],pty[n-2*nah-1]);

            printf("ptx[n-1]  %f %f\n",ptx[n-1],pty[n-1]);
            printf("ptx[n-2]  %f %f\n",ptx[n-2],pty[n-2]);
            printf("ptx[n-nah-3]  %f %f\n\n",ptx[n-nah-3],pty[n-nah-3]);*/


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
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      (*tcon1)[ptr] = i*nahp+1;
      (*tcon3)[ptr] = (i+1)*nahp+1;
      (*tcon2)[ptr++] = opoint+2;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with sides\n");*/

   /* edge triangles to bottom outside point */
   
   for (i=0;i<nah;i++)
      {
      (*tcon1)[ptr] = nav*nahp+i+1;
      (*tcon3)[ptr] = nav*nahp+i+2;
      (*tcon2)[ptr++] = opoint+3;
      tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
      }
   /*printf("done with bottom row\n");*/
   (*tcon1)[ptr] = opoint+3;
   (*tcon3)[ptr] = opoint+2;
   (*tcon2)[ptr++] = n-nahp+1;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with bottom left\n");*/
   
   (*tcon1)[ptr] = opoint+3;
   (*tcon3)[ptr] = n;
   (*tcon2)[ptr++] = opoint+1;
   tgridchk(npoints,ptx,pty,tcon1,tcon2,tcon3,gridnah,ptr);
   /*printf("done with bottom right\n");*/
   
      /*printf("triB3 %f %f\n %f %f\n%f %f \n",      useful code
     ptx[(*tcon1)[ptr-1]-1]+3388.0,pty[(*tcon1)[ptr-1]-1]+3722.0,
     ptx[(*tcon2)[ptr-1]-1]+3388.0,pty[(*tcon2)[ptr-1]-1]+3722.0,
     ptx[(*tcon3)[ptr-1]-1]+3388.0,pty[(*tcon3)[ptr-1]-1]+3722.0);*/
   
   return;
}





void main44(void)
{
   double *rpar;
   int *con1,*con2,*con3;
   
   double tmaxx,tmaxy,tminx,tminy,x,y,xx,yy=0,*ptx,*pty;
   double clsq[MAXLSQ10], clsqxy[MAXLSQ2], elsqxy[MAXLSQ2];
   double csol[20],*optx,*opty;
   double tab[4],work[16];
   double **coeff,dx,dy,**vout;
   double minx,miny,maxx,maxy,normlz[4];
   float **pout,*rout,fac;
   int cols[4];
   int zgeom,mgeom,abendl,abendg,lgeom,geomv,keystone,linear; /*booleans*/
   int quad,cubic,polynom; /*booleans*/
   int plot,printit;
   int found,null9;
   int nah = 30,nav = 30,npoint = 4,nrank = 6,nerr = 0;
   int geoma = 1,lgeomlike = 0;
   char outnam[73],polystring[10];
   
   int i,j,k,n,ier,inpcnt,status,unit,colcount,ibis,clen,ptr,record;
   int irow,ntiepp,icnt,idef,nvt,nklsq,nlret;
   int ntri,nah1,nav1,gridnah,gridnav=0,pcount,pdef;
   int ttri,ttrj1,minldf,maxldf,minsdf,maxsdf,coldef,tiepdef,nht,tiept;
   int ix,itri,tri,isign,p,ibisOut,parmOut,ptr2,nout,outdf;
   double eps,skinny,reject;
        
   zifmessage("tieconv version Tue Mar 01 2011");
   
   printit = zvptst("PRINT");
   
   zvparm("poly",polystring,&pcount,&pdef,4,0);
   keystone = strncmp(polystring,"KEYSTONE",8)==0;
   linear = strncmp(polystring,"LINEAR",6)==0;
   quad = strncmp(polystring,"QUAD",4)==0;
   cubic = strncmp(polystring,"CUBIC",5)==0;
   polynom = keystone||linear||quad||cubic;
   zvparmd("gridtol",&gridtol,&pcount,&pdef,1,0);
   
   /*	if inp file specified then read in tiepoints from
	  the ibis interface file */

   status = zvpcnt("inp",&inpcnt);
   if (inpcnt>0)
      {
      status = zvunit(&unit,"inp",1, NULL);
     
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1,0);
      mz_alloc1((unsigned char **)&rpar,colcount*clen,8);
      ptr = 0;
      status = IBISRecordOpen(ibis,&record,0,cols,colcount,IFMT_DOUB);
      if (status!=1) IBISSignal(ibis,status,1);
      for (irow=1;irow<=clen;irow++)
	 {
         status = IBISRecordRead(record,(char*)(&rpar[ptr]),irow);
         if (status!=1) IBISSignal(ibis,status,1);
	 ptr += colcount;
	 }
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      ntiepp = ptr;
      }
   else
      {
      mz_alloc1((unsigned char **)&rpar,1625,8);
      zvparm("tiepoint",&rpar,&ntiepp,&tiepdef,1625,0);
      }
   zvparm("nah",&nht,&icnt,&idef,1,0);
   zvparm("nav",&nvt,&icnt,&idef,1,0);
   zvparm("gridnah",&gridnah,&icnt,&idef,1,0);

   abendl = zvptst("abend");
   abendg = zvptst("abendg");
   reject = 0.01;
   zvparm("reject",&reject,&icnt,&idef,1,0);
   reject = reject*reject;
   geoma = zvptst("geoma");
   geomv = zvptst("geomv");
   mgeom = zvptst("mgeom");
   lgeom = zvptst("lgeom");
   lgeomlike = mgeom||lgeom||geomv;
   zgeom = zvptst("geomz");
   plot = zvptst("plot");
   if (plot&&polynom)
      zmabend("can't plot with polynomial fit options");
   
   if (zgeom) npoint = 3;
   if (zgeom) nrank = 3;
   
   n = ntiepp/npoint;
   if (n<3) zmabend("need 3 tiepoints");
   if (n<4&&keystone) zmabend("need 4 tiepoints for keystone option");
   if (n<6&&quad) zmabend("need 6 tiepoints for quadratic option");
   if (n<10&&cubic) zmabend("need 10 tiepoints for cubic option");
   if (n>MAXTIE) zmabend("maximum input tiepoints exceeded");
   if (lgeomlike)
      {
      for (i=0;i<ntiepp;i+=4)
         {
         dx = rpar[i];
         rpar[i] = rpar[i+2];
         rpar[i+2] = dx;
         dx = rpar[i+1];
         rpar[i+1] = rpar[i+3];
         rpar[i+3] = dx;
         }
      }
 
   tmaxx = 0.0;
   tmaxy = 0.0;
   tminx = 1.e20;
   tminy = 1.e20;
   if (n>MAXLSQ) nklsq = MAXLSQ; else nklsq = n;
   
   mz_alloc1((unsigned char **)&ptx,n+4,8);
   mz_alloc1((unsigned char **)&pty,n+4,8);
   mz_alloc1((unsigned char **)&optx,n+4,8);
   if (!zgeom) mz_alloc1((unsigned char **)&opty,n+4,8);
   
   /* transfer to vector format for thiessen; the slight random 
   perterbation was an experiment, can help with debugging the 
   rectangular grid input case */
   
   /*srand((unsigned)1);*/
   if (zgeom) ptr = 0; else ptr = 2;
   for (i=0;i<4;i++) normlz[i] = 0.0;
   for (i=0;i<n;i++)
      {
      /*  experimental code, also need srand() call above
      randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
      ptx[i] = rpar[ptr]*randout;
      randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
      pty[i] = rpar[ptr+1]*randout;*/
      
      ptx[i] = rpar[ptr];
      pty[i] = rpar[ptr+1];
      
      if (!zgeom)
         {
         optx[i] = rpar[ptr-2];
         opty[i] = rpar[ptr-1];
         }
      else optx[i] = rpar[ptr+2];
      
      normlz[0] += ptx[i];
      normlz[1] += pty[i];
      normlz[2] += optx[i];
      if (!zgeom) normlz[3] += opty[i];
      
      if (ptx[i]>tmaxx) tmaxx = ptx[i];
      if (ptx[i]<tminx) tminx = ptx[i];
      if (pty[i]>tmaxy) tmaxy = pty[i];
      if (pty[i]<tminy) tminy = pty[i];
      ptr += npoint;
      }
   free(rpar);
   for (i=0;i<4;i++) normlz[i] = (double)((int)(normlz[i]/(double)n+0.5));
   for (i=0;i<n;i++)
      {
      ptx[i] -= normlz[0];
      pty[i] -= normlz[1];
      optx[i] -= normlz[2];
      if (!zgeom) opty[i] -= normlz[3];
      }
   tmaxx -= normlz[0];
   tminx -= normlz[0];
   tmaxy -= normlz[1];
   tminy -= normlz[1];
   
   /* detect grid here and set gridnah, not necessary for polynom cases */
   /* some grids have null areas (-999,-999) for example off earth.  these */
   /* are required to be a grid, and the getlinef is applied to fill the */
   /* -999 areas with an approximate grid */
   
   if (!polynom)
      {
      cartogetline(ptx,pty,0,1,n,n,&gridnah,&null9);
      if (null9==1) goto nullarea;
      if (gridnah==0) goto endgrid;
      if (n%(gridnah+1)!=0) { gridnah = 0; goto endgrid; }
      gridnav = n/(gridnah+1)-1;
      if (n%(gridnav+1)!=0) { gridnah = 0; goto endgrid; }
      if ((double)gridnah<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      if ((double)gridnav<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      for (i=1;i<=gridnav;i++)
         {
         cartogetline(ptx,pty,i*(gridnah+1),1,gridnah+1,n,&k,&null9);
         if (null9==1) goto nullarea2;
         if (k!=gridnah) { gridnah = 0; goto endgrid; }
         }
      for (i=0;i<=gridnah;i++)
         {
         cartogetline(ptx,pty,i,gridnah+1,gridnav+1,n,&k,&null9);
         if (null9==1) goto nullarea2;
         if (k!=gridnav) { gridnah = 0; goto endgrid; }
         }
      }
   goto endgrid;
   nullarea:   /* try five more ways to find grid with null areas */
      cartogetline(ptx,pty,n-1,-1,n,n,&gridnah,&null9);
      if (null9==0)
         {
         if (n%(gridnah+1)!=0) zmabend("(nullarea) failure to find grid");
         gridnav = n/(gridnah+1)-1;
         if (n%(gridnav+1)!=0) zmabend("(nullarea) failure to find grid");
         goto nullarea2;
         }
      getline2(ptx,pty,0,1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline2(ptx,pty,n-1,-1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline3(ptx,pty,0,1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      getline3(ptx,pty,n-1,-1,n/2,n,&gridnah);
      if (gridnah>0) gridnav = n/(gridnah+1)-1;
      if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
      else zmabend("(nullarea) failure to find grid");
   nullarea2: 
      for (i=0;i<=gridnav;i++) gridfill(ptx,pty,i*(gridnah+1),1,gridnah);
      for (i=0;i<=gridnav;i++) gridfill(ptx,pty,(i+1)*(gridnah+1)-1,-1,gridnah);
      for (i=0;i<=gridnah;i++) gridfill(ptx,pty,i,gridnah+1,gridnav);
      for (i=0;i<=gridnah;i++) gridfill(ptx,pty,n-1-i,-gridnah-1,gridnav);
   endgrid:
      
   /* for the large case, the random formula scatters the points to be
   fitted across the area extended; will get duplicates but that is not
   a problem with a sample of 400; the sequence repeats for the the j 
   loop */
   
   for (j=0;j<npoint-2;j++)
      {
      k = 0;
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = (k*379+i*i)%n; else k = i;
         clsq[i] = ptx[k];
         clsq[i+nklsq] = pty[k];
         clsq[i+nklsq*2] = 1.0;
         clsq[i+nklsq*3] = ptx[k]*pty[k];
         clsq[i+nklsq*4] = ptx[k]*ptx[k];
         clsq[i+nklsq*5] = pty[k]*pty[k];
         clsq[i+nklsq*6] = ptx[k]*ptx[k]*ptx[k];
         clsq[i+nklsq*7] = ptx[k]*ptx[k]*pty[k];
         clsq[i+nklsq*8] = ptx[k]*pty[k]*pty[k];
         clsq[i+nklsq*9] = pty[k]*pty[k]*pty[k];
         clsqxy[i] = optx[k];
         if (!zgeom) clsqxy[i+nklsq] = opty[k];
         elsqxy[i] = optx[k];
         if (!zgeom) elsqxy[i+nklsq] = opty[k];
         }
      eps = 1.e-7;
      for (i=0;i<10;i++) csol[i+j*10] = 0.0;
      if (linear)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
      else if (keystone)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,4,&csol[j*10],eps,&ier);
      else if (quad)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,6,&csol[j*10],eps,&ier);
      else if (cubic)
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,10,&csol[j*10],eps,&ier);
      else
         lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
      }
   if (ier!=0)
      {
      printf("j,npoint %d %d\n",j,npoint);
      zmabend("least squares fit failure");
      }
   zvparmd("mins",&miny,&icnt,&minsdf,1,0);
   zvparmd("maxs",&maxy,&icnt,&maxsdf,1,0);
   zvparmd("minl",&minx,&icnt,&minldf,1,0);
   zvparmd("maxl",&maxx,&icnt,&maxldf,1,0);
   minx -= normlz[0];
   maxx -= normlz[0];
   miny -= normlz[1];
   maxy -= normlz[1];
   if (minsdf==1) miny = tminy;
   if (maxsdf==1) maxy = tmaxy;
   if (minldf==1) minx = tminx;
   if (maxldf==1) maxx = tmaxx;
   if (lgeom) nah = 10; else if (geomv) nah = 50; else nah = 30;
   if (lgeom) nav = 10; else if (geomv) nav = 50; else nav = 30;
   if (nht!=0) nah = nht;
   if (nvt!=0) nav = nvt;
   dx = (tmaxx-tminx+tmaxy-tminy)*5.0;
   if (polynom) goto polyout;
   
   if (printit)
      {
      zprnt(8,3,csol,"lsq fit x'=ax+by+c.");
      if (!zgeom) zprnt(8,3,&csol[10],"lsq fit y'=dx+ey+f.");
      zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
      }
      
   for (i=0;i<nklsq;i++)
      {
      if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
      elsqxy[i] = elsqxy[i]-ptx[k]*csol[0]-pty[k]*csol[1]-csol[2];
      if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]-
             ptx[k]*csol[10]-pty[k]*csol[11]-csol[12];
      }
   if (printit) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
     
   ptr = n*npoint;
   if (!zgeom) ptr = ptr+2;
   ptx[n] = tminx-dx;
   pty[n] = (tminy+tmaxy)*0.5;
   ptx[n+1] = (tminx+tmaxx)*0.5;
   pty[n+1] = tmaxy+dx;
   ptx[n+2] = (tminx+tmaxx)*0.5;
   pty[n+2] = tminy-dx;
   ptx[n+3] = tmaxx+dx;
   pty[n+3] = (tminy+tmaxy)*0.5;
  
   fac = 0.5;         /* 0 for linear, 1.0 for cubic, 0.5 for 1/2 each */
   for (i=0;i<4;i++)
      {
      x = ptx[n+i];
      y = pty[n+i];
      optx[n+i] = csol[0]*x+csol[1]*y+csol[2]+fac*(csol[3]*x*y+
         csol[4]*x*x+csol[5]*y*y+csol[6]*x*x*x+csol[7]*x*x*y+
         csol[8]*x*y*y+csol[9]*y*y*y);
      if (!zgeom) opty[n+i] = csol[10]*x+csol[11]*y+csol[12]+fac*(csol[13]*x*y+
         csol[14]*x*x+csol[15]*y*y+csol[16]*x*x*x+csol[17]*x*x*y+
         csol[18]*x*y*y+csol[19]*y*y*y);
      }
   n += 4;

   /* ready for the big triangulation routine, con1,con2,con3 are
      mallocked in the subroutine (type is **) */
      
   if (gridnah==0)
      {
      printf("no grid detected\n");
      if (abendg&&n>1500) zmabend("user abend on no grid found");
      skinny = 0.0;
      thiessen(n,&nlret,reject,skinny,abendl,ptx,pty,&ntri,
			  &con1,&con2,&con3);
      }
   else
      {
      printf("grid automatically detected\n");
      /* this local routine substituted for library routine alz 11/23/10 */
      tgridxxx(n,&nlret,ptx,pty,&ntri,&con1,&con2,&con3,gridnah,csol,
         optx,opty,zgeom);
      }
   
   if (printit)
      {
      zprnt(4,1,&n,"nodes.");
      zprnt(4,1,&nlret,"edges.");
      zprnt(4,1,&ntri,"triangles.");
      }
   
   if (!plot)
      {
      /* solve triangles */
         
      mz_alloc2((unsigned char ***)&coeff,nrank,ntri,8);
      for (ix=0;ix<2;ix++)
         {
         if (zgeom&&ix>0) break;
         for (itri=0;itri<ntri;itri++)
	    {
	    work[0] = ptx[con1[itri]-1];
	    work[1] = ptx[con2[itri]-1];
	    work[2] = ptx[con3[itri]-1];
	    work[3] = pty[con1[itri]-1];
	    work[4] = pty[con2[itri]-1];
	    work[5] = pty[con3[itri]-1];
	    work[6] = 1.;
	    work[7] = 1.;
	    work[8] = 1.;
	    if (ix==0)
	       {
	       tab[0] = optx[con1[itri]-1];
	       tab[1] = optx[con2[itri]-1];
	       tab[2] = optx[con3[itri]-1];
	       }
	    else
	       {
	       tab[0] = opty[con1[itri]-1];
	       tab[1] = opty[con2[itri]-1];
	       tab[2] = opty[con3[itri]-1];
	       }
	    dgauss(work,tab,3,1.e-14,&ier);
	    for (j=0;j<3;j++) coeff[j+ix*3][itri] = tab[j];
	    if (ier!=0) coeff[0][itri] = 1.0E35;
	    }
	 }
         
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* start the geom of the grid, geomv goes to ibis file */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      tri = 0;
      ttrj1 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            isign = -1;
            found = 0;
            ttri = tri+ntri;
               
            for (k=0;k<ntri;k++)
               {
               tri = (ttri+((k+1)/2)*isign)%ntri;
               isign = -isign;
               if (coeff[0][tri]>1.0E34) continue;
               if (insidetri(x,y,
                       ptx[con1[tri]-1],pty[con1[tri]-1],
                       ptx[con2[tri]-1],pty[con2[tri]-1],
                       ptx[con3[tri]-1],pty[con3[tri]-1]))
                  {
                  found = 1;
                  break;
                  }
               }
            if (!found)
               {
               tri = ttri-ntri;
               if (j==1) tri = ttrj1;
               if (printit)
                 printf("grid point %f,%f not in triangle\n",x,y);
               }
            if (j==1) ttrj1 = tri;
            
            xx = coeff[0][tri]*x+coeff[1][tri]*y+coeff[2][tri];
            if (!zgeom) yy = coeff[3][tri]*x+coeff[4][tri]*y+coeff[5][tri];
            
            if (geomv)
               {
               vout[0][ptr] = x+normlz[0];
               vout[1][ptr] = y+normlz[1];
               vout[2][ptr] = xx+normlz[2];
               vout[3][ptr++] = yy+normlz[3];
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x+normlz[0];
               rout[ptr2+1] = y+normlz[1];
               rout[ptr2+2] = xx+normlz[2];
               rout[ptr2+3] = yy+normlz[3];
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x+normlz[0];
                  rout[ptr2+1] = y+normlz[1];
                  rout[ptr2+2] = xx+normlz[2];
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx+normlz[2];
                  rout[ptr2+1] = yy+normlz[3];
                  rout[ptr2+2] = x+normlz[0];
                  rout[ptr2+3] = y+normlz[1];
                  ptr2 += 4;
                  }
               }
            }
         }
   
   /* the next section is for output of polynomial fit cases */
   
   polyout:
   if (polynom)
      {
      if (printit)
         {
         printf("x' lsq fit, coeff of x = %15.8f\n",csol[0]);
         printf("x' lsq fit, coeff of y = %15.8f\n",csol[1]);
         printf("x' lsq fit, coeff of 1 = %15.8f\n",csol[2]);
         printf("x' lsq fit, coeff of xy = %15.8f\n",csol[3]);
         printf("x' lsq fit, coeff of xx = %15.8f\n",csol[4]);
         printf("x' lsq fit, coeff of yy = %15.8f\n",csol[5]);
         printf("x' lsq fit, coeff of xxx = %15.8f\n",csol[6]);
         printf("x' lsq fit, coeff of xxy = %15.8f\n",csol[7]);
         printf("x' lsq fit, coeff of xyy = %15.8f\n",csol[8]);
         printf("x' lsq fit, coeff of yyy = %15.8f\n",csol[9]);
         
         if (!zgeom)
            {
            printf("y' lsq fit, coeff of x = %15.8f\n",csol[10]);
            printf("y' lsq fit, coeff of y = %15.8f\n",csol[11]);
            printf("y' lsq fit, coeff of 1 = %15.8f\n",csol[12]);
            printf("y' lsq fit, coeff of xy = %15.8f\n",csol[13]);
            printf("y' lsq fit, coeff of xx = %15.8f\n",csol[14]);
            printf("y' lsq fit, coeff of yy = %15.8f\n",csol[15]);
            printf("y' lsq fit, coeff of xxx = %15.8f\n",csol[16]);
            printf("y' lsq fit, coeff of xxy = %15.8f\n",csol[17]);
            printf("y' lsq fit, coeff of xyy = %15.8f\n",csol[18]);
            printf("y' lsq fit, coeff of yyy = %15.8f\n",csol[19]);
            }
	 printf("normlz[0] = %15.8f\n", normlz[0]);
	 printf("normlz[1] = %15.8f\n", normlz[1]);
	 printf("normlz[2] = %15.8f\n", normlz[2]);
	 printf("normlz[3] = %15.8f\n", normlz[3]);
         zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
         }
      
      for (i=0;i<nklsq;i++)
         {
         if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
         elsqxy[i] = elsqxy[i]
            -csol[0]*ptx[k]
            -csol[1]*pty[k]
            -csol[2]
            -csol[3]*ptx[k]*pty[k]
            -csol[4]*ptx[k]*ptx[k]
            -csol[5]*pty[k]*pty[k]
            -csol[6]*ptx[k]*ptx[k]*ptx[k]
            -csol[7]*ptx[k]*ptx[k]*pty[k]
            -csol[8]*ptx[k]*pty[k]*pty[k]
            -csol[9]*pty[k]*pty[k]*pty[k];
         if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]
            -csol[10]*ptx[k]
            -csol[11]*pty[k]
            -csol[12]
            -csol[13]*ptx[k]*pty[k]
            -csol[14]*ptx[k]*ptx[k]
            -csol[15]*pty[k]*pty[k]
            -csol[16]*ptx[k]*ptx[k]*ptx[k]
            -csol[17]*ptx[k]*ptx[k]*pty[k]
            -csol[18]*ptx[k]*pty[k]*pty[k]
            -csol[19]*pty[k]*pty[k]*pty[k];
         }
      if (printit) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
      
      ntri = ntri-nerr;
      nav1 = nav+1;
      nah1 = nah+1;
      dx = (maxx-minx)/(double)nav;
      dy = (maxy-miny)/(double)nah;
        
      /* like above except use surface fit for all points */
      
      if (geomv) mz_alloc2((unsigned char ***)&vout,4,nah1*nav1,8);
      else if (zgeom) mz_alloc1((unsigned char **)&rout,3*nah1*nav1,4);
      else mz_alloc1((unsigned char **)&rout,4*nah1*nav1,4);
      
      ptr = 0;
      ptr2 = 0;
      for (i=0;i<nav1;i++)
         {
         x = minx+(double)i*dx;
         if (lgeom) x = (int)x;
         for (j=0;j<nah1;j++)
            {
            y = miny+(double)j*dy;
            if (lgeom) y = (int)y;
            xx = csol[0]*x
                +csol[1]*y
                +csol[2]
                +csol[3]*x*y
                +csol[4]*x*x
                +csol[5]*y*y
                +csol[6]*x*x*x
                +csol[7]*x*x*y
                +csol[8]*x*y*y
                +csol[9]*y*y*y;
            if (!zgeom) yy = csol[10]*x
                +csol[11]*y
                +csol[12]
                +csol[13]*x*y
                +csol[14]*x*x
                +csol[15]*y*y
                +csol[16]*x*x*x
                +csol[17]*x*x*y
                +csol[18]*x*y*y
                +csol[19]*y*y*y;
                
            if (geomv)
               {
               vout[0][ptr] = x+normlz[0];
               vout[1][ptr] = y+normlz[1];
               vout[2][ptr] = xx+normlz[2];
               vout[3][ptr++] = yy+normlz[3];
               }
            else if (lgeomlike)
               {
               rout[ptr2] = x+normlz[0];
               rout[ptr2+1] = y+normlz[1];
               rout[ptr2+2] = xx+normlz[2];
               rout[ptr2+3] = yy+normlz[3];
               ptr2 += 4;
               }
            else
               {
               if (zgeom)
                  {
                  rout[ptr2] = x+normlz[0];
                  rout[ptr2+1] = y+normlz[1];
                  rout[ptr2+2] = xx+normlz[2];
                  ptr2 += 3;
                  }
               else
                  {
                  rout[ptr2] = xx+normlz[2];
                  rout[ptr2+1] = yy+normlz[3];
                  rout[ptr2+2] = x+normlz[0];
                  rout[ptr2+3] = y+normlz[1];
                  ptr2 += 4;
                  }
               }
            }
         }
      }
      
      /* Output array to IBIS file in col_ordr, or tiepoints parm file */
      
      if (geomv)
         {
         clen = (nah+1)*(nav+1);
         status = zvunit(&ibisOut,"out",1, NULL);
         status = IBISFileUnit(ibisOut,&ibis,"write",4,clen,0,"column");
         status = IBISFileSet(ibis,"fmt_default","doub",0);
         status = IBISFileUnitOpen(ibis);
         /*status = IBISFileOpen(ibisOut,&ibis,"write",4,clen,0,0);*/
        
         for (i=0;i<npoint;i++)
	    {
	    status = IBISColumnWrite(ibis,(char*)(vout[i]),i+1,1,clen);
	    if (status!=1) IBISSignal(ibis,status,0);
	    }
	 status = IBISFileClose(ibis,0);
         return;
         }
      else
         {
         status = zvunit(&parmOut,"out",1, NULL);
         status = zvparm("out",outnam,&nout,&outdf,1,0);
         status = zvpopen(outnam,"SA",&tiept);
         status = zvpout("NAH",&nah,"INT",1,0);
         status = zvpout("NAV",&nav,"INT",1,0);
         status = zvpout("TIEPOINT",rout,"REAL",ptr2,0);
         status = zvpclose();
         return;
         }
      } 
   else    /* new plot option */
      {
      mz_alloc2((unsigned char ***)&pout,2,ntri*5,4);
      
      p = 0;
      for (i=0;i<ntri;i++)
         {
         for (j=0;j<5;j++)
            {
            for (k=0;k<2;k++)
               {
               switch (j+k*5)
                  {
                  case 0: pout[k][p] = ptx[con1[i]-1]+normlz[0]; break;
                  case 1: pout[k][p] = ptx[con2[i]-1]+normlz[0]; break;
                  case 2: pout[k][p] = ptx[con3[i]-1]+normlz[0]; break;
                  case 3: pout[k][p] = ptx[con1[i]-1]+normlz[0]; break;
                  
                  case 5: pout[k][p] = pty[con1[i]-1]+normlz[1]; break;
                  case 6: pout[k][p] = pty[con2[i]-1]+normlz[1]; break;
                  case 7: pout[k][p] = pty[con3[i]-1]+normlz[1]; break;
                  case 8: pout[k][p] = pty[con1[i]-1]+normlz[1]; break;
                  
                  default: pout[k][p] = 0.0;
                  }
               }
            p++;
            }
         }
      clen = ntri*5;
      
      status = zvunit(&ibisOut,"out",1, NULL);
      status = IBISFileOpen(ibisOut,&ibis,"write",2,clen,0,0);
      for (i=0;i<2;i++)
	 {
	 status = IBISColumnWrite(ibis,(char*)(pout[i]),i+1,1,clen);
	 if (status!=1) IBISSignal(ibis,status,0);
	 }
      status = IBISFileClose(ibis,0);
      return;
      }
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create tieconv.imake
#define  PROGRAM   tieconv

#define MODULE_LIST tieconv.c 

#define MAIN_LANG_C
#define R2LIB 

#define USES_C

#define LIB_P2SUB
#define LIB_TAE
#define LIB_RTL

/*#define LIB_LOCAL	/* comment out on delivery */
/*#define DEBUG		/* comment out on delivery */
$ Return
$!#############################################################################
$PDF_File:
$ create tieconv.pdf
PROCESS       HELP=*
PARM INP      TYPE=(STRING,99) COUNT=(0:1) DEFAULT=--
PARM OUT      TYPE=(STRING,99)
PARM TIEPOINT TYPE=REAL  COUNT=(1:400) DEFAULT=0.
PARM NAH      TYPE=INTEGER COUNT=1 DEFAULT=0
PARM NAV      TYPE=INTEGER COUNT=1 DEFAULT=0
PARM MODE     TYPE=KEYWORD VALID=(LGEOM,GEOMA,MGEOM,GEOMV,GEOMZ)
PARM COLS     TYPE=INTEGER COUNT=3:4 DEFAULT=(1,2,3,4)
PARM PRINT    TYPE=KEYWORD COUNT=(0:1) VALID=PRINT DEFAULT=--
PARM PLOT     TYPE=KEYWORD COUNT=(0:1) VALID=PLOT    DEFAULT=--
PARM ABEND    TYPE=KEYWORD COUNT=(0:1) VALID=ABEND   DEFAULT=--
PARM ABENDG   TYPE=KEYWORD COUNT=(0:1) VALID=ABENDG  DEFAULT=--
PARM POLY     TYPE=(STRING,9) COUNT=(0:1) +
              VALID=("","LINEAR","KEYSTONE","QUAD","CUBIC")  DEFAULT=""
PARM MINS     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MAXS     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MINL     TYPE=REAL COUNT=1 DEFAULT=0.
PARM MAXL     TYPE=REAL COUNT=1 DEFAULT=0.
PARM REJECT   TYPE=REAL COUNT=1 DEFAULT=.01
PARM GRIDTOL  TYPE=REAL COUNT=1 DEFAULT=.99
PARM PARMS    TYPE=(STRING,99) COUNT=(0:1) DEFAULT=--
END-PROC
.TITLE
VICAR Program tieconv
.HELP
PURPOSE

     TIECONV prepares a gridded dataset for POLYGEOM, GEOMA, 
     LGEOM,  MGEOM,  GEOMV or  GEOMZ  transformations. Input
     is paired sets of tiepoints with no restrictions. It is
     in principle, a surface generation routine, but creates 
     the  gridded  dataset so as to best interface with  the 
     VICAR routines above.   The sequence GEN,  TIECONV, and 
     GEOMZ can be used to generate a surface in image format 
     through an arbitrary set of points.
     TIECONV uses the finite element method  (triangulation) 
     for  surface  fitting.   It is anticipated  that  other 
     surface  fitting methods will be integrated into  VICAR 
     in  the same fashion as tieconv so that users will have 
     maximum flexibility both in terms of choice of  surface 
     fit and in terms on application.
     The triangulation method is Thiessen triangulation. The
     maximum number of points is  probably  about 1 million.
     The largest case  tried so far is 400,000 points, which
     ran in 4 hours 31 minutes on a SUN SPARCstation 20.
     
     TIECONV can be used to prepare a polynomial surface fit
     of four types.  The keyword is POLY  and  it  has  five
     values: "LINEAR","KEYSTONE","QUAD","CUBIC", (or ""  for
     triangulation).  LINEAR fits the  best plane (in the L2
     norm or least squares) through the x-distortion and the
     y-distortion.   KEYSTONE fits a  bilinear surface, QUAD
     fits a general quadratic surface and CUBIC fits  a gen-
     eral cubic surface.  The  number of  tiepoints required
     are LINEAR - 3, KEYSTONE - 4, QUAD - 6, and CUBIC - 10.
     More tiepoints  are handled  by a least squares fit.
     
     
.PAGE
TAE COMMAND LINE FORMAT

     tieconv OUT=B PARAMS
     tieconv PARMS=parm_file OUT=B PARAMS
     tieconv INP=tiep  OUT=B  PARAMS

     where

     parm_file           is an optional disk parameter dataset 
			   containing the input tiepoints.
     tiep		 is an optional IBIS tabular file containing
			   the input tiepoints.
     B                   is the output parameter dataset.
     PARAMS              is a standard VICAR parameter field.
.PAGE
OPERATION

     tieconv operates in two phases.   In phase 1, the input 
     points  are  fully  triangulated  by  a version  of the 
     Thiessen   algorithm.   This operates by  computing the
     Voronoi polygons (polygons of area  nearest each point)
     and  computing  the  triangles  formed by the bisectors
     of the  polygons.   Four extra points  are  added  five 
     diameters away from the convex hull so that the surface 
     will extend smoothly beyond the input tiepoints.

     In  phase 2,  the output grid is formed by  evaluating 
     the  triangular surface at grid point locations.   That 
     is,  a grid point will fall in some triangle,  and  the 
     GEOM  shift  will  be the linear interpolation  of  the 
     input shifts at the three corners of the triangle.  The 
     user  should  note  that  the  triangular  surface   is 
     continuous but not differentiable and it passes through 
     all  of  the input  points.   Point-surface  generation 
     routines can e compared in the following table.
.PAGE
|       \ PROPERTIES| CONTI-|DIFFEREN-| EVALUATES |   WELL   |
|        \    OF    | NUOUS |TIABLE   | AT INPUT  |  BEHAVE  |
| METHOD  \ SURFACE |       |         |   POINT   | NO MESAS |
|-------------------|-------|---------|-----------|----------|
| Triangulation     |  yes  |    no   |    yes    |   yes    |
|-------------------|-------|---------|-----------|----------|
|                -1 |       |         |           |          |
| Interpolation r   |   no  |    no   |    yes    |   yes    |
|-------------------|-------|---------|-----------|----------|
|                -p |       |         |           |          |
| Interpolation r   |   no  |    no   |    yes    |    no    |
|-------------------|-------|---------|-----------|----------|
| Polynomial Fit    |  yes  |    yes  |    no     |    no    |
|-------------------|-------|---------|-----------|----------|
| Linear            |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|
| Bilin (keystone)  |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|
| Quadratic         |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|
| Cubic             |  yes  |    yes  |    no     |   yes    |
|-------------------|-------|---------|-----------|----------|

.PAGE
EXAMPLE

     tieconv OUT=B 'GEOMA NAH=44 NAV=24
           TIEPOINT=(   346       432       353      422
                        479       316       482      313
                         .
                         .
                         .
                        723       529       715      527)
     POLYGEOM INP=X PARMS=B OUT=Y

     In this example,  the tiepoints are used to set up a 44 
     x  24 grid for use by POLYGEOM.   The tiepoints can  be 
     scattered  over  the  image  in  any  fashion   whereas 
     POLYGEOM requires a regular grid.
     
     TIECONV can be used to prepare a polynomial surface fit
     of four types.  The keyword is POLY  and  it  has  five
     values: "LINEAR","KEYSTONE","QUAD","CUBIC", (or ""  for
     triangulation).  LINEAR fits the  best plane (in the L2
     norm or least squares) through the x-distortion and the
     y-distortion.   KEYSTONE fits a  bilinear surface, QUAD
     fits a general quadratic surface and CUBIC fits  a gen-
     eral cubic surface.  The  number of  tiepoints required
     are LINEAR - 3, KEYSTONE - 4, QUAD - 6, and CUBIC - 10.
     More tiepoints  are handled  by a least squares fit.
     
     
     Back to the  triangulation case, if the  input data set
     is a  grid, or  approximately  a  grid, a  shortcut  is
     applied that  speeds up  enormously.  The detection  of
     the grid is automatic.
     
.PAGE

     GEN OUT=X NL=1000 NS=1000 IVAL=0 LINC=0 SINC=0
     tieconv OUT=B 'GEOMZ+
             TIEPOINT=(
               1   1    0
               1000   1    0
               1    1000     0
               1000  1000      0
               500   500       255)
     GEOMZ INP=X PARMS=B OUT=Y

     this  example constructs a "pyramid" shaped  brightness 
     surface in the image Y.
.PAGE

TIMING

     Timing  is dominated by the triangulation method  which 
     is 0(n*log(n)) where n is the number of input points.  A case 
     with  10000  points  was run in 1.63 minutes  CPU  time, 
     and a case with 400,000 points was run in 4.52 hours on
     a SPARCstation 20.
     
     If the input data set is a grid, or approximately a grid,
     a shortcut is applied that speeds up enormously.  The
     detection of the grid is automatic.
     
     The polynomial fits (LINEAR, KEYSTONE, QUAD, CUBIC) are
     very fast.
     
RESTRICTIONS

   The maximum number of input tiepoints is probably about 1 million.
   This value will increase as the machines get more virtual and
   real memory since dynamic memory allocation is used throughout
   the algorithm.
   
   The maximum number of output tiepoints is limited by IBIS table
   size (currently about 10 million?).  Internal to the program,
   dynamic memory allocation is used.


.PAGE
WRITTEN BY:            A. L. Zobrist, 29 August 1979

COGNIZANT PROGRAMMER:  A. L. Zobrist

REVISIONS: 
  PORTED TO UNIX	C. R. Schenk (CRI)  31-Oct 1994
  ALGORITHM UPDATED     A. L. Zobrist 19-Jul 1999
  lsq update for poly   A. L. Zobrist 27-Aug 2002
Fri Jan 11 2008 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup  

.LEVEL1
.VARIABLE INP
Input IBIS tabular file
.VARIABLE COLS
Columns to use from
IBIS file.
.VARIABLE OUT
Output dataset (type depends
on other parameters, see
detailed help)
.VARIABLE PARMS
Input parameter dataset
.VARIABLE TIEPOINT
Specify tiepoint pairs 
.VARIABLE NAH
Number of grid cells horizontal
.VARIABLE NAV
Number of grid cells vertical
.VARIABLE MINL
Bounds of the output grid
.VARIABLE MINS
Bounds of the output grid
.VARIABLE MAXL
Bounds of the output grid
.VARIABLE MAXS
Bounds of the output grid
.VARIABLE REJECT
Radius for duplicate  points
.VARIABLE MODE
GEOMA for GEOMA or POLYGEOM use 
GEOMZ for GEOMZ  use
LGEOM for LGEOM  use 
MGEOM for MGEOM  use
GEOMV for GEOMV  use
.VARIABLE PLOT
Gen plot file of triangulation
.VARIABLE PRINT
Keyword to print data values
.VARIABLE ABEND
ABEND abend if duplicate points
.VARIABLE ABENDG
ABENDG abend if not a grid
.VARIABLE POLY
forget triangulation and do
Polynomial fit (see help 2)
.VARIABLE GRIDTOL
Use to make grid-finding more
or less tolerant
.LEVEL2
.VARIABLE INP
       INP=A		 Input IBIS tabular file containing the
			 input tiepoints.  If INP is specified
			 then the tiepoints will be taken from
			 the IBIS interface file rather than the
			 TIEPOINT parameter or the parameter
			 data set.  
.VARIABLE COLS
    COLS=(C1,C2,C3,C4)   Columns in the IBIS tabular file that
			 contain the tiepoints.  C1 has new line,
			 C2 has new sample, C3 has old line, and
			 C4 has old sample.

.VARIABLE OUT
       OUT=B             Output parameter data set containing
			 gridded tiepoints suitable for the
			 GEOM programs.  If the GEOMV keyword
			 is used then this is in an IBIS file.
			 If the plot option is chosen, then
			 this is a plot file.
.VARIABLE PARMS
       PARMS=parm_file   Optional parameter data set created
                         by routine XVPOUT. This data set con-
                         tains keywords and data for TIEPOINT
                         NAH and NAV and can be used instead
                         of specifying these keywords in the
                         TAE COMMAND LINE.
.VARIABLE TIEPOINT
     TIEPOINT=(NL1,NS1,  these  specify  the input  tiepoint 
       OL1,OS1, . . .,   pairs   for   GEOM    applications.  
       NLk,NSk,OLk,OSk)  Maximum k is 100 (due to TAE).

     TIEPOINT=(NL1,NS1,  this  form  of parameter  specifies 
       DZ1, . . .,NLk,   the input tiepoint pairs for  GEOMZ 
       NSk, DZk)         applications   or   image   surface 
                         generation.   Maximum k is 133 (due 
                         to TAE).
.VARIABLE NAH
     NAH=n               the  integer n specifies the number 
                         of  grid cells horizontally in  the 
                         output  grid (default is 30  except 
                         in the case of LGEOM which is 10).
.VARIABLE NAV
     NAV=m               the integer m specifies the  number 
                         of  grid  cells vertically  in  the 
                         output  grid (default is 30  except 
                         in the case of LGEOM which is 10).

.VARIABLE MINL
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MINS
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MAXL
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE MAXS
     MINL=w              the integers w,  x, y, z define the 
     MINS=x              lower   and  upper  bounds  of  the 
     MAXL=y              output  grid in terms of  line  and 
     MAXS=z              sample.  The default is to make the 
                         grid  exactly  contain  the  convex 
                         hull of the input tiepoints.

.VARIABLE REJECT
     REJECT=r            the    floating   point   value   r 
                         specifies  a  radius  within  which 
                         separate points will be  considered 
                         as  duplicate  points  (default  is 
                         .01).

.VARIABLE MODE
     GEOMV               this  keyword  specifies  that  the 
                         output  dataset is to be  formatted 
                         for  GEOMV  use.   The output  disk
                         dataset will be an IBIS file.  Note
                         that GEOMV no longer  requires  the
                         NAH or NAV keywords.

     GEOMA               this  keyword  specifies  that  the 
                         output  dataset is to be  formatted 
                         for  GEOMA or  POLYGEOM  use.   The 
                         output  disk  dataset will  contain 
                         the   proper  GEOMA   or   POLYGEOM 
                         keywords  and  format  so  that  no 
                         addition   parameters  need  to  be 
                         specified unless desired.

     GEOMZ               this  keyword  specifies  that  the 
                         output is to be formatted for GEOMZ 
                         use.   The output disk dataset will 
                         contain  the proper GEOMZ  keywords 
                         and  format so that  no  additional 
                         parameters  need  to  be  specified 
                         unless desired.

     LGEOM               this  keyword  specifies  that  the 
                         output is to be formatted for LGEOM 
                         use.   The output disk dataset will 
                         contain  the proper LGEOM  keywords 
                         and  format so that  no  additional 
                         parameters  need  to  be  specified 
                         unless   desired.    The  user   is 
                         cautioned    to    observe    LGEOM 
                         application size limitations.

     MGEOM               this  keyword  specifies  that  the 
                         output is to be formatted for MGEOM 
                         use.   The output disk dataset will 
                         contain  proper MGEOM keywords  and 
                         format   so   that  no   additional 
                         keywords   need  to  be   specified 
                         unless desired.
.VARIABLE ABEND
    ABEND=ABEND          specifies  that the routine  should 
    or 'ABEND            abend   if  duplicate  points   are 
                         found.
.VARIABLE ABENDG
    ABENDG=ABENDG        specifies  that the routine  should 
    or 'ABENDG           abend   if  the grid finding routine
                         fails to find a grid.
.VARIABLE POLY
    POLY=LINEAR     LINEAR fits a linear surface through
                    the x-distortion and the y-distortion
                    using least squares.  This requires at
                    least three input data points.  More
                    tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.

   POLY=KEYSTONE    KEYSTONE fits a bilinear surface through
                    the x-distortion and the y-distortion
                    using least squares.  This requires at
                    least four input data points.  More
                    tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.

   POLY=QUAD        TIECONV can be used to prepare various
                    polynomial fits.  For a general quadratic
                    fit of the form x' = ax^2+bxy+cy^2+dx+ey+f
                    y' = gx^2+hxy+iy^2+jx+ky+l, use the 
                    keyword QUAD, which requires six tiepoints.
                    More tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.

   POLY=CUBIC       TIECONV can be used to prepare various
                    polynomial fits.  For a general cubic
                    fit of the form
                    x' = ax^3+bx^2y+cxy^2+dy^3+ex^2+fxy+gy^2
                         +hx+iy+j
                    y' = kx^3+lx^2y+mxy^2+ny^3+ox^2+pxy+qy^2
                         +rx+sy+t      use the
                    keyword CUBIC, which requires ten tiepoints.
                    More tiepoints will be handled with a least
                    squares fit and the residuals can be 
                    viewed.  All input output formats are 
                    valid with these options except for PLOT.
.VARIABLE PLOT
    PLOT=PLOT            bypasses tiepoint generation phase.
    or 'PLOT             creates graphics file of triangula-
                         tion for plotting.
.VARIABLE PRINT
    'PRINT		 Keyword to print informative data.
.VARIABLE GRIDTOL
If the input is a grid (and no poly keywords are used) the
program is sped up by a huge factor.  The grid is detected
automatically and has a tolerance for points being out of
regularity on the grid.  Each point is predicted by its two
previous neighbors and the distance from the predicted to
the actual is ratioed to the neighbor grid distance.  GRIDTOL
is the maximum ratio allowed.

Failure to find a grid due to this parameter causes the 
program to apply thiessen triangulation to the point set
which can be slower by a factor of hundreds.

You can be generous with this parameter, say 10 or 20, but
keep it smaller than the nah or nav by half.  The row or 
column that is found must match the length of the other rows
or columns and (nah+1)*(nav+1) must equal the total number of
points.
.END
$ Return
$!#############################################################################
$Test_File:
$ create tsttieconv.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"


!! Note to testers:
!!
!!  differences in "data" values of less than 0.1% between different
!!  platforms are due to roundoff and are not significant;
!!
!!  also, differences in "residuals" when less than about 1.0E-10
!!  are completely insignificant.
!!
!!  (lwk, 2002/03/28)


ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV  

! list the IBIS file
ibis-list b 'format

! testing the lgeom parameter output

tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'LGEOM  

! now test that the tiepoint data sets are OK in the geom programs

gen mgtest 10 10 SINC=40 LINC=40

geomv (mgtest,b) mgtest2 SIZE=(1,1,10,10)
list mgtest2

lgeom mgtest mgtest3 SIZE=(1,1,10,10) PARMS=b2
list mgtest3

difpic (mgtest2,mgtest3)

! now larger test case

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=3000
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=30,NAV=5,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV  

! list the IBIS file
ibis-list b 'format


! list out a plot file just for definite look at triangles

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=11
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'PLOT

! list the IBIS file
ibis-list b 'format

! test linear and keystone keywords

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=44
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")

tieconv INP=a COLS=(1,2,3,4)  poly=linear OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  poly=linear OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have near-zero residuals

ibis-list b 'format
ibis-list b2 'format

tieconv INP=a COLS=(1,2,3,4)  poly=keystone OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  poly=keystone OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have near-zero residuals

ibis-list b 'format
ibis-list b2 'format

tieconv INP=a COLS=(1,2,3,4)  poly=keystone OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 
tieconv INP=b COLS=(1,2,3,4)  poly=linear OUT=b2 +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 

! list the IBIS files, the second should have NONzero residuals

ibis-list b 'format
ibis-list b2 'format

!!  NOTE:  the GEOMZ option is not incuded in this proc, because
!!  it is still experimental code.  See Al Zobrist or the Cart lab
!!  for later versions.

end-proc
$!-----------------------------------------------------------------------------
$ create devtieconv.pdf
procedure
refgbl $echo
refgbl $autousage
parm version string def="ibis-1"
parm org string def="column"
body
!let _onfail="continue"
let $autousage="none"
let $echo="yes"


!!!!!!!!!!!! DEVELOPMENT CASES FOR ALZ, ALSO HAS A PLOT CASE
!!!!!!!!!!!! SEE THE TSTTIECONV.PDF FILE FOR THE REGRESSION TEST


!ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
!    nc=4 nr=44
!mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
!mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
!mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12 'format

!!datetime
!tieconv INP=a COLS=(1,2,3,4)  OUT=b +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'GEOMV 'NOPR 
!!datetime


! testing the lgeom parameter output

!tieconv INP=a COLS=(1,2,3,4)  OUT=b2 +
!      NAH=3,NAV=3,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
!    'LGEOM 'NOPR 

! now test that the tiepoint data sets are OK

!gen mgtest 10 10 SINC=40 LINC=40

!geomv (mgtest,b) mgtest2 SIZE=(1,1,10,10)
!list mgtest2

!lgeom mgtest mgtest3 SIZE=(1,1,10,10) PARMS=b2
!list mgtest3

!difpic (mgtest2,mgtest3)

! now large test cases with plot

ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=2000
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
!mf a func=("c1=c1-mod(c1,0.1)","c2=c2-mod(c2,0.1)")
mf a func=("c3=c1*1.1","c4=c2*1.1")
!ibis-list a cols=(1,2,3,4) csize=12 'format

datetime
tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=1,NAV=1,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'NOPR !'PLOT
datetime

!pltgraf inp=b xrange=(-20.0,120.0) yrange=(-20.0,120.0) xlen=10 ylen=10


! rectangular grid case, hard for voronoi algorithms, see Euler #


ibis-gen a version=&version org=&org  datacol=(1,2,3,4) +
    nc=4 nr=8281
mf a func=("c1=1.1*int((index-1)/91.0)","c2=1.1*mod(index-1,91)")
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")


tieconv INP=a COLS=(1,2,3,4)  OUT=b +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100. +
    'GEOMV 'NOPR 'plot

!pltgraf inp=b xrange=(-20.0,120.0) yrange=(-20.0,120.0) xlen=10 ylen=10

end-proc
$!-----------------------------------------------------------------------------
$ create tsttieconv.log_solos
tsttieconv
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=44
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        1.82        1.82
        1.00       15.14        2.03       17.58
        1.00       29.29        2.23       33.35
        1.00       43.43        2.34       49.01
        1.00       57.57        2.14       64.37
        1.00       71.71        2.15       79.93
        1.00       85.86        3.19       96.54
        1.00      100.00        4.06      112.96
       15.14        1.00       17.67        2.12
       15.14       15.14       17.89       17.89
       15.14       29.29       18.21       33.77
       15.14       43.43       18.44       49.56
       15.14       57.57       17.75       64.42
       15.14       71.71       18.70       80.93
       15.14       85.86       18.87       96.66
       15.14      100.00       19.78      113.12
       29.29        1.00       33.55        2.43
       29.29       15.14       33.82       18.26
       29.29       29.29       34.26       34.26
       29.29       43.43       35.20       50.76
       29.29       57.57       34.48       65.60
       29.29       71.71       34.30       80.97
       29.29       85.86       34.37       96.60
       29.29      100.00       35.38      113.17
       43.43        1.00       49.80        3.13
       43.43       15.14       48.78       17.66
       43.43       29.29       51.12       35.56
       43.43       43.43       51.31       51.31
       43.43       57.57       51.51       67.06
       43.43       71.71       48.97       80.08
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.60       97.28
       43.43      100.00       50.75      112.98
       57.57        1.00       66.51        4.28
       57.57       15.14       65.57       18.90
       57.57       29.29       65.93       34.81
       57.57       43.43       64.39       48.83
       57.57       57.57       65.49       65.49
       57.57       71.71       64.99       80.54
       57.57       85.86       65.08       96.20
       57.57      100.00       66.09      112.76
       71.71        1.00       82.89        5.11
       71.71       15.14       82.56       20.33
       71.71       29.29       80.05       33.38
       71.71       43.43       80.89       49.78
       71.71       57.57       80.46       64.90
       71.71       71.71       81.74       81.74
       71.71       85.86       81.58       97.14
       71.71      100.00       81.43      112.55
       85.86        1.00       98.56        5.22
       85.86       15.14       98.29       20.50
       85.86       29.29       97.32       35.10
       85.86       43.43       96.17       49.50
       85.86       57.57       97.07       65.96
       85.86       71.71       97.90       82.34
       85.86       85.86       96.79       96.79
       85.86      100.00       96.78      112.34
      100.00        1.00      114.23        5.33
      100.00       15.14      114.01       20.67
      100.00       29.29      113.05       35.26
      100.00       43.43      112.08       49.86
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      111.12       64.45
      100.00       71.71      112.20       81.08
      100.00       85.86      113.72       98.16
      100.00      100.00      112.50      112.50
tieconv INP=a COLS=(1,2,3,4)  OUT=b2  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'LGEOM
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
gen mgtest 10 10 SINC=40 LINC=40
Beginning VICAR task gen
GEN Version 6
GEN task completed
geomv (mgtest,b) mgtest2 SIZE=(1,1,10,10)
Beginning VICAR task geomv
geomv version Tue Nov 11 2008
list mgtest2
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:40:54 2011
 Task:GEOMV     User:lwk       Date_Time:Sun Dec 11 11:40:54 2011
     Samp     1       3       5       7       9
   Line
      1      80 111 156 201 184  52  81 126   0   0
      2     111 156 202 200  36  81 126 171   0   0
      3     157 202 203  36  82 127 172 189   0   0
      4     203 197  37  82 127 173 178  64   0   0
      5     178  38  83 128 173 172  71  53   0   0
      6      49  83 128 174 171  72  53  98   0   0
      7      84 129 174 175  69  54  99 144   0   0
      8     129 175 185  60  54  99 144 190   0   0
lgeom mgtest mgtest3 SIZE=(1,1,10,10) PARMS=b2
Beginning VICAR task lgeom
list mgtest3
Beginning VICAR task list

   BYTE     samples are interpreted as   BYTE   data
 Task:GEN       User:lwk       Date_Time:Sun Dec 11 11:40:54 2011
 Task:LGEOM     User:lwk       Date_Time:Sun Dec 11 11:40:55 2011
     Samp     1       3       5       7       9
   Line
      1      66 111 156 201 184  52  81 126   0   0
      2     111 156 202 200  36  81 126 171   0   0
      3     157 202 203  37  82 127 172 189   0   0
      4     203 197  37  82 127 173 178  64   0   0
      5     178  38  83 128 173 172  71  53   0   0
      6      49  83 128 174 171  72  53  98   0   0
      7      84 129 174 175  68  54  99 144   0   0
      8     129 175 185  60  54  99 145 190   0   0
difpic (mgtest2,mgtest3)
Beginning VICAR task difpic
DIFPIC version 06Oct11
 NUMBER OF DIFFERENT PIXELS =   4
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=3000
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=30,NAV=5,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:186  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00      126.36      126.36
        1.00        4.30      165.81      169.44
        1.00        7.60      125.18      132.44
        1.00       10.90      103.16      114.05
        1.00       14.20      173.49      188.01
        1.00       17.50      108.32      126.47
        1.00       20.80      244.86      266.64
        1.00       24.10      114.12      139.53
        1.00       27.40      189.86      218.90
        1.00       30.70      257.37      290.04
        1.00       34.00      246.12      282.42
        1.00       37.30      204.38      244.31
        1.00       40.60       27.28       70.84
        1.00       43.90      198.90      246.09
        1.00       47.20      191.78      242.60
        1.00       50.50      128.65      183.10
        1.00       53.80      115.95      174.03
        1.00       57.10       36.62       98.33
        1.00       60.40      153.14      218.48
        1.00       63.70      195.57      264.54
        1.00       67.00      139.65      212.25
        1.00       70.30      200.99      277.22
        1.00       73.60      206.17      286.03
        1.00       76.90      159.77      243.26
        1.00       80.20      139.48      226.60
        1.00       83.50       49.85      140.60
        1.00       86.80       88.91      183.29
        1.00       90.10      223.98      321.99
        1.00       93.40      210.61      312.25
        1.00       96.70      246.51      351.78
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00      100.00      219.59      328.49
       20.80        1.00       86.12       64.34
       20.80        4.30      225.22      207.07
       20.80        7.60      195.75      181.23
       20.80       10.90      137.35      126.46
       20.80       14.20      225.98      218.72
       20.80       17.50      234.90      231.27
       20.80       20.80      208.63      208.63
       20.80       24.10      224.65      228.28
       20.80       27.40      287.09      294.35
       20.80       30.70      242.77      253.66
       20.80       34.00      167.52      182.04
       20.80       37.30      133.52      151.67
       20.80       40.60      211.10      232.88
       20.80       43.90       71.40       96.81
       20.80       47.20       98.27      127.31
       20.80       50.50      267.56      300.23
       20.80       53.80      176.62      212.92
       20.80       57.10      255.32      295.25
       20.80       60.40      260.29      303.85
       20.80       63.70      190.69      237.88
       20.80       67.00      185.51      236.33
       20.80       70.30      234.70      289.15
       20.80       73.60      255.29      313.37
       20.80       76.90       95.54      157.25
       20.80       80.20       97.81      163.15
       20.80       83.50      197.71      266.68
       20.80       86.80      237.56      310.16
       20.80       90.10      233.43      309.66
       20.80       93.40      158.18      238.04
 
Rows: 61:90
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       20.80       96.70      109.53      193.02
       20.80      100.00      260.33      347.45
       40.60        1.00      165.41      121.85
       40.60        4.30      207.04      167.11
       40.60        7.60      115.97       79.67
       40.60       10.90      172.81      140.14
       40.60       14.20      235.09      206.05
       40.60       17.50      254.89      229.48
       40.60       20.80      218.80      197.02
       40.60       24.10      101.17       83.02
       40.60       27.40      154.82      140.30
       40.60       30.70      184.82      173.93
       40.60       34.00      171.54      164.28
       40.60       37.30      198.84      195.21
       40.60       40.60      178.12      178.12
       40.60       43.90      165.52      169.15
       40.60       47.20      123.93      131.19
       40.60       50.50      266.44      277.33
       40.60       53.80      269.61      284.13
       40.60       57.10      258.04      276.19
       40.60       60.40      316.92      338.70
       40.60       63.70      165.80      191.21
       40.60       67.00      234.36      263.40
       40.60       70.30      215.90      248.57
       40.60       73.60      253.42      289.72
       40.60       76.90      229.25      269.18
       40.60       80.20      211.50      255.06
       40.60       83.50      156.12      203.31
       40.60       86.80      162.03      212.85
       40.60       90.10      219.57      274.02
 
Rows: 91:120
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       40.60       93.40      190.97      249.05
       40.60       96.70      166.43      228.14
       40.60      100.00      223.88      289.22
       60.40        1.00      118.40       53.06
       60.40        4.30      199.99      138.28
       60.40        7.60      188.72      130.64
       60.40       10.90      319.23      264.78
       60.40       14.20      178.35      127.53
       60.40       17.50      292.64      245.45
       60.40       20.80      150.65      107.09
       60.40       24.10      252.15      212.22
       60.40       27.40      303.25      266.95
       60.40       30.70      126.38       93.71
       60.40       34.00      201.05      172.01
       60.40       37.30      309.41      284.00
       60.40       40.60      296.33      274.55
       60.40       43.90      230.39      212.24
       60.40       47.20      168.49      153.97
       60.40       50.50      281.48      270.59
       60.40       53.80      246.90      239.64
       60.40       57.10      164.89      161.26
       60.40       60.40      167.94      167.94
       60.40       63.70      250.66      254.29
       60.40       67.00      222.47      229.73
       60.40       70.30      175.95      186.84
       60.40       73.60      115.05      129.57
       60.40       76.90      228.26      246.41
       60.40       80.20      173.21      194.99
       60.40       83.50      308.30      333.71
       60.40       86.80      270.05      299.09
 
Rows: 121:150
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       60.40       90.10      238.55      271.22
       60.40       93.40      273.58      309.88
       60.40       96.70      170.24      210.17
       60.40      100.00      141.15      184.71
       80.20        1.00      187.51      100.39
       80.20        4.30      285.74      202.25
       80.20        7.60      348.09      268.23
       80.20       10.90      237.04      160.81
       80.20       14.20      208.59      135.99
       80.20       17.50      220.70      151.73
       80.20       20.80      325.74      260.40
       80.20       24.10      304.98      243.27
       80.20       27.40      141.19       83.11
       80.20       30.70      283.38      228.93
       80.20       34.00      307.29      256.47
       80.20       37.30      309.73      262.54
       80.20       40.60      266.59      223.03
       80.20       43.90      199.57      159.64
       80.20       47.20      256.17      219.87
       80.20       50.50      230.62      197.95
       80.20       53.80      330.36      301.32
       80.20       57.10      226.68      201.27
       80.20       60.40      228.96      207.18
       80.20       63.70      229.41      211.26
       80.20       67.00      242.16      227.64
       80.20       70.30      192.60      181.71
       80.20       73.60      342.62      335.36
       80.20       76.90      163.38      159.75
       80.20       80.20      232.48      232.48
       80.20       83.50      289.62      293.25
 
Rows: 151:180
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       80.20       86.80      245.76      253.02
       80.20       90.10      185.34      196.23
       80.20       93.40      187.62      202.14
       80.20       96.70      207.16      225.31
       80.20      100.00      161.15      182.93
      100.00        1.00      284.87      175.97
      100.00        4.30      207.92      102.65
      100.00        7.60      192.84       91.20
      100.00       10.90      177.75       79.74
      100.00       14.20      162.67       68.29
      100.00       17.50      169.49       78.74
      100.00       20.80      176.84       89.72
      100.00       24.10      184.18      100.69
      100.00       27.40      200.69      120.83
      100.00       30.70      236.04      159.81
      100.00       34.00      271.38      198.78
      100.00       37.30      306.73      237.76
      100.00       40.60      342.08      276.74
      100.00       43.90      364.72      303.01
      100.00       47.20      346.78      288.70
      100.00       50.50      328.83      274.38
      100.00       53.80      310.89      260.07
      100.00       57.10      292.95      245.76
      100.00       60.40      275.00      231.44
      100.00       63.70      257.06      217.13
      100.00       67.00      239.11      202.81
      100.00       70.30      221.17      188.50
      100.00       73.60      203.22      174.18
      100.00       76.90      185.28      159.87
      100.00       80.20      167.33      145.55
 
Rows: 181:186
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       83.50      156.23      138.08
      100.00       86.80      183.06      168.54
      100.00       90.10      209.89      199.00
      100.00       93.40      236.73      229.47
      100.00       96.70      263.56      259.93
      100.00      100.00      285.78      285.78
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=11
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  OUT=b  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV 'PLOT
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:120  Number of Columns: 2       
File Version:IBIS-2  Organization:ROW  SubType:NONE
 
Rows: 1:30
+-----------+-----------
         C:1         C:2
        REAL        REAL
+-----------+-----------
        1.36       66.95
        5.18       72.16
     -719.41       47.98
        1.36       66.95
        0.00        0.00
        5.53       61.93
        1.36       66.95
     -719.41       47.98
        5.53       61.93
        0.00        0.00
       49.26      792.93
     -719.41       47.98
        5.18       72.16
       49.26      792.93
        0.00        0.00
        5.53       61.93
     -719.41       47.98
       49.26     -696.97
        5.53       61.93
        0.00        0.00
        5.18       72.16
        1.36       66.95
        5.53       61.93
        5.18       72.16
        0.00        0.00
        5.53       61.93
       43.22       64.43
        5.18       72.16
        5.53       61.93
        0.00        0.00
 
Rows: 31:60
+-----------+-----------
         C:1         C:2
        REAL        REAL
+-----------+-----------
        5.53       61.93
       51.00       24.63
       43.22       64.43
        5.53       61.93
        0.00        0.00
       49.26     -696.97
       51.00       24.63
        5.53       61.93
       49.26     -696.97
        0.00        0.00
       43.22       64.43
       97.15       64.73
        5.18       72.16
       43.22       64.43
        0.00        0.00
       49.26      792.93
        5.18       72.16
       97.15       64.73
       49.26      792.93
        0.00        0.00
       58.61       47.00
       43.22       64.43
       51.00       24.63
       58.61       47.00
        0.00        0.00
       62.81       28.52
       58.61       47.00
       51.00       24.63
       62.81       28.52
        0.00        0.00
 
Rows: 61:90
+-----------+-----------
         C:1         C:2
        REAL        REAL
+-----------+-----------
       74.31       23.80
       51.00       24.63
       49.26     -696.97
       74.31       23.80
        0.00        0.00
       51.00       24.63
       62.68       25.23
       62.81       28.52
       51.00       24.63
        0.00        0.00
       43.22       64.43
       58.61       47.00
       78.34       51.98
       43.22       64.43
        0.00        0.00
       62.68       25.23
       51.00       24.63
       74.31       23.80
       62.68       25.23
        0.00        0.00
       58.61       47.00
       62.81       28.52
       78.34       51.98
       58.61       47.00
        0.00        0.00
       62.81       28.52
       62.68       25.23
       74.31       23.80
       62.81       28.52
        0.00        0.00
 
Rows: 91:120
+-----------+-----------
         C:1         C:2
        REAL        REAL
+-----------+-----------
       78.34       51.98
       62.81       28.52
       74.31       23.80
       78.34       51.98
        0.00        0.00
       78.34       51.98
       97.15       64.73
       43.22       64.43
       78.34       51.98
        0.00        0.00
       74.31       23.80
       97.15       64.73
       78.34       51.98
       74.31       23.80
        0.00        0.00
       74.31       23.80
       49.26     -696.97
      817.92       47.98
       74.31       23.80
        0.00        0.00
      817.92       47.98
       49.26      792.93
       97.15       64.73
      817.92       47.98
        0.00        0.00
      817.92       47.98
       97.15       64.73
       74.31       23.80
      817.92       47.98
        0.00        0.00
ibis-gen a version=ibis-1 org=column  datacol=(1,2,3,4)  +
    nc=4 nr=44
Beginning VICAR task ibis
mf a func=("c1=sqrt(index*17+2763)","c2=sqrt(index*7+3431)")
Beginning VICAR task mf
mf a func=("c1=mod(c1,0.0001)*1000000","c2=mod(c2,0.0001)*1000000")
Beginning VICAR task mf
mf a func=("c3=c1*1.1+index/10","c4=c2*1.1+index/10")
Beginning VICAR task mf
tieconv INP=a COLS=(1,2,3,4)  poly=linear OUT=b  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
tieconv INP=b COLS=(1,2,3,4)  poly=linear OUT=b2  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        1.92        1.92
        1.00       15.14        2.12       17.68
        1.00       29.29        2.33       33.45
        1.00       43.43        2.54       49.21
        1.00       57.57        2.75       64.98
        1.00       71.71        2.96       80.74
        1.00       85.86        3.16       96.51
        1.00      100.00        3.37      112.27
       15.14        1.00       17.62        2.06
       15.14       15.14       17.83       17.83
       15.14       29.29       18.04       33.59
       15.14       43.43       18.25       49.36
       15.14       57.57       18.45       65.12
       15.14       71.71       18.66       80.89
       15.14       85.86       18.87       96.65
       15.14      100.00       19.08      112.42
       29.29        1.00       33.33        2.21
       29.29       15.14       33.54       17.98
       29.29       29.29       33.74       33.74
       29.29       43.43       33.95       49.51
       29.29       57.57       34.16       65.27
       29.29       71.71       34.37       81.04
       29.29       85.86       34.57       96.80
       29.29      100.00       34.78      112.57
       43.43        1.00       49.03        2.36
       43.43       15.14       49.24       18.13
       43.43       29.29       49.45       33.89
       43.43       43.43       49.66       49.66
       43.43       57.57       49.87       65.42
       43.43       71.71       50.07       81.19
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.28       96.95
       43.43      100.00       50.49      112.72
       57.57        1.00       64.74        2.51
       57.57       15.14       64.95       18.28
       57.57       29.29       65.16       34.04
       57.57       43.43       65.36       49.81
       57.57       57.57       65.57       65.57
       57.57       71.71       65.78       81.34
       57.57       85.86       65.99       97.10
       57.57      100.00       66.19      112.87
       71.71        1.00       80.45        2.66
       71.71       15.14       80.65       18.42
       71.71       29.29       80.86       34.19
       71.71       43.43       81.07       49.95
       71.71       57.57       81.28       65.72
       71.71       71.71       81.48       81.48
       71.71       85.86       81.69       97.25
       71.71      100.00       81.90      113.01
       85.86        1.00       96.15        2.81
       85.86       15.14       96.36       18.57
       85.86       29.29       96.57       34.34
       85.86       43.43       96.78       50.10
       85.86       57.57       96.98       65.87
       85.86       71.71       97.19       81.63
       85.86       85.86       97.40       97.40
       85.86      100.00       97.61      113.16
      100.00        1.00      111.86        2.96
      100.00       15.14      112.07       18.72
      100.00       29.29      112.27       34.49
      100.00       43.43      112.48       50.25
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.69       66.02
      100.00       71.71      112.90       81.78
      100.00       85.86      113.10       97.55
      100.00      100.00      113.31      113.31
ibis-list b2 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        1.92        1.92
        1.00       15.14        2.12       17.68
        1.00       29.29        2.33       33.45
        1.00       43.43        2.54       49.21
        1.00       57.57        2.75       64.98
        1.00       71.71        2.96       80.74
        1.00       85.86        3.16       96.51
        1.00      100.00        3.37      112.27
       15.14        1.00       17.62        2.06
       15.14       15.14       17.83       17.83
       15.14       29.29       18.04       33.59
       15.14       43.43       18.25       49.36
       15.14       57.57       18.45       65.12
       15.14       71.71       18.66       80.89
       15.14       85.86       18.87       96.65
       15.14      100.00       19.08      112.42
       29.29        1.00       33.33        2.21
       29.29       15.14       33.54       17.98
       29.29       29.29       33.74       33.74
       29.29       43.43       33.95       49.51
       29.29       57.57       34.16       65.27
       29.29       71.71       34.37       81.04
       29.29       85.86       34.57       96.80
       29.29      100.00       34.78      112.57
       43.43        1.00       49.03        2.36
       43.43       15.14       49.24       18.13
       43.43       29.29       49.45       33.89
       43.43       43.43       49.66       49.66
       43.43       57.57       49.87       65.42
       43.43       71.71       50.07       81.19
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.28       96.95
       43.43      100.00       50.49      112.72
       57.57        1.00       64.74        2.51
       57.57       15.14       64.95       18.28
       57.57       29.29       65.16       34.04
       57.57       43.43       65.36       49.81
       57.57       57.57       65.57       65.57
       57.57       71.71       65.78       81.34
       57.57       85.86       65.99       97.10
       57.57      100.00       66.19      112.87
       71.71        1.00       80.45        2.66
       71.71       15.14       80.65       18.42
       71.71       29.29       80.86       34.19
       71.71       43.43       81.07       49.95
       71.71       57.57       81.28       65.72
       71.71       71.71       81.48       81.48
       71.71       85.86       81.69       97.25
       71.71      100.00       81.90      113.01
       85.86        1.00       96.15        2.81
       85.86       15.14       96.36       18.57
       85.86       29.29       96.57       34.34
       85.86       43.43       96.78       50.10
       85.86       57.57       96.98       65.87
       85.86       71.71       97.19       81.63
       85.86       85.86       97.40       97.40
       85.86      100.00       97.61      113.16
      100.00        1.00      111.86        2.96
      100.00       15.14      112.07       18.72
      100.00       29.29      112.27       34.49
      100.00       43.43      112.48       50.25
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.69       66.02
      100.00       71.71      112.90       81.78
      100.00       85.86      113.10       97.55
      100.00      100.00      113.31      113.31
tieconv INP=a COLS=(1,2,3,4)  poly=keystone OUT=b  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
tieconv INP=b COLS=(1,2,3,4)  poly=keystone OUT=b2  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        2.26        2.26
        1.00       15.14        2.39       17.95
        1.00       29.29        2.53       33.64
        1.00       43.43        2.66       49.33
        1.00       57.57        2.79       65.02
        1.00       71.71        2.93       80.71
        1.00       85.86        3.06       96.40
        1.00      100.00        3.20      112.10
       15.14        1.00       17.88        2.32
       15.14       15.14       18.03       18.03
       15.14       29.29       18.18       33.74
       15.14       43.43       18.33       49.45
       15.14       57.57       18.49       65.16
       15.14       71.71       18.64       80.87
       15.14       85.86       18.79       96.58
       15.14      100.00       18.95      112.29
       29.29        1.00       33.49        2.38
       29.29       15.14       33.67       18.11
       29.29       29.29       33.84       33.84
       29.29       43.43       34.01       49.57
       29.29       57.57       34.18       65.30
       29.29       71.71       34.35       81.03
       29.29       85.86       34.53       96.76
       29.29      100.00       34.70      112.48
       43.43        1.00       49.11        2.44
       43.43       15.14       49.30       18.19
       43.43       29.29       49.49       33.94
       43.43       43.43       49.69       49.69
       43.43       57.57       49.88       65.43
       43.43       71.71       50.07       81.18
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.26       96.93
       43.43      100.00       50.45      112.68
       57.57        1.00       64.73        2.50
       57.57       15.14       64.94       18.27
       57.57       29.29       65.15       34.04
       57.57       43.43       65.36       49.80
       57.57       57.57       65.57       65.57
       57.57       71.71       65.78       81.34
       57.57       85.86       65.99       97.11
       57.57      100.00       66.20      112.87
       71.71        1.00       80.35        2.56
       71.71       15.14       80.58       18.35
       71.71       29.29       80.81       34.13
       71.71       43.43       81.04       49.92
       71.71       57.57       81.27       65.71
       71.71       71.71       81.49       81.49
       71.71       85.86       81.72       97.28
       71.71      100.00       81.95      113.07
       85.86        1.00       95.96        2.62
       85.86       15.14       96.21       18.43
       85.86       29.29       96.46       34.23
       85.86       43.43       96.71       50.04
       85.86       57.57       96.96       65.85
       85.86       71.71       97.21       81.65
       85.86       85.86       97.46       97.46
       85.86      100.00       97.71      113.26
      100.00        1.00      111.58        2.68
      100.00       15.14      111.85       18.51
      100.00       29.29      112.12       34.33
      100.00       43.43      112.39       50.16
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.65       65.98
      100.00       71.71      112.92       81.81
      100.00       85.86      113.19       97.63
      100.00      100.00      113.46      113.46
ibis-list b2 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        2.26        2.26
        1.00       15.14        2.39       17.95
        1.00       29.29        2.53       33.64
        1.00       43.43        2.66       49.33
        1.00       57.57        2.79       65.02
        1.00       71.71        2.93       80.71
        1.00       85.86        3.06       96.40
        1.00      100.00        3.20      112.10
       15.14        1.00       17.88        2.32
       15.14       15.14       18.03       18.03
       15.14       29.29       18.18       33.74
       15.14       43.43       18.33       49.45
       15.14       57.57       18.49       65.16
       15.14       71.71       18.64       80.87
       15.14       85.86       18.79       96.58
       15.14      100.00       18.95      112.29
       29.29        1.00       33.49        2.38
       29.29       15.14       33.67       18.11
       29.29       29.29       33.84       33.84
       29.29       43.43       34.01       49.57
       29.29       57.57       34.18       65.30
       29.29       71.71       34.35       81.03
       29.29       85.86       34.53       96.76
       29.29      100.00       34.70      112.48
       43.43        1.00       49.11        2.44
       43.43       15.14       49.30       18.19
       43.43       29.29       49.49       33.94
       43.43       43.43       49.69       49.69
       43.43       57.57       49.88       65.43
       43.43       71.71       50.07       81.18
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.26       96.93
       43.43      100.00       50.45      112.68
       57.57        1.00       64.73        2.50
       57.57       15.14       64.94       18.27
       57.57       29.29       65.15       34.04
       57.57       43.43       65.36       49.80
       57.57       57.57       65.57       65.57
       57.57       71.71       65.78       81.34
       57.57       85.86       65.99       97.11
       57.57      100.00       66.20      112.87
       71.71        1.00       80.35        2.56
       71.71       15.14       80.58       18.35
       71.71       29.29       80.81       34.13
       71.71       43.43       81.04       49.92
       71.71       57.57       81.27       65.71
       71.71       71.71       81.49       81.49
       71.71       85.86       81.72       97.28
       71.71      100.00       81.95      113.07
       85.86        1.00       95.96        2.62
       85.86       15.14       96.21       18.43
       85.86       29.29       96.46       34.23
       85.86       43.43       96.71       50.04
       85.86       57.57       96.96       65.85
       85.86       71.71       97.21       81.65
       85.86       85.86       97.46       97.46
       85.86      100.00       97.71      113.26
      100.00        1.00      111.58        2.68
      100.00       15.14      111.85       18.51
      100.00       29.29      112.12       34.33
      100.00       43.43      112.39       50.16
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.65       65.98
      100.00       71.71      112.92       81.81
      100.00       85.86      113.19       97.63
      100.00      100.00      113.46      113.46
tieconv INP=a COLS=(1,2,3,4)  poly=keystone OUT=b  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
tieconv INP=b COLS=(1,2,3,4)  poly=linear OUT=b2  +
      NAH=7,NAV=7,MINL=1.,MINS=1.,MAXL=100.,MAXS=100.  +
    'GEOMV
Beginning VICAR task tieconv
tieconv version Tue Mar 01 2011
ibis-list b 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        2.26        2.26
        1.00       15.14        2.39       17.95
        1.00       29.29        2.53       33.64
        1.00       43.43        2.66       49.33
        1.00       57.57        2.79       65.02
        1.00       71.71        2.93       80.71
        1.00       85.86        3.06       96.40
        1.00      100.00        3.20      112.10
       15.14        1.00       17.88        2.32
       15.14       15.14       18.03       18.03
       15.14       29.29       18.18       33.74
       15.14       43.43       18.33       49.45
       15.14       57.57       18.49       65.16
       15.14       71.71       18.64       80.87
       15.14       85.86       18.79       96.58
       15.14      100.00       18.95      112.29
       29.29        1.00       33.49        2.38
       29.29       15.14       33.67       18.11
       29.29       29.29       33.84       33.84
       29.29       43.43       34.01       49.57
       29.29       57.57       34.18       65.30
       29.29       71.71       34.35       81.03
       29.29       85.86       34.53       96.76
       29.29      100.00       34.70      112.48
       43.43        1.00       49.11        2.44
       43.43       15.14       49.30       18.19
       43.43       29.29       49.49       33.94
       43.43       43.43       49.69       49.69
       43.43       57.57       49.88       65.43
       43.43       71.71       50.07       81.18
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.26       96.93
       43.43      100.00       50.45      112.68
       57.57        1.00       64.73        2.50
       57.57       15.14       64.94       18.27
       57.57       29.29       65.15       34.04
       57.57       43.43       65.36       49.80
       57.57       57.57       65.57       65.57
       57.57       71.71       65.78       81.34
       57.57       85.86       65.99       97.11
       57.57      100.00       66.20      112.87
       71.71        1.00       80.35        2.56
       71.71       15.14       80.58       18.35
       71.71       29.29       80.81       34.13
       71.71       43.43       81.04       49.92
       71.71       57.57       81.27       65.71
       71.71       71.71       81.49       81.49
       71.71       85.86       81.72       97.28
       71.71      100.00       81.95      113.07
       85.86        1.00       95.96        2.62
       85.86       15.14       96.21       18.43
       85.86       29.29       96.46       34.23
       85.86       43.43       96.71       50.04
       85.86       57.57       96.96       65.85
       85.86       71.71       97.21       81.65
       85.86       85.86       97.46       97.46
       85.86      100.00       97.71      113.26
      100.00        1.00      111.58        2.68
      100.00       15.14      111.85       18.51
      100.00       29.29      112.12       34.33
      100.00       43.43      112.39       50.16
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.65       65.98
      100.00       71.71      112.92       81.81
      100.00       85.86      113.19       97.63
      100.00      100.00      113.46      113.46
ibis-list b2 'format
Beginning VICAR task ibis
 
Number of Rows:64  Number of Columns: 4       
File Version:IBIS-2  Organization:COLUMN  SubType:NONE
 
Rows: 1:30
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
        1.00        1.00        2.02        2.02
        1.00       15.14        2.22       17.78
        1.00       29.29        2.43       33.54
        1.00       43.43        2.63       49.30
        1.00       57.57        2.83       65.06
        1.00       71.71        3.03       80.81
        1.00       85.86        3.23       96.57
        1.00      100.00        3.43      112.33
       15.14        1.00       17.71        2.15
       15.14       15.14       17.91       17.91
       15.14       29.29       18.11       33.67
       15.14       43.43       18.31       49.43
       15.14       57.57       18.51       65.18
       15.14       71.71       18.71       80.94
       15.14       85.86       18.91       96.70
       15.14      100.00       19.11      112.46
       29.29        1.00       33.39        2.28
       29.29       15.14       33.59       18.04
       29.29       29.29       33.79       33.79
       29.29       43.43       34.00       49.55
       29.29       57.57       34.20       65.31
       29.29       71.71       34.40       81.07
       29.29       85.86       34.60       96.83
       29.29      100.00       34.80      112.59
       43.43        1.00       49.08        2.41
       43.43       15.14       49.28       18.16
       43.43       29.29       49.48       33.92
       43.43       43.43       49.68       49.68
       43.43       57.57       49.88       65.44
       43.43       71.71       50.08       81.20
 
Rows: 31:60
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
       43.43       85.86       50.28       96.95
       43.43      100.00       50.48      112.71
       57.57        1.00       64.76        2.53
       57.57       15.14       64.96       18.29
       57.57       29.29       65.16       34.05
       57.57       43.43       65.37       49.81
       57.57       57.57       65.57       65.57
       57.57       71.71       65.77       81.32
       57.57       85.86       65.97       97.08
       57.57      100.00       66.17      112.84
       71.71        1.00       80.45        2.66
       71.71       15.14       80.65       18.42
       71.71       29.29       80.85       34.18
       71.71       43.43       81.05       49.94
       71.71       57.57       81.25       65.69
       71.71       71.71       81.45       81.45
       71.71       85.86       81.65       97.21
       71.71      100.00       81.85      112.97
       85.86        1.00       96.13        2.79
       85.86       15.14       96.33       18.55
       85.86       29.29       96.53       34.31
       85.86       43.43       96.73       50.06
       85.86       57.57       96.94       65.82
       85.86       71.71       97.14       81.58
       85.86       85.86       97.34       97.34
       85.86      100.00       97.54      113.10
      100.00        1.00      111.82        2.92
      100.00       15.14      112.02       18.67
      100.00       29.29      112.22       34.43
      100.00       43.43      112.42       50.19
 
Rows: 61:64
+-----------+-----------+-----------+-----------
         C:1         C:2         C:3         C:4
        DOUB        DOUB        DOUB        DOUB
+-----------+-----------+-----------+-----------
      100.00       57.57      112.62       65.95
      100.00       71.71      112.82       81.71
      100.00       85.86      113.02       97.46
      100.00      100.00      113.22      113.22
end-proc
exit
slogoff
if ($RUNTYPE = "INTERACTIVE")
  if ($syschar(1) = "VAX_VMS")
  end-if
else
  if ($syschar(1) = "VAX_VMS")
  end-if
end-if
ulogoff
END-PROC
END-PROC
$ Return
$!#############################################################################
