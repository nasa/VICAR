$!****************************************************************************
$!
$! Build proc for MIPL module splot
$! VPACK Version 1.5, Friday, March 19, 1993, 19:46:56
$!
$! Execute by entering:		$ @splot
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
$ write sys$output "*** module splot ***"
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
$   if F$SEARCH("splot.imake") .nes. ""
$   then
$      vimake splot
$      purge splot.bld
$   else
$      if F$SEARCH("splot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake splot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @splot.bld "STD"
$   else
$      @splot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create splot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack splot.com -
	-s splot.c -
	-i splot.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create splot.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*************************************************************************/
/* Bridge for FORTRAN access                                             */
/*************************************************************************/
int splot_(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1)
{
   splot(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1);
}

/**************************************************************************/


#include <stdio.h>
#define	 XMAX  780
#define  YMAX  370
#define	 XMIN  100
#define  YMIN   10
#define  TICLEN	5
#define	 BUFSIZE 132


char *splotlabelr(r) float r;
   {
   static char buf[20];
   sprintf(buf,"%9.5g",r);

   return buf;
   }

int splot(x,y,n,xlo,xhi,ylo,yhi,xtic,ytic,mode,grid1)
   float *x,	/* array of x data points */
	 *y,	/* array of y data plints */
	 *xlo,	/* min x value		  */
	 *xhi,	/* max x value		  */
	 *ylo,	/* min y value		  */
	 *yhi;	/* max y value		  */
   int	 *n,	/* number of points	  */
	 *xtic,	/* number of x tickmarks  */
	 *ytic,	/* number of y tickmarks  */
	 *mode, /* mode of plotting 	  */
         *grid1; /* 0=nogrid 1=grid	  */
/*
 *	Plots data on screen of VT240 terminal 
 *
 *		Programmer Stan Schultz
 *		Startdate  ?/?/84
 *		modified   8/20/85	SDS to check if last string = string
 *		modified   1/14/86	SDS to add grid option
 *              modified   1/26/87      REA adapt to be a VICAR subroutine
 *		modified   4/8/87	REA initialize character size
 *
 *		Mode is an integer that is defined as follows
 *
 *			mode =-1	draw continuious line
 *			mode = 0	draw continuious line
 *			mode = 1	break line when y=ylo
 *			mode = 2	point plot
 */
{
   int	i,j,ix,iy,nx,ny,out,grid,lastx,lasty;
   float rangex,rangey,offx,offy,t;
   char esc=27;
   char s[BUFSIZE],c;

   rangey = (*yhi-*ylo);
   offy   = (*ylo);
   rangex = (*xhi-*xlo);
   offx   = (*xlo);
   if(*grid1!=0)
      grid = 1;
   else
      grid = 0;

   if(*xtic>0 && *ytic>0)
      printf("\033[2J\033[0;0H",esc,esc);		/* clear screen */
   printf("%cP1pS(A[0,0][799,479])W(P1)T(S1);",esc);	/* enter regis */
   if(*xtic>0 && *ytic>0)
      {
      printf("P[%d,%d]V[%d,%d]V[%d,%d]",XMIN,YMIN,XMAX,YMIN,XMAX,YMAX);/* draw*/
      printf("V[%d,%d]V[%d,%d]P[%d,%d]",XMIN,YMAX,XMIN,YMIN,XMIN,YMAX);/* box */
      for(j=grid;j<2;j++)			/*  X tickmarks */
         {
         int iy2;
         iy = j*(YMAX-YMIN-TICLEN*2) + YMIN;
         if(grid)
            {
            iy  = YMAX;
            iy2 = YMIN;
            }
         else
            iy2 = iy +TICLEN*2;
         for(i=0;i<=*xtic;i++)
            {
            ix = XMIN + i*(XMAX-XMIN)/(*xtic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix,iy2);
            if(j==1)
               {
	       t = offx + i * rangex/(*xtic);
	       printf("\n;P[%d,%d]T\"%s\"",ix-80,iy+20,splotlabelr(t));
               }
            }
         }

      for(j=grid;j<2;j++)			/* Y tickmarks */
         {
         int ix2;
         ix = j*(XMAX-XMIN-TICLEN) + XMIN;
         if(grid)
            ix2 = XMIN;
         else
            ix2 = ix+TICLEN;
         for(i=0;i<=*ytic;i++)
            {
            iy = YMIN + i*(YMAX-YMIN)/(*ytic);
            printf("\n;P[%d,%d]V[%d,%d]",ix,iy,ix2,iy);
            if(j==1)
               {
               t = offy + (*ytic-i) * rangey/(*ytic);
               printf("\n;P[%d,%d]T\"%s\"",5,iy-7,splotlabelr(t));
               }
            }
         }
      }

   s[0]=0;
   out=0;/* not out of bounds */
   for(i=0;i<*n;i++)				/* plot data at given scale */
      {
      if(c=='Q')
          break;
      iy = YMAX - (y[i]-offy) * (YMAX-YMIN) /rangey;
      ix = XMIN + (x[i]-offx) * (XMAX-XMIN) /rangex;
      if(i!=0 && ix==lastx && iy==lasty)
         continue;
      else
         {
         lastx=ix;
         lasty=iy;
         }
      if(iy<YMIN || iy>YMAX || ix<XMIN || ix>XMAX)/* point is out of bounds */
         out++;
      if(out)
         {
         if(iy < YMIN) iy = YMIN;
         if(iy > YMAX) iy = YMAX;
         if(ix < XMIN) ix = XMIN;
         if(ix > XMAX) ix = XMAX;
         if(out>1 && i<*n-1)   /* if not first out of bounds point */
            {
            ny = YMAX - (y[i+1]-offy) * (YMAX-YMIN) /rangey;
            nx = XMIN + (x[i+1]-offx) * (XMAX-XMIN) /rangex;
            if(nx<YMIN || ny>YMAX || nx<XMIN || nx>XMAX) /* if next point is */
               continue;                                 /* also off screen  */
            out=0;                             /* if next point is on screen */
            }
         }
      if(i==0)
         printf("P[%d,%d]",ix,iy);
      if(out<=1)
         {
         s[0]=0;j=0;
         switch (*mode)
            {
            case 2 : sprintf(s,"P[%d,%d]V[%d,%d]",ix,iy,ix,iy);
	             break;
	    case 1 : if((y[i]==offy))	/* break line */
	                break;
	             if((i!=0) && (y[i-1]==offy))
		        j=sprintf(s,"P[%d,%d]",ix,iy);
                     if(y[i]==offy)
                        break;
            case 0 : 
	    case -1: sprintf(s+j,"V[%d,%d]",ix,iy);
	             break;
            }
         puts(s);	/* send command */
         }
      }

   printf("%c\\%c[22;1H",esc,esc);		/* exit regis */
   return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create splot.imake

#define SUBROUTINE splot

#define MODULE_LIST splot.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
