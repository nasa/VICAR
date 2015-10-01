$!****************************************************************************
$!
$! Build proc for MIPL module psplot
$! VPACK Version 1.8, Thursday, February 08, 2001, 16:45:15
$!
$! Execute by entering:		$ @psplot
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
$ write sys$output "*** module psplot ***"
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
$ write sys$output "Invalid argument given to psplot.com file -- ", primary
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
$   if F$SEARCH("psplot.imake") .nes. ""
$   then
$      vimake psplot
$      purge psplot.bld
$   else
$      if F$SEARCH("psplot.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake psplot
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @psplot.bld "STD"
$   else
$      @psplot.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create psplot.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack psplot.com -
	-s psplot.c -
	-i psplot.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create psplot.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*****************************************************************************/
/* Bridge for FORTRAN access                                                 */
/*                                                                           */
/*****************************************************************************/
psplot_(x,y,npts,xlo,xhi,ylo,yhi,xtic,ytic,xlabel,ylabel,title1,title2,title3,
       lflag)
{
   psplot(x,y,npts,xlo,xhi,ylo,yhi,xtic,ytic,xlabel,ylabel,title1,title2,
	  title3,lflag);
}

/******************************************************************************/



/*------------------------------------------------------------------------------

	Plots data on PostScript printer

------------------------------------------------------------------------------*/
#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdio.h>
#if !defined (VMS)
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#endif
#ifdef VMS
#include <ssdef.h>
#include <descrip.h>
#endif

#define perr(cmd, str)	(void) printf("\nERROR: %s <%s>\n", cmd, str)

#define XMIN	0.0
#define YMIN	0.0
#define XMAX	7.0
#define YMAX	4.4

void	psdone();
void	psgraph();
void	psinit();
void	psxaxis();
void	psyaxis();

static		psnum = 0;
static FILE	*psout = NULL;

/*----------------------------------------------------------------------------*/
psplot(x,y,npts,xlo,xhi,ylo,yhi,xtic,ytic,xlabel,ylabel,title1,title2,title3,
       lflag)
float	*x, *y;		/* arrays of x and y data points */
int	*npts;		/* number of points		 */
float	*xlo, *xhi;	/* min, max x values		 */
float	*ylo, *yhi;	/* min, max y values		 */
int	*xtic, *ytic;	/* number of x and y tickmarks	 */
char	*xlabel;	/* label for x axis		 */
char	*ylabel;	/* label for y axis		 */
char	*title1;	/* first title line of plot	 */
char	*title2;	/* second title line of plot	 */
char    *title3;	/* third title line of plot	 */
int	*lflag;		/* last plot flag, <> 0 for last plot
					   = 0 otherwise */
{
#ifdef VMS
	long	timval;
#else
	time_t	timval;
#endif

	/* Do initialization and labeling the FIRST time through ONLY */
	if (!psnum)
	{
		/* Open and initialize PostScript file */
	    	psinit();
		if (psout == NULL)
			return -1;

		/* Do axis labeling (tic marks, numbers, and label strings) */
		psxaxis(*xtic, *xlo, *xhi, xlabel);
		psyaxis(*ytic, *ylo, *yhi, ylabel);
	
		/* Write plot title strings */
		(void) fprintf(psout, "newpath\n");
		(void) fprintf(psout, "Bfont\n");
		(void) fprintf(psout,
				"XMAX 2 div inch YMAX 0.75 add inch moveto\n");
		(void) fprintf(psout, "(%s) centershow\n", title1);


		(void) fprintf(psout, "newpath\n");
		(void) fprintf(psout, "Lfont\n");
		(void) fprintf(psout, "XMAX 2 div inch 1.25 inch neg moveto\n");
		(void) fprintf(psout, "(%s) centershow\n", title2);

		(void) fprintf(psout, "newpath\n");
		(void) fprintf(psout, "Lfont\n");
		(void) fprintf(psout, "XMAX 2 div inch 1.75 inch neg moveto\n");
		(void) fprintf(psout, "(%s) centershow\n", title3);

		/* Write timestamp */
#ifdef VMS
		timval = time((long *) NULL);
#else
		timval = time((time_t *) NULL);
#endif

		(void) fprintf(psout, "newpath\n");
		(void) fprintf(psout, "Tfont\n");
		(void) fprintf(psout, "7.5 inch -2 inch moveto\n");
		(void) fprintf(psout, "(%s) show\n", ctime(&timval));
	}

	/* Plot actual graphs */
	psgraph(x, y, *npts, *xlo, *xhi, *ylo, *yhi);
	if (*lflag ) psdone();
	return 1;
}
/*----------------------------------------------------------------------------*/
void
psinit()
{
	float	xmin = XMIN, xmax = XMAX, ymin = YMIN, ymax = YMAX;

	if ((psout = fopen("psplot.ps", "w")) == NULL)
	{
		(void) printf("Error opening psplot.ps\n");
		return;
	}
	
	(void) fprintf(psout, "%%!PS-Adobe\n");
	(void) fprintf(psout, "%% INITIALIZATION\n");
	(void) fprintf(psout, "/inch {72 mul} def\n");

	(void) fprintf(psout, "\n%% Point sizes\n");
	(void) fprintf(psout, "/BPS 16 def\n");
	(void) fprintf(psout, "/LPS 13 def\n");
	(void) fprintf(psout, "/SPS 9 def\n");
	(void) fprintf(psout, "/TPS 6 def\n");

	(void) fprintf(psout, "\n%% Box corners\n");
	(void) fprintf(psout, "/XMIN %f def\n", xmin);
	(void) fprintf(psout, "/XMAX %f def\n", xmax);
	(void) fprintf(psout, "/YMIN %f def\n", ymin);
	(void) fprintf(psout, "/YMAX %f def\n", ymax);

	(void) fprintf(psout, "\n%% Font definitions\n");
	(void) fprintf(psout, "/Bfont {\n");
	(void) fprintf(psout, " /Helvetica-Bold findfont\n");
	(void) fprintf(psout, " BPS scalefont setfont} def\n");
	(void) fprintf(psout, "/Lfont {\n");
	(void) fprintf(psout, " /Helvetica findfont\n");
	(void) fprintf(psout, " LPS scalefont setfont} def\n");
	(void) fprintf(psout, "/Sfont {\n");
	(void) fprintf(psout, " /Helvetica findfont\n");
	(void) fprintf(psout, " SPS scalefont setfont} def\n");
	(void) fprintf(psout, "/Tfont {\n");
	(void) fprintf(psout, " /Helvetica findfont\n");
	(void) fprintf(psout, " TPS scalefont setfont} def\n");

	(void) fprintf(psout, "\n%% Linetype definitions\n");
	(void) fprintf(psout, "/Solid [] def\n");
	(void) fprintf(psout, "/Wdash [4 2] def\n");	/* Wide dash */
	(void) fprintf(psout, "/Ndash [2 4] def\n");	/* Narrow dash */
	(void) fprintf(psout, "/Dashdot [5 3 1 3] def\n");
	(void) fprintf(psout, "/Dot [1 2] def\n");
	(void) fprintf(psout, "/Dashdotdot [5 3 1 3 1 3] def\n");
	(void) fprintf(psout, "/Dotdashdash [5 3 5 3 1 3] def\n");

	(void) fprintf(psout, "\n%% Horizontally Center Text\n");
	(void) fprintf(psout, "/centershow {\n");
	(void) fprintf(psout, " dup\n");
	(void) fprintf(psout, " stringwidth pop 2 div neg 0 rmoveto\n");
	(void) fprintf(psout, " show} def\n");

	(void) fprintf(psout, "\n%% Right Justify, Vertically Center Text\n");
	(void) fprintf(psout, "/rvertshow {\n");
	(void) fprintf(psout, " dup\n");
	(void) fprintf(psout, " stringwidth pop neg SPS 3 div neg rmoveto\n");
	(void) fprintf(psout, " show} def\n");

	(void) fprintf(psout, "\n%% Define coordinate space\n");
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "0 inch 11 inch translate\n");
	(void) fprintf(psout, "90 neg rotate\n");
	(void) fprintf(psout, "2 inch 2.25 inch translate\n");
	(void) fprintf(psout, "gsave\n\n");

	(void) fprintf(psout, "%% Draw Box\n");
	(void) fprintf(psout, "0 inch 0 inch moveto\n");
	(void) fprintf(psout, "0 inch YMAX inch rlineto\n");
	(void) fprintf(psout, "XMAX inch 0 inch rlineto\n");
	(void) fprintf(psout, "0 inch YMAX inch neg rlineto\n");
	(void) fprintf(psout, "closepath\n");
	(void) fprintf(psout, "stroke\n");
	(void) fprintf(psout, "0.25 setlinewidth\n");
	return;
}
/*----------------------------------------------------------------------------*/
void
psdone()
{
#ifdef VMS
	$DESCRIPTOR(d, "print/form=post/delete psplot.ps");
#endif

	(void) fprintf(psout, "showpage");

	if (fclose(psout) == EOF)
		perr("PSPLOT", "Cannot close plot file");

#ifdef VMS
	if (lib$spawn(&d) != SS$_NORMAL)
		perror("spawn() failed\n");
#endif
#ifdef unix
	(void) system("lpr psplot.ps");
	(void) system("rm -f psplot.ps");
#endif
	(void) putchar('\n');
	
	psnum = 0;
	return;
}
/*----------------------------------------------------------------------------*/
void
psxaxis(xtic, xlo, xhi, label)
int	xtic;
float	xlo, xhi;
char	*label;
{
	int	ix;
	float	tx, xval, xloc;

	tx = (XMAX - XMIN) / xtic;

	/* Do X tick marks */
	(void) fprintf(psout, "\n%% X AXIS\n");
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "0 0 moveto\n");
	for (ix = xtic; ix > 0; ix--)
	{
		(void) fprintf(psout, "%f inch 0.0625 inch rmoveto\n", tx);
		(void) fprintf(psout, "0 inch -0.0625 inch rlineto\n");
	}
	(void) fprintf(psout, "stroke\n");

	/* Write X axis values */
	xval = xlo;
	xloc = XMIN;
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "Sfont\n");
	(void) fprintf(psout, "%f inch -0.25 inch moveto\n", xloc);
	(void) fprintf(psout, "(%g) centershow\n", xval);

	xval += (xhi - xlo) / xtic;
	xloc += tx;
	for (ix = xtic; ix > 0; ix--)
	{
		(void) fprintf(psout, "%f inch -0.25 inch moveto\n", xloc);
		(void) fprintf(psout, "(%g) centershow\n", xval);
		xval += (xhi - xlo) / xtic;
		xloc += tx;
	}

	/* Write X label */
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "Lfont\n");
	(void) fprintf(psout, "XMAX 2 div inch 0.5 inch neg moveto\n");
	(void) fprintf(psout, "(%s) centershow\n", label);

	return;
}
/*----------------------------------------------------------------------------*/
void
psyaxis(ytic, ylo, yhi, label)
int	ytic;
float	ylo, yhi;
char	*label;
{
	int	iy;
	float	ty, yval, yloc;

	ty = (YMAX - YMIN) / ytic;
	
	/* Do Y tick marks */
	(void) fprintf(psout, "\n%% Y AXIS\n");
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "90 rotate\n");
	(void) fprintf(psout, "0 0 moveto\n");
	for (iy = ytic; iy > 0; iy--)
	{
		(void) fprintf(psout, "%f inch -0.0625 inch rmoveto\n", ty);
		(void) fprintf(psout, "0 inch 0.0625 inch rlineto\n");
	}
	(void) fprintf(psout, "stroke\n");
	(void) fprintf(psout, "grestore\n");

	/* Write Y axis values */
	yval = ylo;
	yloc = YMIN;
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "Sfont\n");
	(void) fprintf(psout, "-0.25 inch %f inch moveto\n", yloc);
	(void) fprintf(psout, "(%.2f) rvertshow\n", yval);

	yval += (yhi - ylo) / ytic;
	yloc += ty;
	for (iy = ytic; iy > 0; iy--)
	{
		(void) fprintf(psout, "-0.25 inch %f inch moveto\n", yloc);
		(void) fprintf(psout, "(%.2f) rvertshow\n", yval);
		yval += (yhi - ylo) / ytic;
		yloc += ty;
	}

	/* Write Y label */
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "90 rotate\n");
	(void) fprintf(psout, "Lfont\n");
	(void) fprintf(psout, "YMAX 2 div inch 1 inch moveto\n");
	(void) fprintf(psout, "(%s) centershow\n", label);
	(void) fprintf(psout, "90 neg rotate\n");

	return;
}
/*----------------------------------------------------------------------------*/
void
psgraph(x, y, npts, xlo, xhi, ylo, yhi)
float	*x, *y;		/* arrays of x and y data points */
int	npts;		/* number of points		 */
float	xlo, xhi;	/* min, max x values		 */
float	ylo, yhi;	/* min, max y values		 */
{
	char	linetype[20];
	float	scalex, scaley;

	scalex = (XMAX - XMIN) / (xhi - xlo);
	scaley = (YMAX - YMIN) / (yhi - ylo);

	switch (psnum % 7)
	{
		case 0:	(void) strcpy(linetype, "Solid");
			break;
		case 1:	(void) strcpy(linetype, "Wdash");
			break;
		case 2:	(void) strcpy(linetype, "Ndash");
			break;
		case 3:	(void) strcpy(linetype, "Dashdot");
			break;
		case 4:	(void) strcpy(linetype, "Dot");
			break;
		case 5:	(void) strcpy(linetype, "Dashdotdot");
			break;
		case 6:	(void) strcpy(linetype, "Dotdashdash");
			break;
		default:
			(void) strcpy(linetype, "Solid");
			break;
	}

	(void) printf("\n");

	(void) fprintf(psout, "\n%% PLOT GRAPH %d\n", psnum);
	(void) fprintf(psout, "%% Set Clipping Path\n");
	(void) fprintf(psout, "newpath\n");
	(void) fprintf(psout, "0 inch 0 inch moveto\n");
	(void) fprintf(psout, "0 inch YMAX inch rlineto\n");
	(void) fprintf(psout, "XMAX inch 0 inch rlineto\n");
	(void) fprintf(psout, "0 inch YMAX inch neg rlineto\n");
	(void) fprintf(psout, "closepath\n");
	(void) fprintf(psout, "clip\n");
	(void) fprintf(psout, "0 0 moveto\n");
	(void) fprintf(psout, "0.25 setlinewidth\n");
	(void) fprintf(psout, "%s 0 setdash\n", linetype);
	(void) fprintf(psout, "%f inch %f inch moveto\n",
		(*x - xlo) * scalex, (*y - ylo) * scaley);

	++x, ++y;
	--npts;
	while (npts)
	{
  		(void) fprintf(psout, "%f inch %f inch lineto\n",
  			(*x - xlo) * scalex, (*y - ylo) * scaley);
   		if (!(npts % 100))
   		{
   			(void) putchar('.');
   			(void) fprintf(psout, "stroke\n");
   			(void) fprintf(psout, "newpath\n");
   			(void) fprintf(psout, "%f inch %f inch moveto\n",
			(*x - xlo) * scalex, (*y - ylo) * scaley);
		}
		++x, ++y;
		--npts;
	}
	(void) fprintf(psout, "stroke\n");
	psnum++;
	(void) putchar('\n');

	return;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create psplot.imake

#define SUBROUTINE psplot

#define MODULE_LIST psplot.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
