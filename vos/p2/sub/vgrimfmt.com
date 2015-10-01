$!****************************************************************************
$!
$! Build proc for MIPL module vgrimfmt
$! VPACK Version 1.9, Monday, December 07, 2009, 16:41:09
$!
$! Execute by entering:		$ @vgrimfmt
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
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!   OTHER       Only the "other" files are created.
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
$ write sys$output "*** module vgrimfmt ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Test = ""
$ Create_Imake = ""
$ Create_Other = ""
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
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if primary .eqs. "OTHER" then Create_Other = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Test .or. Create_Imake .or -
        Create_Other .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to vgrimfmt.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Create_Other then gosub Other_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$   Create_Other = "Y"
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
$   if F$SEARCH("vgrimfmt.imake") .nes. ""
$   then
$      vimake vgrimfmt
$      purge vgrimfmt.bld
$   else
$      if F$SEARCH("vgrimfmt.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake vgrimfmt
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @vgrimfmt.bld "STD"
$   else
$      @vgrimfmt.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create vgrimfmt.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack vgrimfmt.com -mixed -
	-s vgrimfmt.c -
	-i vgrimfmt.imake -
	-t tvgrimfmt.f tzvgrimfmt.c tvgrimfmt.imake tvgrimfmt.pdf -
	   tstvgrimfmt.pdf -
	-o vgrimfmt.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create vgrimfmt.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*===========================================================================*
 | VGRIMFMT   -- Fortran bridge function to return image format information  |
 |		 for each Voyager telemetry format. (UNIX version)           |
 |									     |
 |	vgrimfmt(imcode,fmt,&scan_rate,spix,npix,&ind)			     |
 |			where ind=1 for normal return			     |
 |			         =0 for invalid image format		     |
 |									     |
 |	Input:     imcode						     |
 |									     |
 |	Outputs:   fmt,scan_rate,spix,npix			             |
 *============================================================================
 *	Module history:
 *
 *	  DATE		 FR	    DESCRIPTION
 *     -----------     ------- ----------------------------------------------
 *	27-Oct-94		REL	Fortran bridge to C routine zvgrimfmt	
 *					for the Unix platform
 *				Original release.
 *=============================================================================
 */
#include  "xvmaininc.h"   
#include  "ftnbridge.h" 
#include  "vgrimcod.h"   
#include <string.h>

/*============================================================================
 *
 *	Fortran-Callable Version
 *
 *==========================================================================*/

void FTN_NAME2(vgrimfmt, VGRIMFMT) (int *imcode, char format[], int *scan_rate,
	short int spix[], short int npix[], int *ind, ZFORSTR_PARAM)
#if 0
int *imcode;	/* input image format code */
char format[];	/* ASCII image format, e.g. "IM2s" */
int *scan_rate;	/* number of minor frames expected (== scan rate) */
short int spix[],npix[];  /* starting pixel, number of pixels for each seg */
int  *ind;
#endif
{
	ZFORSTR_BLOCK
	char	c_format[6];  

	*ind=zvgrimfmt( *imcode, c_format, scan_rate, spix, npix );

	zsc2for( c_format, 5, format, &imcode, 5, 2, 1, ind);

}

/*============================================================================
 *
 *	C-Callable Version
 *
*==========================================================================*/
int
zvgrimfmt(imcode,format,scan_rate,spix,npix)
int imcode;	/* input image format code */
char *format;	/* ASCII image format, e.g. "IM2s" */
int *scan_rate;	/* number of minor frames expected (== scan rate) */
short int spix[],npix[];  /* starting pixel, number of pixels for each seg */

{
static struct imd {
	char imcode;
	char format[5];
	int scan_rate;
	short ss;		/* Starting sample of first segment of line */
	short ns;		/* Number of samples per telemetry frame    */
   } imdata[33] = {
/*	 0,"I3-G4", 1,   1, 800,	   decomitted in favor of IM2WP */
	 0,"IM-2W", 1,   1, 800,	/* IM2WP, PWS part of IM2W	*/
	 1,"     ", 0,   0,   0,	/* IM16 */
         2,"IM-S ", 5,  61, 338,
	 3,"IM-20",10,  29, 744,	/* 742 PIXELS FOR ODD LINES */
	 4,"IM-O ", 5,   1, 800,
	 5,"IM-21",10,  30, 372,	/* 370 PIXELS FOR ODD LINES */
	 6,"IM-Q ", 5,   1, 800,
	 7,"IM-22",10,   1, 400,	/* ODD LINES ONLY, ALT. PIXELS */
	 8,"IM-K ",10,   1, 800,
/*	 9,"IM-7 ", 3,   1, 272,   ------ commented out Oct. 10, 1989 ----*/
         9,"GS-4B", 1,   1, 800,    
	10,"IM-2X", 2,   1, 800,
/*	11,"IM-9 ", 3, 161, 160,   -----  commented out Oct. 10, 1989 ----*/
        11,"GS-4C", 1,   1, 800,
	12,"IM-2C", 5,   1, 800,
	13,"IM-23",10,  17, 384,	/* ODDS 17-783, EVES 18-780	*/
	14,"IM-24", 2, 101, 600,
	15,"GS-4 ", 1,   1, 800,
	16,"IM-2W", 1,   1, 800,
	17,"GS-2 ", 1,   1, 800,
	18,"     ", 0,   0,   0,		/* IM14? */
	19,"OC-3 ",10,365,   44,	/* ODDS 333-419. EVENS 334-420	*/
	20,"IM-12", 5, 201,  80,
	21,"IM-11", 5,   1, 160,
	22,"IM-10", 5, 136, 106,	/* Real-time imaging from PB-10 */
	23,"IM-26", 5,   2, 800,
	24,"IM-15", 1, 265, 272,
	25,"IM-25", 5, 101, 600,
/*	26,"IM-6 ", 1, 181, 440,	/* Decommited in favor of IM2D	*/
	26,"IM-2D", 5,  77, 324,	/* ODDS 77-723, EVENS 78-724	*/
	27,"IM-5 ", 2,   1, 440,
	28,"IM-4 ", 1,  97, 608,
	29,"IM-3 ", 1,   1, 800,
	30,"IM-2 ", 1,   1, 800,
	31,"IM-13",10,	 1,  80};

	int	imc;
	int	n;
	int	seg;
	int	sr;
	short	ss;
	short	ns;

    imc = imcode;
    if ( (imc < 0 ) || (imc > 31) )
	return (0);
    sr = imdata[imc].scan_rate;
    if (sr == 0)
	return(0);		/* skip if format error */
    strcpy( format, imdata[imc].format);
    *scan_rate = sr;
    ss = imdata[imc].ss;
    ns = imdata[imc].ns;
    for (seg=0; seg<sr; seg++)
    {	spix[seg] = ss;
	npix[seg] = ns;
	/*
	 *--------------------------------------------------------------------
	 * IM20,21 AND 22 ARE SPECIAL SINCE THERE IS REALY ONE SEGMENT OF
	 * IMAGING LINE EVEN THOUGH THE RATE IS 10:1 THEREFORE THE STARTING
	 * SAMPLE FOR ALL SEGMENTS IS THE SAME
	 *--------------------------------------------------------------------
	 */
	if ((imc == IM20) || (imc == IM21) || (imc == IM22))
	    continue;
	else
	    ss += ns;
    }
    if (imc == IM5)
	npix[1] = 360;
/* 						   */
/* ---- IM7 does NOT exist anymore   Oct. 10, 1989 */
/*   else			*/
/*	if (imc == IM7)         */
/*	    npix[2] = 256;      */
    return(1);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create vgrimfmt.imake
/***********************************************************************

                     IMAKE FILE FOR SUBROUTINE LIBRARY prnt

   To Create the build file give the command:

	$ vimake vgrimfmt                     (VMS)
   or
	% vimake vgrimfmt                     (Unix)


*************************************************************************/

#define SUBROUTINE vgrimfmt

#define MODULE_LIST vgrimfmt.c

#define FTN_STRING
#define P2_SUBLIB

#define USES_ANSI_C
$ Return
$!#############################################################################
$Test_File:
$ create tvgrimfmt.f
C TVGRIMFMT.F
C Routine to test VGRIMFMT IN FORTAN.....
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44

      INTEGER*4 SCAN_RATE
      INTEGER*2 SS(10),NS(10)
      CHARACTER*5 FORMAT
      CHARACTER*80 MS1
C
      DO IM=0,31
	  DO I = 1,10
		SS(I)=0
		NS(I)=0
          ENDDO
          CALL VGRIMFMT(IM,FORMAT,SCAN_RATE,SS,NS,IND)
          IF (IND.EQ.1) THEN
              WRITE(MS1,101) IM,FORMAT,SCAN_RATE
	      CALL XVMESSAGE(MS1,' ')
	      WRITE(MS1,102)
	      CALL XVMESSAGE(MS1, ' ')
	      DO I = 1,SCAN_RATE
		WRITE(MS1,103) SS(I),NS(I)
		CALL XVMESSAGE(MS1, ' ')
	      ENDDO
          ENDIF
      ENDDO

C	Repeat test case in C to test C interface: zvgrimfmt
      
      CALL XVMESSAGE( ' ',' ')
      CALL XVMESSAGE( '    REPEAT ABOVE TESTS FROM C LANGUAGE', ' ')
      CALL XVMESSAGE( ' ',' ')
      CALL TZVGRIMFMT()
C
  101 FORMAT('  IMCODE=',I2,'   FORMAT=',A5,'   SCAN_RATE=',I2)
  102 FORMAT('        ** SS **    ** NS **')
  103 FORMAT('	',I5,'       ',I5)
      END
$!-----------------------------------------------------------------------------
$ create tzvgrimfmt.c
/*===========================================================================*
 *	tzvgrimfmt.c
 *
 *  Routine to test zvgrimfmt in C
 *============================================================================
 */
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzvgrimfmt) ()
{
	int	imc,imcode,count,def;
	int	scan_rate;
	char	format[6];
	int	i,status;
	short	ss[10],ns[10];
	char	message[128];

	imcode=31;
	for ( imc=0; imc<=imcode; imc++ ) {
		for (i=0; i<10; i++)
			ss[i]=ns[i]=0;

		status = zvgrimfmt( imc,format,&scan_rate,ss,ns);
		if (status == 1) {
			sprintf(message,"IMCODE=%d, FORMAT=%s, SCAN_RATE=%d",imc,format,scan_rate);
			zvmessage(message,"");
			sprintf(message,"	SS	NS");
			zvmessage(message,"");
			for (i=0; i<scan_rate; i++){ 
			   sprintf(message,"	%d	%d",ss[i],ns[i]);
			   zvmessage(message,"");
			}
		}
		zvmessage(" ","");
	}
}
$!-----------------------------------------------------------------------------
$ create tvgrimfmt.imake
/* Imake file for Test of VICAR subroutine vgrimfmt */

#define PROGRAM tvgrimfmt

#define MODULE_LIST tvgrimfmt.f tzvgrimfmt.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_FORTRAN
#define LIB_P2SUB

$!-----------------------------------------------------------------------------
$ create tvgrimfmt.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstvgrimfmt.pdf
procedure  help=*
refgbl $echo
body
let _onfail="continue"
Write " "
Write " "
Write " *************TEST ON VGRIMFMT ROUTINE************"
Write " ***********Calling from FORTRAN program *********"

tvgrimfmt

Write " "
Write " *************TEST COMPLETED************"

end-proc
$ Return
$!#############################################################################
$Other_File:
$ create vgrimfmt.hlp
1 VGRIMFMT

	Returns image format information for each Voyager telemetry format.
		    

	FORTRAN Calling sequence:

                INTEGER*4 	SCAN_RATE
		INTEGER*2       SS(10),NS(10)
		CHARACTERS*5	FORMAT

		CALL VGRIMFMT(IMCODE,FORMAT,SCAN_RATE,SS,NS)

	C Calling sequence:
		int	imcode;
		int 	scan_rate;
		char  	format[6];
		short 	ss[10],ns[10];
		int 	status;

		zvgrimfmt(imcode,format,&scan_rate,ss,ns);

	Input:	imcode		- integer consisting of image format code,
				  from bits 3-7 of the Voyager telemetry
				  format ID.

	Output:	 format		- ASCII image format (e.g. IM-11)
		 scan_rate	- integer scan rate
		 ss[10]		- short integer array consisting of samples
				  for each segment of line.
		 ns[10]		- short integer array consisting of the
				  number of samples for each segment.
 

	Status returns:	1 for normal return.
			0 for invalid image code.


2 History

Original programmer:	Gary Yagi
Cognizant programmer:	GARY YAGI
Source language:	C

Ported to UNIX by:	Raymond Lin,	Nov-1994
$ Return
$!#############################################################################
