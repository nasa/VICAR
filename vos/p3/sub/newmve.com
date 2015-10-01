$!****************************************************************************
$!
$! Build proc for MIPL module newmve
$! VPACK Version 1.9, Friday, March 05, 2010, 16:30:32
$!
$! Execute by entering:		$ @newmve
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
$ write sys$output "*** module newmve ***"
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
$ write sys$output "Invalid argument given to newmve.com file -- ", primary
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
$   if F$SEARCH("newmve.imake") .nes. ""
$   then
$      vimake newmve
$      purge newmve.bld
$   else
$      if F$SEARCH("newmve.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake newmve
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @newmve.bld "STD"
$   else
$      @newmve.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create newmve.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack newmve.com -mixed -
	-s newmve.c -
	-i newmve.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create newmve.c
$ DECK/DOLLARS="$ VOKAGLEVE"
static char sccsNewmve[] = "@(#) newmve.c 2.2 2/3/87 PDS Vicar2";

#include <stdio.h>


/****************************************************************************
 *   NEWMVE  - an addition to the MVE bridge subroutines that allows for more
 *             dcodes than given in the original mve.c.
 *
 *   NOTE :    The C and FORTRAN bridges for mve are in this file along with
 *             the internal version of the code.  The interface is the  same
 *             as the VMS version for both fortran and C.  The FORTRAN
 *             bridges pass along the number of arguments by value in numarg.
 *
 ***************************************************************************
 *   Format of call :
 *  
 *   CALL MVE(numarg, DCODE, N, A, B, AINC, BINC)     FORTRAN version
 *
 *   Where :
 *
 *     numarg    is the number of arguments to follow. (passed by value)
 *
 *     dcode     is the transfer mode.  This addition to the mve.c routines
 *               only takes care of the following cases :
 *               21  : double to byte
 *               22  : double to halfword
 *               23  : fullword to double
 *               24  : fullword to real
 *               25  : real to byte
 *               26  : real to halfword
 *
 *     N         is the number of elements to transfer
 *     A         Source vector to be transfered
 *     B         Destination vector to be transferred *     AINC      Source address increment - default 1 if ommitted
 *     BINC      Destination address increment - default 1 if ommitted
 *
 **************************************************************************/

/* FORTRAN bridge to newmve.c  */
newmve_(numarg,dcode,_n,a,b,_ainc,_binc, /* plus dummy f77 lengths */ l1, l2)
	int             numarg, *dcode, *_n, *_ainc, *_binc;
	unsigned char   *a, *b;
{
	newmve(numarg, dcode, _n, a, b, _ainc, _binc);
}

/* internal version of newmve, called from bridges above. */

newmve(numarg, dcode, len, a, b, _ainc, _binc)
	int             numarg, *dcode, *len, *_ainc, *_binc;
	unsigned char   *a, *b;

{
	register        i, n, ainc, binc;

	unsigned char  *byte;	/* 8 bit                    */
	short          *word;	/* 16 bit, same as halfword */
	unsigned       *full;	/* 32 bit, same as fullword */
	double         *comp;	/* 64 bit, same as double   */
	float          *real;	/* 32 bit                   */

	n = *len;
	if (numarg < 4) {
		printf("newmve: error %d are insufficient arguments to call\n",
		       numarg);
		zabend();
	}
	if (numarg < 5)
		ainc = 1;
	else
		ainc = *_ainc;
	if (numarg < 6)
		binc = 1;
	else
		binc = *_binc;

	switch (*dcode) {
	case -21:		/* byte to comp */
		comp = (double *)b;
		for (i = 0; i <= n; i++, a += ainc, comp += binc)
			*comp = *a;
		break;
	case -22:		/* word to comp */
		word = (short *)a, comp = (double *)b;
		for (i = 0; i <= n; i++, word += ainc, comp += binc)
			*comp = *word;
		break;
	case -23:		/* comp to full */
		comp = (double *)a, full = (unsigned *)b;
		for (i = 0; i <= n; i++, comp += ainc, full += binc)
			*full = *comp;
		break;
	case -24:		/* real to full */
		real = (float *)a, full = (unsigned *)b;
		for (i = 0; i <= n; i++, real += ainc, full += binc)
			*full = *real;
		break;
	case -25:		/* byte to real */
		real = (float *)b;
		for (i = 0; i <= n; i++, a += ainc, real += binc)
			*real = *a;
		break;
	case -26:		/* word to real */
		word = (short *)a, real = (float *)b;
		for (i = 0; i <= n; i++, word += ainc, real += binc)
			*real = *word;
		break;
	case 21:		/* comp to byte */
		comp = (double *)a;
		for (i = 0; i <= n; i++, comp += ainc, b += binc)
			*b = *comp;
		break;
	case 22:		/* comp to word */
		comp = (double *)a, word = (short *)b;
		for (i = 0; i <= n; i++, comp += ainc, word += binc)
			*word = *comp;
		break;
	case 23:		/* full to comp */
		full = (unsigned *)a, comp = (double *)b;
		for (i = 0; i <= n; i++, full += ainc, comp += binc)
			*comp = *full;
		break;
	case 24:		/* full to real */
		full = (unsigned *)a, real = (float *)b;
		for (i = 0; i <= n; i++, full += ainc, real += binc)
			*real = *full;
		break;
	case 25:		/* real to byte */
		real = (float *)a;
		for (i = 0; i <= n; i++, real += ainc, b += binc)
			*b = *real;
		break;
	case 26:		/* real to word */
		real = (float *)a, word = (short *)b;
		for (i = 0; i <= n; i++, real += ainc, word += binc)
			*word = *real;
		break;
	default:		/* otherwise invalid data */
		printf("  newmve : **** invalid value for dcode ****\n");
		zabend();
	      }
      }
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create newmve.imake

#define SUBROUTINE newmve

#define MODULE_LIST newmve.c

#define P3_SUBLIB

#define USES_C

$ Return
$!#############################################################################
