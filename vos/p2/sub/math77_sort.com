$!****************************************************************************
$!
$! Build proc for MIPL module math77_sort
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:07
$!
$! Execute by entering:		$ @math77_sort
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
$ write sys$output "*** module math77_sort ***"
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
$ write sys$output "Invalid argument given to math77_sort.com file -- ", primary
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
$   if F$SEARCH("math77_sort.imake") .nes. ""
$   then
$      vimake math77_sort
$      purge math77_sort.bld
$   else
$      if F$SEARCH("math77_sort.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake math77_sort
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @math77_sort.bld "STD"
$   else
$      @math77_sort.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create math77_sort.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack math77_sort.com -mixed -
	-s math77_sort.c -
	-i math77_sort.imake -
	-t tmath77_sort.c tmath77_sort.imake tmath77_sort.pdf -
	   tstmath77_sort.pdf -
	-o math77_sort.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create math77_sort.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/************************************************************************/
/* C-callable version of MATH77 sort routines.                          */
/************************************************************************/
#include "xvmaininc.h"
#include "ftnbridge.h"

void zisort(buf,m,n)
int *buf;
int m,n;
{
FTN_NAME2(isort, ISORT) (buf,&m,&n);
}

void zssort(buf,m,n)
float *buf;
int m,n;
{
FTN_NAME2(ssort, SSORT) (buf,&m,&n);
}

void zdsort(buf,m,n)
double *buf;
int m,n;
{
FTN_NAME2(dsort, DSORT) (buf,&m,&n);
}

void zisortp(buf,m,n,ip)
int *buf;
int m,n;
int *ip;
{
FTN_NAME2(isortp, ISORTP) (buf,&m,&n,ip);
}

void zssortp(buf,m,n,ip)
float *buf;
int m,n;
int *ip;
{
FTN_NAME2(ssortp, SSORTP) (buf,&m,&n,ip);
}

void zdsortp(buf,m,n,ip)
double *buf;
int m,n;
int *ip;
{
FTN_NAME2(dsortp, DSORTP) (buf,&m,&n,ip);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create math77_sort.imake
/* Imake file for VICAR subroutine math77_sort */
#define SUBROUTINE math77_sort
#define MODULE_LIST math77_sort.c
#define P2_SUBLIB
#define USES_C
$ Return
$!#############################################################################
$Test_File:
$ create tmath77_sort.c
#include "vicmain_c"

void main44()
{
  int i,n,buf[10],buf2[10],ptr[10];
  float rbuf[10],rbuf2[10];
  double dbuf[10],dbuf2[10];

  n=10;
  for (i=0; i<n; i++) {
     buf[i] = -1000 * i + 8000;
     buf2[i] = buf[i];
     rbuf[i] = buf[i];
     rbuf2[i] = buf[i];
     dbuf[i] = buf[i];
     dbuf2[i] = buf[i];
  }

  zprnt(4,10,buf,"Test zisort: buf=");
  zisort(buf,1,n);
  zprnt(4,10,buf," Sorted buf =");

  zprnt(7,10,rbuf,"Test zssort: buf=");
  zssort(rbuf,1,n);
  zprnt(7,10,rbuf," Sorted buf =");

  zprnt(8,10,dbuf,"Test zdsort: buf=");
  zdsort(dbuf,1,n);
  zprnt(8,10,dbuf," Sorted buf =");

  zprnt(4,10,buf2,"Test zisortp: buf=");
  zisortp(buf2,1,n,ptr);
  for (i=0; i<n; i++) buf[i]=buf2[ptr[i]-1];
  zprnt(4,10,buf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");

  zprnt(7,10,rbuf2,"Test zssortp: buf=");
  zssortp(rbuf2,1,n,ptr);
  for (i=0; i<n; i++) rbuf[i]=rbuf2[ptr[i]-1];
  zprnt(7,10,rbuf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");

  zprnt(8,10,dbuf2,"\nTest zdsortp: buf=");
  zdsortp(dbuf2,1,n,ptr);
  for (i=0; i<n; i++) dbuf[i]=dbuf2[ptr[i]-1];
  zprnt(8,10,dbuf," Sorted buf =");
  zprnt(4,10,ptr,"Pointer=");
}
$!-----------------------------------------------------------------------------
$ create tmath77_sort.imake
/* Imake file for Test of VICAR subroutine math77_sort */

#define PROGRAM tmath77_sort

#define MODULE_LIST tmath77_sort.c

#define MAIN_LANG_C
#define TEST
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
#define LIB_MATH77
/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create tmath77_sort.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstmath77_sort.pdf
! Test of C-bridges for MATH77 sort subroutines 
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tmath77_sort
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create math77_sort.hlp
1 math77_sort

  MATH77_SORT consists of C bridges to the FORTRAN sort routines contained in
  the MATH77 library:

	FORTRAN routine			C bridge

	ISORT(BUF,M,N)			zisort(buf,m,n);
        SSORT(SBUF,M,N)		        zssort(sbuf,m,n);
	DSORT(DBUF,M,N)			zdsort(dbuf,m,n);

	ISORTP(BUF,M,N,PTR)		zisortp(buf,m,n,ptr);
	SSORTP(SBUF,M,N,PTR)		zssortp(sbuf,m,n,ptr);
	DSORTP(DBUF,M,N,PTR)		zdsortp(dbuf,m,n,ptr);

  The MATH77 routines sort array elements M thru N in ascending order.  The
  MATH77 library also includes routines CSORT and CSORTP for sorting character
  strings, but the C bridges for these are not included here.

  You must include the following in your imake file if you use any of the
  MATH77 routines:

	#define LIB_MATH77 


2 Calling MATH77 Sort Routines From FORTRAN:

	INTEGER*4 BUF(K)
	REAL*4 SBUF(K)
	REAL*8 DBUF(K)

	CALL ISORT(BUF,M,N)		CALL ISORTP(BUF,M,N,PTR)
        CALL SSORT(SBUF,M,N)		CALL SSORTP(SBUF,M,N,PTR)
	CALL DSORT(DBUF,M,N)		CALL DSORTP(DBUF,M,N,PTR)

  For ISORTP, etc., the first argument is left unaltered and the sorted
  ordering is returned in the PTR array.  To sort the array, an additional
  step is required:

        CALL ISORTP(BUF,1,N,PTR)
        DO I=1,N
           BUF2[I] = BUF(PTR(I));
        ENDDO


2 Calling MATH77 Sort Routines From C:

	int buf[k],ptr[k];
	short hbuf[k],hptr[k];
	float sbuf[k];
	double dbuf[k];

	zisort(buf,m,n);	zisortp(buf,m,n,ptr);
        zssort(sbuf,m,n);	zssortp(sbuf,m,n,ptr);
	zdsort(dbuf,m,n);	zdsortp(dbuf,m,n,ptr);

  To sort an array when using routine zisortp, zssortp, or zdsortp, an
  additional step is required:

        zisortp(buf,1,n,ptr);
        for (i=0; i<n; i++) buf2[i]=buf[ptr[i]-1];


2 HISTORY

Written by: Gary Yagi, May 4, 2001
Current Cognizant Programmer: Gary Yagi
Source Language: C
Revision history:

04 May 01  GMY  Initial release
$ Return
$!#############################################################################
