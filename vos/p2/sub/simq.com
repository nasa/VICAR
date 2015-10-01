$!****************************************************************************
$!
$! Build proc for MIPL module simq
$! VPACK Version 1.9, Monday, August 06, 2001, 16:13:57
$!
$! Execute by entering:		$ @simq
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
$ write sys$output "*** module simq ***"
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
$ write sys$output "Invalid argument given to simq.com file -- ", primary
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
$   if F$SEARCH("simq.imake") .nes. ""
$   then
$      vimake simq
$      purge simq.bld
$   else
$      if F$SEARCH("simq.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake simq
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @simq.bld "STD"
$   else
$      @simq.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create simq.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack simq.com -mixed -
	-s simq.f zsimq.c -
	-i simq.imake -
	-t tsimq.c tsimq.pdf tstsimq.pdf tsimq.imake -
	-o simq.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create simq.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Solve a set of simultaneous linear equations: AX = B
C Uses MATH77 (from LINPACK) routines SGEFA AND SGESLD.
C SINGLE PRECISION version.
C
      SUBROUTINE SIMQ(A,B,N,IFAIL)
      REAL A(N,N)		!Input matrix
      REAL B(N)			!Input right-hand side, output as X
      INTEGER N			!Matrix dimension
      INTEGER IFAIL		!0=success, 1=singular matrix
      INTEGER IPVT(100),IND

      IF (N .GT. 100) THEN
	CALL XVMESSAGE('***SIMQ matrix size too large',' ')
	CALL ABEND
      END IF

      IFAIL = 1
      CALL SGEFA(A,N,N,IPVT,IND)	!Get the lu decomposition of matrix A.
      IF (IND .NE. 0) RETURN		!Quit if matrix is singular
      CALL SGESLD(A,N,N,IPVT,B)		!Solve for right hand side
      IFAIL = 0
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Solve a set of simultaneous linear equations: AX = B
C Uses MATH77 (from LINPACK) routines DGEFA AND DGESLD.
C DOUBLE PRECISION version.
C
      SUBROUTINE dsimq2(A,B,N,IFAIL)
      REAL*8 A(N,N)		!Input matrix
      REAL*8 B(N)		!Input right-hand side, output as X
      INTEGER N			!Matrix dimension
      INTEGER IFAIL		!0=success, 1=singular matrix
      INTEGER IPVT(100),IND

      IF (N .GT. 100) THEN
	CALL XVMESSAGE('***SIMQ matrix size too large',' ')
	CALL ABEND
      END IF

      IFAIL = 1
      CALL DGEFA(A,N,N,IPVT,IND)	!Get the lu decomposition of matrix A.
      IF (IND .NE. 0) RETURN		!Quit if matrix is singular
      CALL DGESLD(A,N,N,IPVT,B)		!Solve for right hand side
      IFAIL = 0
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zsimq.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

int zsimq(a, b, n)
  float *a, *b;
  int n;
{
   int ifail = 0;

   FTN_NAME(simq)(a, b, &n, &ifail);
   return ifail;
}

int zdsimq2(a, b, n)
  double *a, *b;
  int n;
{
   int ifail = 0;

   FTN_NAME(dsimq2)(a, b, &n, &ifail);
   return ifail;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create simq.imake
#define SUBROUTINE simq

#define MODULE_LIST simq.f zsimq.c

#define MAIN_LANG_FORTRAN
#define USES_FORTRAN
#define USES_ANSI_C

#define P2_SUBLIB
$ Return
$!#############################################################################
$Test_File:
$ create tsimq.c
#include "vicmain_c"
#include "ftnbridge.h"

void main44()
{
  char output[80];
  int n=3,ifail,i,j;
  float a[3][3],y[3];
  double da[3][3],dy[3];

  a[0][0]=33;  a[0][1]=-24;  a[0][2]= -8;
  a[1][0]=16;  a[1][1]=-10;  a[1][2]= -4;
  a[2][0]=72;  a[2][1]=-57;  a[2][2]=-17;

  y[0]=-359;   y[1]=281;     y[2]=85;

  for (i=0; i<3; i++) {
     dy[i] = y[i];
     for (j=0; j<3; j++) {
        da[i][j] = a[i][j];
     }
  }

  zvmessage(" Input matices:","");
  zvmessage("          A                 Y","");

  for (i=0; i<3; i++) {
     sprintf(output,"%6.2f %6.2f %6.2f    %6.2f",
	a[i][0],a[i][1],a[i][2],y[i]);
     zvmessage(output,"");
  }

  zvmessage(" ","");
  zvmessage("Single precision test:","");
  ifail = zsimq(a,y,n);
  if (ifail != 0) {
     zvmessage(" ***SIMQ failed","");
     zabend();
  }

  zvmessage("Output matrices:","");
  zvmessage("          A                 Y","");
  for (i=0; i<3; i++) {
     sprintf(output,"%6.2f %6.2f %6.2f    %6.2f",
	a[i][0],a[i][1],a[i][2],y[i]);
     zvmessage(output,"");
  }

  zvmessage(" ","");
  zvmessage("Double precision test:","");
  ifail = zdsimq2(da,dy,n);
  if (ifail != 0) {
     zvmessage(" ***dsimq2 failed","");
     zabend();
  }

  zvmessage("Output matrices:","");
  zvmessage("          A                 Y","");
  for (i=0; i<3; i++) {
     sprintf(output,"%6.2lf %6.2lf %6.2lf    %6.2lf",
	da[i][0],da[i][1],da[i][2],dy[i]);
     zvmessage(output,"");
  }
}
$!-----------------------------------------------------------------------------
$ create tsimq.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstsimq.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!
! TEST SCRIPT FOR simq SUBROUTINE
!
tsimq
!
end-proc
$!-----------------------------------------------------------------------------
$ create tsimq.imake
#define PROGRAM tsimq

#define MODULE_LIST tsimq.c

#define TEST

#define MAIN_LANG_FORTRAN
#define USES_ANSI_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$ Return
$!#############################################################################
$Other_File:
$ create simq.hlp
1 SIMQ,dsimq2

  Solves the system of N linear equations AX = Y, where A is an NxN matrix and
  X and Y are N-dimensional vectors.  A and Y are input and X is solved for
  and returned.

  Note: These subroutines use MATH77 routines (from LINPACK) SGEFA, SGESLD,
  DGEFA, and DGESLD.  You should include the following line in your Imake file:

	#define LIB_MATH77


2 Fortran Calling Sequence:

      REAL*4 A(N,N),Y(N)
      REAL*8 DA(N,N),DY(N)	
      INTEGER*4 N		!Input matrix dimension
      INTEGER*4 STATUS		!0=success, 1=singular matrix

      CALL SIMQ(A,Y,N,STATUS)
  or  CALL dsimq2(DA,DY,N,STATUS)

  Upon return, Y is replaced by the solution X.  Also, the matrix A is
  destroyed.


  C calling sequence:

      float a[n][n],y[n];
      double da[n][n],dy[n];
      int n;			/* input matrix dimension */
      int status;		/* 0=success, 1=singular matrix */

      status = zsimq(a,y,n);
  or  status = zdsimq2(da,dy,n);

  Upon return, y is replaced by the solution x.  Also, the matrix a is
  destroyed.

  Note that C organizes matrices ass-backwards from Fortran.  I.e., the
  Fortran matrix element A(i,j) corresponds to the C matrix element a[j-1][i-1].
  The C routines zsimq and dzsimq are incomplete bridges to the corresponding
  Fortran routines SIMQ and dsimq2.  Therefore you should transpose matrix A
  before calling zsimq or dzsimq.  For example, consider the linear equations

         x1 + 3*x2 = 1
       2*x1 + 4*x2 = 5

  Then matrix a and vector y look like this:

       a = |1 3|   y = |1|   n = 2
           |2 4|       |5|

  In C, maxtrix a normally looks like this:

       a[0][0]=1  a[0][1]=3
       a[1][0]=2  a[1][1]=4

  but for zsimq to work, you would have to transpose the matrix so that

       a[0][0]=1  a[0][1]=2
       a[1][0]=3  a[1][1]=4


2 History

  Original Programmer: Steve Pohorsky
  Current Cognizant Programmer: Steve Pohorsky
  Source Language: Fortran
  Revision History:
    20 Jul 01  GMY  Added double precision routines dsimq2 and zdsimq2.
$ Return
$!#############################################################################
