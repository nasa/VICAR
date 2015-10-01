$!****************************************************************************
$!
$! Build proc for MIPL module dgelg
$! VPACK Version 1.9, Monday, December 07, 2009, 16:11:19
$!
$! Execute by entering:		$ @dgelg
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
$ write sys$output "*** module dgelg ***"
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
$ write sys$output "Invalid argument given to dgelg.com file -- ", primary
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
$   if F$SEARCH("dgelg.imake") .nes. ""
$   then
$      vimake dgelg
$      purge dgelg.bld
$   else
$      if F$SEARCH("dgelg.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dgelg
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dgelg.bld "STD"
$   else
$      @dgelg.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dgelg.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dgelg.com -mixed -
	-s dgelg.f zdgelg.c -
	-i dgelg.imake -
	-t tdgelg.f tzdgelg.c tdgelg.imake tdgelg.pdf tstdgelg.pdf -
	-o dgelg.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dgelg.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C*****************************************************************************
C
C Subroutine DGELG
C
C Solves a general system of simultaneous linear equations
C 11/18/1992 - M. O'Shaughnessy		Ported DGELG to UNIX
C*****************************************************************************
      subroutine dgelg( r, a, m, n, eps, ifail)

c Work arrays are ipvt and info. The input parameter EPS is NOT used 
c in this subroutine.
      double precision r(m,n),a(m,m)
      integer n_eqns
      parameter (n_eqns = 100)
      integer i,m,n,ifail,eps,ipvt(n_eqns),info
      character*80 msg

      ifail = 0
      if (m .gt. n_eqns) then
         write (msg,10) m
10    format(' DGELG> Internal array needs to be enlarged >= to ',i4)
         call xvmessage(msg,' ')
         call abend
      end if

c..get the lu decomposition of matrix a.

      call dgefa(a,m,m,ipvt,info)

c..if not singular, then solve for each right hand side

      if ( info .ne. 0 )  then
         call xvmessage('DGELG> Matrix is singular',' ')
         ifail = 1
      else
         do 100 i = 1, n
            call dgesld(a,m,m,ipvt,r(1,i))
100      continue
      end if

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zdgelg.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of DGELG                                         */
/************************************************************************/

void zdgelg(r,a,m,n,eps,ifail)
double *r,
       *a;
int     m,
        n,
        eps,
        ifail;
{
FTN_NAME2(dgelg, DGELG) (r,a,m,n,eps,ifail);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dgelg.imake
/* Imake file for VICAR subroutine DGELG */

#define SUBROUTINE dgelg

#define MODULE_LIST dgelg.f zdgelg.c
#define FTNINC_LIST fortport

#define USES_C
#define USES_FORTRAN
#define P2_SUBLIB




$ Return
$!#############################################################################
$Test_File:
$ create tdgelg.f
C*****************************************************************************
C Unit test program TDGELG.F for subroutine DGELG
C Ported to UNIX 11/10/1992
C*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44

      integer i, ifail, ir, n, eps
c     .. local arrays ..
      double precision a(3,3)/33,-24,-8,16,-10,-4,72,-57,-17/
      double precision b(3,2)/-359,281,85,-718,562,170/
      character*80 string

      n = 3
      ir = 2

c.. Test the fortran subroutine
      call xvmessage(' Testing DGELG.F',' ')
      call xvmessage(' ...input R matrix:',' ')
      do 10 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 10   continue
      call xvmessage(' ...input A coefficients:',' ')
      do 20 i=1,n
        write (string,99995) a(i,1)
        call xvmessage(string,' ')
        write (string,99995) a(i,2)
        call xvmessage(string,' ')
        write (string,99995) a(i,3)
        call xvmessage(string,' ')
 20   continue

      call dgelg(b,a,n,ir,eps,ifail)

      if (ifail .ne. 0) go to 100
      call xvmessage(' ...solution matrix:',' ')
      do 30 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 30   continue

c.. Test the C-bridge
      call xvmessage(' Testing the C-bridge',' ')
      b(1,1) = -359
      b(2,1) = 281
      b(3,1) = 85 
      b(1,2) = -718
      b(2,2) = 562
      b(3,2) = 170
      call xvmessage(' ...input R matrix:',' ')
      do 40 i=1,n
        write (string,99995) b(i,1)
        call xvmessage(string,' ')
        write (string,99995) b(i,2)
        call xvmessage(string,' ')
 40   continue
      a(1,1) = 33
      a(2,1) = -24
      a(3,1) = -8
      a(1,2) = 16
      a(2,2) = -10
      a(3,2) = -4
      a(1,3) = 72
      a(2,3) = -57
      a(3,3) = -17
      call xvmessage(' ...input A coefficients:',' ')
      do 50 i=1,n
        write (string,99995) a(i,1)
        call xvmessage(string,' ')
        write (string,99995) a(i,2)
        call xvmessage(string,' ')
        write (string,99995) a(i,3)
        call xvmessage(string,' ')
 50   continue

      call tzdgelg(b,a,n,ir,eps,ifail)

      if (ifail .ne. 0) go to 100
      call xvmessage(' ...solution matrix:',' ')
      do 60 i=1,n
       write (string,99995) b(i,1)
       call xvmessage(string,' ')
       write (string,99995) b(i,2)
       call xvmessage(string,' ')
60    continue

      return

100   write(string,99996) ifail
      call xvmessage(string,' ')
      return

99996 format (' error in dgelg, ifail = ', i2)
99995 format ('   ',1x , f10.5)
      end
$!-----------------------------------------------------------------------------
$ create tzdgelg.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzdgelg)(r,a,m,n,eps,ifail)
double *r,
       *a;
int     m,
        n,
        eps,
        ifail;
{
/*  ============================================  */

      zdgelg(r,a,m,n,eps,ifail);
}
$!-----------------------------------------------------------------------------
$ create tdgelg.imake
/* Imake file for test program of VICAR subroutine dgelg */

#define PROGRAM tdgelg

#define MODULE_LIST tdgelg.f tzdgelg.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C
#define LIB_P2SUB

#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77

/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create tdgelg.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstdgelg.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
TDGELG
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create dgelg.hlp
1 DGELG

  DGELG is a FORTRAN and C-callable, UNIX-ported subroutine which 
  solves a general system of simultaneous linear equations.

  FORTRAN Calling Sequence:  

                call DGELG(R,A,M,N,EPS,IFAIL)

  Arguments: 
		R (input)	double precision M by N right hand side matrix.
				On return, R contains the solutions of the 
				equations.
		A (input)	double precision M by M coefficient matrix.
                                A is modified by DGELG.
		M (input)	integer number of equations in the system
		N (input)	number of right hand side vectors
		EPS (input)     Not used in this implimentation.
		IFAIL(output)	a resulting error parameter.  IFAIL=0
                                if DGELG is successful.

  C Calling Sequence:  

                zdgelg(r,a,m,n,eps,ifail);

  Arguments: 
		double *r;      (input/output) 
                                M by N right hand side matrix. 
				On return, R contains the solutions of the 
				equations.
		double *a;      (input/output)	
                                M by M coefficient matrix. A is modified by 
                                DGELG.
		int m;          (input)	number of equations in the system
		int n;          (input)	number of right hand side vectors
		int eps;        (input) ** NOT USED **
		int ifail;      (outpu) a resulting error parameter. IFAIL=0
                                if DGELG is successful.
2 History

  Original Programmer: STEVE POHORSKY
  Current Cognizant Programmer: STEVE POHORSKY
  Source Language: Fortran, with C-bridge.
  
  11/20/1992 M. O'Shaughnessy -- Ported DGELG to UNIX, added C-bridge.

2 Operation

	  This subroutine uses the MATH77 routines (from LINPACK)
          DGEFA and DGESLD.

2 Arguments

	R (input)	double precision M by N right hand side matrix.
			On return, R contains the solutions of the 
			equations.
	A (input)	double precision M by M coefficient matrix.
                        A is modified by SIMQ.
	M (input)	integer number of equations in the system
	N (input)	number of right hand side vectors
	EPS (input)     Not used in this implimentation.
	IFAIL(output)	a resulting error parameter.  IFAIL=0
                        if DGELG is successful.

        See 'help dgelg' for C-callable parameter descriptions.
$ Return
$!#############################################################################
