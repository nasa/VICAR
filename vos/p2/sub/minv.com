$!****************************************************************************
$!
$! Build proc for MIPL module minv
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:48
$!
$! Execute by entering:		$ @minv
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
$ write sys$output "*** module minv ***"
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
$ write sys$output "Invalid argument given to minv.com file -- ", primary
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
$   if F$SEARCH("minv.imake") .nes. ""
$   then
$      vimake minv
$      purge minv.bld
$   else
$      if F$SEARCH("minv.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake minv
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @minv.bld "STD"
$   else
$      @minv.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create minv.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack minv.com -mixed -
	-s minv.f zminv.c -
	-i minv.imake -
	-t tminv.f tzminv.c tminv.imake tminv.pdf tstminv.pdf -
	-o minv.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create minv.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MINV (A, N, D, L, M)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                               MINV
C       ----------------                                               ---
C	General routine for inverting an array calculating its determinant
C	Fortran format of call:
C
C	CALL MINV (A, N, D, L, M)
C
C	Parameters:-
C
C	A   (input/output)  REAL N by N Input Matrix.
C                           Returned as output the resulting inverse.
C       N   (input)         order of matrix A.
C       D   (output)        resultant determinant.  D=0 if A is singular.
C       L   (input)         work vector of length N.
C       M   (input)         work vector of length N.
C
C       Inverts a matrix and calculates its determinant using
C       MATH77 (from LINPACK) routines SGEFA, SGED and SGEI.
C
C   REVISION HISTORY
C
C      07-03-95   CRI  Removed LIB_LOCAL as per FR85780
C      14-04-94   CRI  MSTP S/W Conversion (VICAR Porting)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      REAL A(N,N), L(N), M(N), D, DET(2)
      INTEGER N


      PARAMETER ( RMIN_PAR = -37 )  ! VALID MAGNITUDES FOR REAL*4 ARE ROUGHLY
      PARAMETER ( RMAX_PAR =  37 )  ! 1.E-37 TO 1.E+37.


C==================================================================

C..GET THE LU DECOMPOSITION OF MATRIX A.

      CALL SGEFA( A,N,N, L, INFO )

      IF ( INFO .NE. 0 )  THEN
         D = 0.0                   ! A IS SINGULAR.

      ELSE

C..IF NOT SINGULAR, THEN INVERT AND CALCULATE DETERMINANT.

         CALL SGED (A,N,N,L,DET)	! DETERMINANT
         CALL SGEI (A,N,N,L,M)		! INVERSE

         IF ( DET(2) .GT. RMAX_PAR )  THEN
            CALL QPRINT( ' DETERMINANT TOO LARGE IN SUB MINV' )
            DET(2) = RMAX_PAR
         END IF

         IF ( DET(2) .LT. RMIN_PAR )  THEN
            D = 0.0

         ELSE
            D = DET(1)*10.0**DET(2)

         END IF
         
      END IF

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zminv.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

void zmrev( r, u)
float *r;
int u;				/* order of matrix a */

/************************************************************************/
/* A Fortran matrix is colume-major order and C is row-major order, thus*/
/* the row and columns will be interchanged in this bridge before and   */
/* after the call to the MINV subroutine                                */
/************************************************************************/

{
int i,j;
float k, *mp, *p;

for (i=0; i<u-1; i++)   /* for each row = i */
    {
    for (j=i+1; j<u; j++)  /* for each column = j skipping */
                           /* intersetion of each row and column */
        {
        p=(r+(i*u)+j);  /* set pointer to [i][j] of exchange */
        mp=(r+(j*u)+i); /* set pointer to [j][i] of exchange */

        k = *p;         /* save [i][j] */
        *p= *mp;        /* move [j][i] into [i][j] */
        *mp= k;         /* store [[i][j] into [j][i] */
        }
    }
}
/************************************************************************/
/* C-Callable Version: zminv - invert matrix and calculate determinant 	*/
/************************************************************************/

void zminv( a, n, d, l, m)
float *a;			/* array to invert, returned inverted */
int n;				/* order of matrix a */
float *d;                       /* determinant returned,
                                   d=0 if a is singular */
float *l;                       /* work vector of length n */
float *m;                       /* work vector of length n */

{
zmrev( a, n);			/* reverse major/minor */

FTN_NAME2(minv, MINV) ( a, &n, d, l, m); /* invoke minv */

zmrev( a, n);			/* reverse major/minor */
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create minv.imake
/***********************************************************************

                IMAKE FILE FOR VICAR SUBROUTINE minv

   To Create the build file give the command:

		$ vimake minv			(VMS)
   or
		% vimake minv			(Unix)


************************************************************************/

#define SUBROUTINE minv

#define MODULE_LIST minv.f zminv.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

/*#define LIB_LOCAL	/* remove on delivery */
/************************* End of Imake file ***************************/
$ Return
$!#############################################################################
$Test_File:
$ create tminv.f
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

      REAL*4 A(4,4), C(4,4)
      REAL*4 B(4), Z(4), D
      INTEGER*4 N

C     Initialize an array with a determinant of 1

      DATA A/5,7,6,5,  7,10,8,7,  6,8,10,9,  5,7,9,10/

C       INITIALIZE AN ARRAY TO COMPARE WITH THE 'C' VERSION

      C(1,1) = 6
      C(1,2) = 4
      C(1,3) = 6
      C(1,4) = 5

      C(2,1) = 7
      C(2,2) = 2
      C(2,3) = 3
      C(2,4) = 4

      C(3,1) = 4
      C(3,2) = 2
      C(3,3) = 1
      C(3,4) = 5

      C(4,1) = 6
      C(4,2) = 6
      C(4,3) = 6
      C(4,4) = 6
 
      N = 4
      CALL XVMESSAGE('FORTRAN TEST',' ')
      CALL XVMESSAGE('MATRIX WITH DETERMINANT = 1',' ')
      CALL TMINVMAT(A,N)
      CALL MINV(A,N,D,B,Z)
      CALL TMINVPRT (A,N,D)

      CALL XVMESSAGE('NOW INVERT BACK TO ORIGINAL',' ')
      CALL MINV(A,N,D,B,Z)
      CALL TMINVPRT (A,N,D)

      CALL XVMESSAGE('MATRIX FOR C BRIDGE COMPARE',' ')
      CALL TMINVMAT(C,N)
      CALL MINV(C,N,D,B,Z)
      CALL TMINVPRT (C,N,D)

      CALL XVMESSAGE('NOW INVERT BACK TO ORIGINAL',' ')
      CALL MINV(C,N,D,B,Z)
      CALL TMINVPRT (C,N,D)

      CALL XVMESSAGE('TEST THE C INTERFACE zminv',' ')
      CALL TZMINV

      RETURN
      END

C*******************************************************************C
C  THIS SUBROUTINE IS USED TO PRINT THE RESULTS OF THE TESTS.       C
C*******************************************************************C

      SUBROUTINE TMINVPRT (A,N,D)

      REAL*4 A(4,4), D
      INTEGER*4 N
      INTEGER*2 I, J
      CHARACTER*80 MSG

      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE('LOWER TRIANGLE OF INVERSE',' ')

      DO 340 I=1,N
         WRITE (MSG,99994) (A(I,J),J=1,I)
         CALL XVMESSAGE(MSG,' ')
  340 CONTINUE

      CALL XVMESSAGE(' ',' ')
      WRITE (MSG,99993) D
      CALL XVMESSAGE(MSG,' ')

99994 FORMAT (' ', 4F10.6)
99993 FORMAT ('VALUE OF DETERMINANT = ',F14.8)
      END
C*******************************************************************C
C  THIS SUBROUTINE IS USED TO PRINT THE MATRIX.			    C
C*******************************************************************C

      SUBROUTINE TMINVMAT (A,N)

      REAL*4 A(4,4)
      INTEGER*4 N
      INTEGER*2 I, J
      CHARACTER*80 MSG

      DO 540 I=1,N
         WRITE (MSG,99994) (A(I,J),J=1,N)
         CALL XVMESSAGE(MSG,' ')
  540 CONTINUE
      CALL XVMESSAGE(' ',' ')

99994 FORMAT (' ', 4F10.6)
      END
$!-----------------------------------------------------------------------------
$ create tzminv.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/*  This routine prints the test matrix.		           	*/
/************************************************************************/
tzminvmat(a1,n1) 
   float *a1;
   int n1;
   {
       int i, j;
       char msg[80], *mp;
       
       zvmessage("","");

       for (i=0; i<n1; i++)
       {
           mp = msg; 
           for (j=0; j<n1; j++)
               {
               sprintf (mp," %10f ",*(a1+(i*n1)+j));
               mp=(mp+11);
               }
           zvmessage(msg,"");
       }
       zvmessage("","");

   }
/************************************************************************/
/*  This routine prints output of tzminv for analysis.           	*/
/************************************************************************/
tzminvprt(a1,n1,d1) 
   float *a1,d1;
   int n1;
   {
       int i, j;
       char msg[80], *mp;
       
       zvmessage("","");
       zvmessage("LOWER TRIANGLE OF INVERSE","");

       for (i=0; i<n1; i++)
       {
           mp = msg; 
           for (j=0; j<=i; j++)
               {
               sprintf (mp," %10f ",*(a1+(i*n1)+j));
               mp=(mp+11);
               }
           zvmessage(msg,"");
       }
       zvmessage("","");

       sprintf (msg,"VALUE OF DETERMINANT = %12.8f\n",d1);
       zvmessage(msg,"");
   }

/************************************************************************/
/*   Main Test routine for the "C" call to minv.   This routine builds  */
/*   an array with a determine of one and invokes the "C" bridge zminv. */
/*   The bridge will reverse the rows and columns prior to invoking     */
/*   minv.  After minv has executed, the bridge will again reverse the  */
/*   rows and columns.  This reversal is done because of the row versus */
/*   columns major between FORTRAN and "C" for multi-dimentioned arrays.*/
/*   The second and third arrays are to test the reversal logic in the  */
/*   bridge as array a is identical in row and column.                  */
/************************************************************************/

void FTN_NAME(tzminv)() 
   {
       float a[4][4];
       float c[4][4];
       float e[4][4];

       float b[4], z[4], d, *dp, *mp, *wp1, *wp2;
       int n;
       n=4;

       /* initialize the arrays: array a is a square array with     */
       /* a determinant of 1, while array c is an arbritrary array  */
       /* and array e is array c with row and column reversal       */

       a[0][0] = 5; a[0][1] = 7; a[0][2] = 6; a[0][3] = 5; /* row 0 */
       a[1][0] = 7; a[1][1] =10; a[1][2] = 8; a[1][3] = 7; /* row 1 */
       a[2][0] = 6; a[2][1] = 8; a[2][2] =10; a[2][3] = 9; /* row 2 */
       a[3][0] = 5; a[3][1] = 7; a[3][2] = 9; a[3][3] =10; /* row 3 */

       c[0][0] = 6; c[0][1] = 4; c[0][2] = 6; c[0][3] = 5; /* row 0 */
       c[1][0] = 7; c[1][1] = 2; c[1][2] = 3; c[1][3] = 4; /* row 1 */
       c[2][0] = 4; c[2][1] = 2; c[2][2] = 1; c[2][3] = 5; /* row 2 */
       c[3][0] = 6; c[3][1] = 6; c[3][2] = 6; c[3][3] = 6; /* row 3 */

       e[0][0] = 6; e[0][1] = 7; e[0][2] = 4; e[0][3] = 6; /* row 0 */
       e[1][0] = 4; e[1][1] = 2; e[1][2] = 2; e[1][3] = 6; /* row 1 */
       e[2][0] = 6; e[2][1] = 3; e[2][2] = 1; e[2][3] = 6; /* row 2 */
       e[3][0] = 5; e[3][1] = 4; e[3][2] = 5; e[3][3] = 6; /* row 3 */

       /* Test with matrix a */

       zvmessage("DETERMINANT = 1 MATRIX","");

       mp  = &a[0][0];
       wp1 = &b[0];
       wp2 = &z[0];
       dp  = &d;

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       /* Test with matrix c */

       zvmessage("ARBRITRARY MATRIX","");

       mp = &c[0][0];

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       /* Test with matrix e which is opposite c  */

       zvmessage("ROW/COLUMN REVERSED ARBRITRARY MATRIX","");

       mp = &e[0][0];

       tzminvmat(mp,n);      /* print the matrix  */

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */

       zvmessage("NOW INVERT BACK TO ORIGINAL","");

       zminv(mp,n,dp,wp1,wp2);   /* invoke the bridge  */

       tzminvprt(mp,n,d);    /* print the results  */
   }
$!-----------------------------------------------------------------------------
$ create tminv.imake
/* Imake file for Test of VICAR subroutine MINV */

#define PROGRAM tminv

#define MODULE_LIST tminv.f tzminv.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tminv.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstminv.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tminv
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create minv.hlp
1 MINV


  FORTRAN Calling Sequence:  CALL MINV(A,N,D,L,M)
  C Calling Sequence:        zminv(a,n,d,l,m); /* n is passed by value */

  Arguments: 
		A (input)	REAL N by N input matrix.
		N (input)	order of matrix A
		D (output)	resultant determinant
		L (input)	Work vector of length N
		M (input)	Work vector of length N

2 History

  Original Programmer: Steve Pohorsky
  Current Cognizant Programmer: Steve Pohorsky
  Made portable for UNIX  RNR(CRI)   02-MAY-94 
  Removed LIB_LOCAL from imake file as per FR85780  (CRI)  07-MAR-95

  Source Language: Fortran

2 Operation

  This subroutine uses MATH77 routines (from LINPACK) SGEFA, SGED, and SGEI
  to invert the matrix and calculate the determinant.  The 'C' bridge will
  interchange row and column before and after the minv call.  This is done
  because a FORTRAN matrix is column-major and 'C' is row-major order.
	
2 Arguments

	A (input/Output) REAL N by N input matrix. On return, matrix contains
			 the resulting inverse.
	N (input)	 order of matrix A
	D (output)	 resultant determinant. D=0 if A is singular.
	L (input)	 Work vector of length N
	M (input)	 Work vector of length N

     A, D, L, M are REAL*4    N is INTEGER*4

     N is passed by value for zminv.

2 Examples

     FORTRAN  
	REAL*4 A(4,4), L(4), M(4), D
	INTEGER*4 N
        DATA N/4/

	CALL MINV (A,N,D,L,M)  ! Invert matrix A and calculate determinant
     C
        float a[4][4], l[4], m[4], d
        int n;
        n=4;
        zminv (a,n,&d,l,m);  /* Invert matrix a and calculate determinant */
$ Return
$!#############################################################################
