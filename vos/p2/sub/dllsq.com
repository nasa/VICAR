$!****************************************************************************
$!
$! Build proc for MIPL module dllsq
$! VPACK Version 1.9, Monday, December 07, 2009, 16:11:47
$!
$! Execute by entering:		$ @dllsq
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
$ write sys$output "*** module dllsq ***"
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
$ write sys$output "Invalid argument given to dllsq.com file -- ", primary
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
$   if F$SEARCH("dllsq.imake") .nes. ""
$   then
$      vimake dllsq
$      purge dllsq.bld
$   else
$      if F$SEARCH("dllsq.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake dllsq
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @dllsq.bld "STD"
$   else
$      @dllsq.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create dllsq.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack dllsq.com -mixed -
	-s dllsq.f zdllsq.c -
	-i dllsq.imake -
	-t tdllsq.f tzdllsq.c tdllsq.imake tdllsq.pdf tstdllsq.pdf -
	-o dllsq.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create dllsq.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE DLLSQ(A,B,M,N,L,X,IPIV,EPS,IER,AUX)

C 4 April 1994 ... CRI ... MSTP S/W Conversion (VICAR Porting)

C Calculates the least squares solution using the MATH77 routine DHFTI.

      DOUBLE PRECISION A(M,N),B(M,L),X(N,L),AUX(N,2),EPS
      INTEGER M,N,L,IER, IPIV(N)

C LOCAL ARRAYS AND VARIABLES.

      INTEGER I,J                       ! Loop control variables
      INTEGER RN_DIM
      PARAMETER (RN_DIM = 500)          ! MAX NUMBER OF RIGHT HAND SIDES
                                        ! = MAX VALUE FOR L.
      DOUBLE PRECISION RNORM(RN_DIM)
      INTEGER KRANK
      CHARACTER*132 CBUF
C==================================================================
      ! If the number of right-hand sides passed to DLLSQ exceeds the
      ! number of elements provided by DLLSQ then WRITE output error
      ! message and terminate
      IF (L .GT. RN_DIM) THEN
          WRITE (CBUF, 90100)
90100     FORMAT ('LOCAL ARRAYS IN SUBR DLLSQ NEED TO BE ENLARGED')
          CALL XVMESSAGE (CBUF, ' ')
          CALL ABEND
      END IF

C  CALL MATH77 ROUTINE TO SOLVE LEAST SQUARES PROBLEM

      IF ( M .GE. N ) THEN

         CALL DHFTI( A,M,M,N, B,M,L, EPS, KRANK, RNORM, AUX(1,1),
     &               AUX(1,2), IPIV )

         IF (KRANK .EQ. 0)  THEN
             IER = - 1

         ELSE IF (KRANK .LT. N)  THEN
             IER = KRANK

         ELSE

           IER = 0         ! ERROR UNLESS PSEUDO-RANK = N.

           DO J = 1, L     ! NOW COPY ANSWERS TO OUTPUT ARRAY.
           DO I = 1, N
              X(I,J) = B(I,J)
           END DO
           END DO
         END IF

      ELSE
         IER = -2        ! SINCE DHFTI USES B ARRAY FOR BOTH INPUT AND OUTPUT,
                         ! THERE IS NO CONVENIENT WAY I SEE TO HANDLE THE
                         ! CASE OF N GT M WITH THE DLLSQ CALLING SEQUENCE.

      END IF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zdllsq.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <stdlib.h>

/************************************************************************/
/* C Bridge for DLLSQ  -  calculate least squares solution.             */
/************************************************************************/

int zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX)
void   *A, *B, *X, *AUX;
double  EPS;
int     M, N, L, *IPIV[];
{

double  *from, *to, *old_A, *new_A, *old_B, *new_B, *old_X, *new_X;
int     i, j, status;

   old_A = (double *)A;     
   /* Invert matrix A[M][N] */
   new_A = (double *)malloc (sizeof(double)*M*N);
   for (i = 0; i < M; i++) {
      for (j = 0; j < N; j++) {
         from  = (old_A + (i*N) + j);
         to    = (new_A + (j*M) + i);
         *to   = *from;
      }
   }

   old_B = (double *)B;     
   /* Invert matrix B[M][L] */
   new_B = (double *)malloc (sizeof(double)*M*L);
   for (j = 0; j < L; j++) {
      for (i = 0; i < M; i++) {
         from = (old_B + (i*L) + j);
         to   = (new_B + (j*M) + i);
         *to  = *from;
      }
   }

   new_X = (double *)malloc (sizeof(double)*N*L);
   old_X = (double *)X;

   FTN_NAME2(dllsq, DLLSQ) ( new_A, new_B, &M, &N, &L, new_X, IPIV, &EPS, &status, AUX);

   /* Invert matrix X[N][L] */
   for (i = 0; i < N; i++) {
      for (j = 0; j < L; j++) {
         from = (new_X + (i*L) + j);
         to   = (old_X + (j*N) + i);
         *to  = *from;
      }
   }

   free (new_A);
   free (new_B);
   free (new_X);
   return status;
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create dllsq.imake
/* Imake file for VICAR subroutine dllsq */

#define SUBROUTINE dllsq

#define MODULE_LIST dllsq.f zdllsq.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tdllsq.f
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44
      DOUBLE PRECISION X(2,2), QR(5,3)
      INTEGER I, M, N, IP, J, IFAIL, IPIV(2)
      DOUBLE PRECISION A(3,2) ! Presets: / 1.1,1.2,1.0,  0.9,1.0,1.0 /
      DOUBLE PRECISION B(3,2) ! Presets: / 2.2,2.3,2.1,  4.4,4.6,4.2 /
      CHARACTER*132 TBUF

      DATA A / 1.1,1.2,1.0,  0.9,1.0,1.0/
      DATA B / 2.2,2.3,2.1,  4.4,4.6,4.2  /
 
C  1.1  0.9  2.2
C  1.2  1.0  2.3
C  1.0  1.0  2.1

      M = 3
      N = 2
      IP = 2
      IFAIL = 1


! Call DLLSQ to calculat the least squares solution for test values
      CALL DLLSQ(A,B,M,N,IP,X,IPIV,1.D-15,IFAIL,QR)

! If a failure was detected then print contents of array and terminate
      IF (IFAIL.EQ.0) GO TO 20
      WRITE (TBUF,99996) IFAIL
99996 FORMAT ('ERROR IN DLLSQ IFAIL = ', I2)
      CALL XVMESSAGE (TBUF, ' ') 
      RETURN

! DLLSQ was successful ... print results of least squares computatin 
   20 WRITE (TBUF,90010)
90010 FORMAT ('SOLUTIONS')
      CALL XVMESSAGE (TBUF, ' ') 

      DO 40 J = 1, IP
      DO 40 I = 1, N
      WRITE (TBUF,99920) X(I,J)
99920 FORMAT (F7.4)
      CALL XVMESSAGE (TBUF, ' ') 
40    END DO

C     Test DLLSQ error processing 
      WRITE (TBUF, 90100)
90100 FORMAT ('TEST ERROR HANDLING. SHOULD GET IER=1')
      CALL XVMESSAGE (TBUF, ' ') 
 
C     Set first 24 (6*4) bytes of memory occupied by matrix A to zero
      CALL ZIA(A,6)

      CALL DLLSQ(A,B,M,N,IP,X,IPIV,1.D-15,IFAIL,QR)
      WRITE (TBUF,99996) IFAIL
      CALL XVMESSAGE (TBUF, ' ') 

! Initiate Call to the C bridge for DLLSQ and the DLLSQ bridge test driver
      CALL tzdllsq

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzdllsq.c
#include "xvmaininc.h"
#include "ftnbridge.h"
#include <string.h>
/************************************************************************/
/*                                                               	*/
/************************************************************************/


void FTN_NAME(tzdllsq)() 
{
double X[2][2], AUX[5][3];
int    M, N, L, status, IPIV[2];

double A[3][2];
double B[3][2];
double EPS, *ptr;
char   tbuf [132];
int    i, j;

    /* Assign initial values */
    A[0][0] = 1.1;
    A[1][0] = 1.2;
    A[2][0] = 1.0;
    A[0][1] = 0.9;
    A[1][1] = 1.0;
    A[2][1] = 1.0;

    B[0][0] = 2.2;
    B[1][0] = 2.3;
    B[2][0] = 2.1;
    B[0][1] = 4.4;
    B[1][1] = 4.6;
    B[2][1] = 4.2;

    M     = 3;
    N     = 2;
    L     = 2;

    /* Assign value to EPS */ 
    EPS = 1.e-15;

    /* Call DLLSQ to calculate the least squares solution for test values */
    status = zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX); 

    /* If a failure was detected then print contents of array and terminate */
    if (status != 0) {
        sprintf (tbuf, "ERROR IN DLLSQ IFAIL = %d", status );
        zvmessage (tbuf, "");
        return;
    }

    /* Otherwise DLLSQ was successful ... print contents of array */
    zvmessage ("SOLUTIONS", "");
    for (j = 0; j < L; j++) {
       for (i = 0; i < N; i++) {
            sprintf (tbuf, "%7.4f", X[i][j]);
            zvmessage (tbuf, "");
        }
    } 

    /* Check Error handling */
    zvmessage ("TEST ERROR HANDLING. SHOULD GET IER=1", "");

    /* Set the memory occupied by matrix 'A' to zero */
    ptr = &A[0][0];
    memset (ptr, '\0', sizeof(double));
    ptr = &A[1][0];
    memset (ptr, '\0', sizeof(double));
    ptr = &A[2][0];
    memset (ptr, '\0', sizeof(double));

    /* Assign value to EPS */ 
    EPS = 1.e-15;

    /* Call DLLSQ to calculate the least squares solution for test values */
    status = zdllsq (A, B, M, N, L, X, IPIV, EPS, AUX); 

    /* Print 'IER' results without testing for error return */
    sprintf (tbuf, "ERROR IN DLLSQ IFAIL = %d", status );
    zvmessage (tbuf, "");

    return;
}
$!-----------------------------------------------------------------------------
$ create tdllsq.imake
/* Imake file for Test of VICAR subroutine dllsq */

#define PROGRAM tdllsq

#define MODULE_LIST tzdllsq.c tdllsq.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
$!-----------------------------------------------------------------------------
$ create tdllsq.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstdllsq.pdf
procedure
refgbl $echo
refgbl $autousage
body
let $autousage="none"
let _onfail="continue"
let $echo="yes"
tdllsq
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create dllsq.hlp
1 DLLSQ


  Fortran Calling Sequence:     DLLSQ  (A,B,M,N,L,X,IPIV,EPS,IER,AUX)

  C Calling Sequence:     IER = zdllsq (A,B,M,N,L,X,IPIV,EPS,AUX)

  Arguments: 
		A (input)	double precision M by N coefficient matrix.
                                A is modified by DLLSQ.
		B (input)	double precision M by L right hand side matrix.
                                B is modified by DLLSQ.
		M (input)	row number of matrices A and B
		N (input)	column number of matrix A, row # of matrix X.
                                N must not be greater than M.
		L (input)	column number of matrices B and X
		X (output)	double precision N by L solution matrix
		IPIV (output)	integer output vector of dimension N which
				contains pivitol information.
		EPS (input)	double precision parameter which specifies a
				relative tolerance for determination of rank of
				matrix A.  Generally you can set EPS = 1.D-15.
                                (When dealing with ill conditioned matrices it
                                is best to set EPS to the precision of the
                                computer (1.D-15 for VAX) times the largest
                                element in the matrix.)
		IER (output)	a resulting error parameter.  IER=0 for success.
                                IER must be passed by reference when calling 
                                the C bridge function 'zdllsq'.
		AUX (input)	a dummy double precision auxiliary storage
				array dimension max(2*N,L).
2 History

  Original Programmer: Steve Pohorsky                 26 Jan. 1987
  Current Cognizant Programmer: Steve Pohorsky        26 Jan. 1987
  Dllsq Source Language: Fortran

  Made portable for UNIX (CRI)                         5 Apr. 1994
  Bridge Source Language: C

2 Operation

	  This subroutine uses the MATH77 routine DHFTI.
	
2 Arguments
		A (input)	double precision M by N coefficient matrix.
                                A is modified by DLLSQ.
		B (input)	double precision M by L right hand side matrix.
                                B is modified by DLLSQ.
		M (input)	row number of matrices A and B
		N (input)	column number of matrix A, row # of matrix X.
                                N must not be greater than M.

	A (input)	double precision M by N coefficient matrix.
                        A is modified by DLLSQ.
	B (input)	double precision M by L right hand side matrix.
                        B is modified by DLLSQ.
	M (input)	row number of matrices A and B
	N (input)	column number of matrix A, row # of matrix X.
                        N must not be greater than M.
	L (input)	column number of matrices B and X
	X (output)	double precision N by L solution matrix
	IPIV (output)	integer output vector of dimension N which
			contains pivitol information.
	EPS (input)	double precision parameter which specifies a
			relative tolerance for determination of rank of
			matrix A. Generally you can set EPS = 1.D-15.
                        (When dealing with ill conditioned matrices it
                        is best to set EPS to the precision of the
                        computer (1.D-15 for VAX) times the largest
                        element in the matrix.)
	IER (output)	a resulting error parameter.  IER=0 for success.
                        IER must be passed by reference when calling 
                        the C bridge function 'zdllsq'.
	AUX (input)	a dummy double precision auxiliary storage
			array dimension max(2*N,L).

$ Return
$!#############################################################################
