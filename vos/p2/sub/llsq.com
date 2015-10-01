$!****************************************************************************
$!
$! Build proc for MIPL module llsq
$! VPACK Version 1.9, Monday, December 07, 2009, 16:25:52
$!
$! Execute by entering:		$ @llsq
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
$ write sys$output "*** module llsq ***"
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
$ write sys$output "Invalid argument given to llsq.com file -- ", primary
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
$   if F$SEARCH("llsq.imake") .nes. ""
$   then
$      vimake llsq
$      purge llsq.bld
$   else
$      if F$SEARCH("llsq.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake llsq
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @llsq.bld "STD"
$   else
$      @llsq.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create llsq.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack llsq.com -mixed -
	-s llsq.f zllsq.c -
	-i llsq.imake -
	-t tllsq.f tzllsq.c tllsq.imake tllsq.pdf tstllsq.pdf -
	-o llsq.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create llsq.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c*****************************************************************************
c Subroutine LLSQ - Calculates the least squares solution using the 
c                   math77 routine shfti.
c
c 12/7/1992 - M. O'Shaughnessy - Ported LLSQ to UNIX.
c*****************************************************************************
      subroutine llsq(a,b,m,n,l,x,ipiv,eps,ier,aux)

      real*4    a(m,n),b(m,l),x(n,l),aux(n,2),eps
      integer*4 m,n,l,ier,ipiv(n)

c local arrays and variables.

      integer*4 rn_dim
      parameter (rn_dim = 500)          ! max number of right hand sides
                                        ! = max value for l.
      real*4    rnorm(rn_dim)
      integer*4 krank,i,j

c==================================================================

      if (l .gt. rn_dim) then
	call xvmessage('LLSQ> local arrays need to be enlarged!', ' ')
        call abend
      end if

c  call math77 routine to solve least squares problem

      if ( m .ge. n ) then
        call shfti(a,m,m,n,b,m,l,eps,krank,rnorm,aux(1,1),
     .             aux(1,2), ipiv )
        if (krank .eq. 0)  then
          ier = - 1
        else if (krank .lt. n)  then
          ier = krank
        else
          ier = 0         ! error unless pseudo-rank = n.
          do j = 1, l     ! now copy answers to output array.
            do i = 1, n
              x(i,j) = b(i,j)
            end do
          end do
        end if

      else
        ier = -2        ! since shfti uses b array for both input and output,
                        ! there is no convenient way i see to handle the
                        ! case of n gt m with the llsq calling sequence.
        call xvmessage('LLSQ> column number must be <= row number',' ')
      end if

      return
      end
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zllsq.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of LLSQ                                           */
/************************************************************************/

void zllsq(a,b,m,n,l,x,ipiv,eps,ier,aux)
void   *a;    /* mxn coefficient matrix             (input) */
void   *b;    /* mxl righthandside matrix           (input) */
int     m;    /* row number of matrices a and b     (input) */
int     n;    /* column number of matrix a, row number of   */
              /*    matrix b. n <= m.               (input) */
int     l;    /* column number of matrices b and x. (input) */
void   *x;    /* nxl solution matrix.              (output) */
void   *ipiv; /* output vector of size n with pivotal info (output) */
double  eps;  /* tolerance for determination of rank (input) */
int    *ier;  /* error parameter, 0=success.        (output) */
void   *aux;  /* dummy auxilliary storage array, dimension max(2*n,l) (input) */
{
  float sigma;
  sigma = (float) eps;
  FTN_NAME2(llsq, LLSQ) (a,b,&m,&n,&l,x,ipiv,&eps,ier,aux);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create llsq.imake
/* Imake file for VICAR subroutine LLSQ */

#define SUBROUTINE llsq

#define MODULE_LIST llsq.f zllsq.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
#define LIB_MATH77
$ Return
$!#############################################################################
$Test_File:
$ create tllsq.f
c*****************************************************************************
c Unit test program TLLSQ.F for subroutine LLSQ
c Ported to UNIX 11/10/1992
c ier=0 indicates success during individual runs...
c*****************************************************************************
      include 'VICMAIN_FOR'
      subroutine main44
      real*4       x(2,2), qr(5,3)
      integer*4    m, n, ip, ifail, ipiv(2)
      real*4       a(3,2) /1.1, 1.2, 1.0, 0.9, 1.0, 1.0/
      real*4       b(3,2) /2.2, 2.3, 2.1, 4.4, 4.6, 4.2/
      real*4       eps/1.e-7/
      character*80 msg

c  1.1  0.9  2.2 
c  1.2  1.0  2.3
c  1.0  1.0  2.1
c*****************************************************************************
c  Fortran test
c*****************************************************************************
      call xvmessage('Testing FORTRAN-callable LLSQ',' ')
      m = 3
      n = 2
      ip = 2
      ifail = 1

      call llsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      if (ifail.eq.0) go to 20
      write (msg,99996) ifail
      call xvmessage(msg,' ')
      return
20    continue
      call xvmessage('output should be 1.301 0.7935 2.602 1.587',' ')
      call prnt(7,4,x,'first run = .')

c  check error handling

      call xvmessage('test error handling. should get ier=1', ' ')
      call zia(a,3)

      call llsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      write (msg,99996) ifail
      call xvmessage(msg,' ')

c*****************************************************************************
c C test
c*****************************************************************************
      call xvmessage('Testing C-callable LLSQ',' ')
      m = 3
      n = 2
      ip = 2
      ifail = 1
      a(1,1) = 1.1
      a(2,1) = 1.2
      a(3,1) = 1.0
      a(1,2) = 0.9
      a(2,2) = 1.0
      a(3,2) = 1.0
      b(1,1) = 2.2
      b(2,1) = 2.3
      b(3,1) = 2.1
      b(1,2) = 4.4
      b(2,2) = 4.6
      b(3,2) = 4.2

      call tzllsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      if (ifail.eq.0) go to 40
      write (msg,99996) ifail
      call xvmessage(msg,' ')
      return
40    continue
      call xvmessage('output should be 1.301 0.7935 2.602 1.587',' ')
      call prnt(7,4,x,'C-callable output = .')

c  check error handling

      call xvmessage('error handling test. should get ier=1', ' ')
      call zia(a,3)

      call tzllsq(a,b,m,n,ip,x,ipiv,eps,ifail,qr)

      write (msg,99996) ifail
      call xvmessage(msg,' ')

      return
99996 format ('error in llsq, ifail =  ' , i2)
      end
$!-----------------------------------------------------------------------------
$ create tzllsq.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TLLSQ.F */
/************************************************************************/
void FTN_NAME(tzllsq) (a,b,m,n,l,x,ipiv,eps,ier,aux)
void   *a;    /* mxn coefficient matrix             (input) */
void   *b;    /* mxl righthandside matrix           (input) */
int    *m;    /* row number of matrices a and b     (input) */
int    *n;    /* column number of matrix a, row number of   */
              /*    matrix b. n <= m.               (input) */
int    *l;    /* column number of matrices b and x. (input) */
void   *x;    /* nxl solution matrix.              (output) */
void   *ipiv; /* output vector of size n with pivotal info (output) */
double *eps;  /* tolerance for determination of rank (input) */
int    *ier;  /* error parameter, 0=success.        (output) */
void   *aux;  /* dummy auxilliary storage array, dimension max(2*n,l) (input) */
{
  zllsq(a,b,*m,*n,*l,x,ipiv,*eps,ier,aux);
}



$!-----------------------------------------------------------------------------
$ create tllsq.imake
/* Imake file for Test of VICAR subroutine llsq */

#define PROGRAM tllsq

#define MODULE_LIST tllsq.f tzllsq.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
#define LIB_MATH77
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 
#define LIB_LOCAL
$!-----------------------------------------------------------------------------
$ create tllsq.pdf
!*****************************************************************************
! TLLSQ.PDF - pdf for test program TLLSQ.F for the subroutine LLSQ
!*****************************************************************************
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstllsq.pdf
!****************************************************************************
! TSTLLSQ.PDF, unit test procedure for subroutine LLSQ.F
!****************************************************************************
procedure help=*
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
write "Test subroutine LLSQ"
TLLSQ
end-proc
.title TSTLLSQ.PDF - unit test for subroutine LLSQ
.help
This unit test creates no files and uses little CPU. Just run it in batch,
with no parameters, and verify the output.
.end
$ Return
$!#############################################################################
$Other_File:
$ create llsq.hlp
1 LLSQ

  LLSQ is a UNIX-ported, FORTRAN and C callable subroutine which calculates the
  least squares solution, using the math77 routine SHFTI.

  Fortran calling Sequence:  LLSQ(A,B,M,N,L,X,IPIV,EPS,IER,AUX)

  Arguments: 
	real*4	  A (input)	single precision M by N coefficient matrix.
                                A is modified by LLSQ.
	real*4	  B (input)	single precision M by L right hand side matrix.
                                B is modified by LLSQ.
	integer*4 M (input)	row number of matrices A and B
	integer*4 N (input)	column number of matrix A, row # of matrix X.
                                N must not be greater than M.
	integer*4 L (input)	column number of matrices B and X
	real*4	  X (output)	single precision N by L solution matrix
	integer*4 IPIV (output)	integer output vector of dimension N which
				contains pivitol information.
	real*4	  EPS (input)	single precision parameter which specifies a
				relative tolerance for determination of rank of
				matrix A.  Generally you can set EPS = 1.E-7.
                                (When dealing with ill conditioned matrices it
                                is best to set EPS to the precision of the
                                computer (1.E-7 for VAX) times the largest
                                element in the matrix.)
	integer*4 IER (output)	a resulting error parameter.  IER=0 for success.
	real*4	  AUX (input)	a dummy single precision auxiliary storage
				array dimension max(2*N,L).

  C calling Sequence:  zllsq(a,b,*m,*n,*l,x,ipiv,*eps,ier,aux);

  Arguments: 
	void	 *a (input)	single precision M by N coefficient matrix.
                                A is modified by LLSQ.
	void	 *b (input)	single precision M by L right hand side matrix.
                                B is modified by LLSQ.
	int      *m (input)	row number of matrices A and B
	int      *n (input)	column number of matrix A, row # of matrix X.
                                N must not be greater than M.
        int      *l (input)	column number of matrices B and X
	void     *x (output)	single precision N by L solution matrix
	void     *ipiv (output)	integer output vector of dimension N which
				contains pivitol information.
	double   *eps (input)	single precision parameter which specifies a
				relative tolerance for determination of rank of
				matrix A.  Generally you can set EPS = 1.E-7.
                                (When dealing with ill conditioned matrices it
                                is best to set EPS to the precision of the
                                computer (1.E-7 will also work for UNIX) times 
                                the largest element in the matrix.)
	int      *ier (output)	a resulting error parameter.  IER=0 for success.
        void     *aux (input)	a dummy single precision auxiliary storage
				array dimension max(2*N,L).
2 History

  Original Programmer: Steve Pohorsky                 26 Jan. 1987
  Current Cognizant Programmer: Steve Pohorsky        26 Jan. 1987
  Source Language: Fortran
  Revisions:
	11 December 1992 - M. O'Shaughnessy.  Ported to UNIX.

2 Operation

	  This subroutine uses the MATH77 routine SHFTI.
	
2 Arguments

   See "help llsq" for arguments of the FORTRAN-callable and C-callable
   subroutines.
$ Return
$!#############################################################################
