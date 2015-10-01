$!****************************************************************************
$!
$! Build proc for MIPL module rft2
$! VPACK Version 1.9, Thursday, March 19, 1998, 13:32:13
$!
$! Execute by entering:		$ @rft2
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
$ write sys$output "*** module rft2 ***"
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
$ write sys$output "Invalid argument given to rft2.com file -- ", primary
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
$   if F$SEARCH("rft2.imake") .nes. ""
$   then
$      vimake rft2
$      purge rft2.bld
$   else
$      if F$SEARCH("rft2.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake rft2
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @rft2.bld "STD"
$   else
$      @rft2.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create rft2.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack rft2.com -
	-s rft2.f zrft2.c -
	-i rft2.imake -
	-t trft2.f tzrft2.c trft2.imake trft2.pdf tstrft2.pdf -
	-o rft2.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create rft2.f
$ DECK/DOLLARS="$ VOKAGLEVE"
c*****************************************************************************
c
c	Subroutine RFT2
c
c Brute force utilization of sri's fft and realtr packages
c to get a real, 2-d, in-main transform.
c
c M and N are considered to be the line and sample dimensions
c of the given real array, respectively. Samples are adjacent
c in main store so N is the first fortran dimension.
c
c The real transform is taken in the line direction. This has
c the advantage that the extra space required for the transform
c due to the represented, but zero, sine coefficients of dc
c and the highest frequency is tacked on at the end of the array
c rather than at the end of each line. Thus the dimensions can
c be more or less consistant everywhere. It has the disadvantage
c that the real and imaginary parts of each complex number of the
c transform are not adjacent, but are separated by one line.
c
c       2 june 93  M. O'Shaughnessy    ported to UNIX
c      22 sept 83  ...cca...           convert to vax 
c      15 june 77  ..jek..             initial release
c*****************************************************************************
      subroutine rft2(a,m,n,isn,status)
      integer*4 m,n,isn,status,m2,mp2,n2
      real*4    a(n,m)

c a - the matrix to be transformed. Dimensions: a(n,m+2)
c m - number of lines
c n - number of samples
c isn - flag to do inverse transform.
c status - new parm to replace old calculated label statements
c m2  - half the value of m
c mp2 - m + 2
c n2  - double the value of n
      status = 1 !default status = success
  
      m2 = m/2

c If the dimension isn't even in the real transform direction, return an error.
      if (2*m2 .ne. m) then
         call xvmessage(
     +    'RFT2> dimension of the input matrix is not even!',' ')
         status = -3
         return
      endif

      mp2 = m + 2
      n2  = 2*n
      if (isn .lt. 0) go to 200

c********************
c forward transform..
c********************
      do 120 i=1,n
        call dfft(a(i,1),a(i,2),m2,m2,m2,n2,*950,*960)
        call realtr(a(i,1),a(i,2),m2,n2)
120   continue

      do 140 i=2,mp2,2
        call dfft(a(1,i-1),a(1,i),n,n,n,1,*950,*960)
140   continue

      return

c************************
c the inverse transform..
c************************
200   continue

      do 220 i=2,mp2,2
        call dfft(a(1,i-1),a(1,i),n,n,n,-1,*950,*960)
220   continue

      do 240 i=1,n
        call realtr(a(i,1),a(i,2),m2,-n2)
        call dfft(a(i,1),a(i,2),m2,m2,m2,-n2,*950,*960)
240   continue

      return

c************ Returns *******************************************************
c return (1) if m or n has too large a prime factor..
950   call xvmessage(
     + 'RFT2> a dimension of the input matrix has too large a ' //
     + 'prime factor!',' ')
      status = -1
      return
c
c return (2) if the product of the square-free factors of m or n is too large..
960   call xvmessage(
     + 'RFT2> product of one of the square-free factors of matrix ' //
     + 'dimensions is too large!',' ')
      status = -2
      return
      end
c***end module***************************************************************

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zrft2.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version of RFT2                                         */
/************************************************************************/

void zrft2(matrix,nl,ns,flag,status)
void  *matrix;  /* input matrix; 2D array of float      (input/output) */
int   nl;       /* number of lines in the matrix (M)        (input) */
int   ns;       /* number of samples in the matrix (N)      (input) */
int   flag;     /* flag to do forward/reverse transform (input) */
int   *status;  /* error statuses for the situation where:
		  - the dimension of the input matrix in the
                   real direction, isn't even.  (status = -3)
                  - one of the dimensions of the input matrix
                   has too large a prime factor. (status = -2)
		   -one of the square-free factors of the 
                   matrix dimensions is too large. (status = -1)
                             -or-
                  -successful operation (status=1) */
{
FTN_NAME(rft2)(matrix,&nl,&ns,&flag,&status);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create rft2.imake
/* Imake file for VICAR subroutine RFT2 */

#define SUBROUTINE rft2

#define MODULE_LIST rft2.f zrft2.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport
$ Return
$!#############################################################################
$Test_File:
$ create trft2.f
C*****************************************************************************
C Unit test program TRFT2.F for subroutine RFT2
C input image (gen'd) is real*4.
C
C Ported to UNIX 6/3/1993
C*****************************************************************************
        include 'VICMAIN_FOR'
        subroutine main44

        dimension a(16,18)
        integer*4 n,m,iunit,stat,status,mp2
        real*4    s

C FORTRAN-callable
        call xvmessage('**** FORTRAN-callable RFT2 ****',' ')
        m = 16
	n = 16
        call xvunit(iunit,'inp',1,stat,' ')
        call xvopen(iunit,stat,' ')
        do i=1,m
           call xvread(iunit,a(1,i),stat,'line',i,'nsamps',n,' ')
        end do
     
c       ***********************************************
c       forward transform..
c       Resulting transform contained in lines m=1 to 18

        call xvmessage('before forward transform:',' ')
	mp2 = m + 2
	do 110 i=1,m
110	call prnt(7,n,a(1,i),' ')

	call rft2(a,m,n,1,status)
        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after forward transform:',' ')
	do 130 i=1,mp2
130	call prnt(7,n,a(1,i),' ')

c       ***********************************************
c       the inverse transform..

	call rft2(a,m,n,-1,status)
        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after reverse transform:',' ')       
	s = 2*m*n

	do 160 i=1,m
	call divv(7,n,s,a(1,i),0,1)
160	call prnt(7,n,a(1,i),' ')

      call xvclose(iunit,stat,' ')

c ****************************************************************
c C-callable
        call xvmessage('**** C-callable RFT2 ****',' ')
        m = 16
	n = 16
        call xvunit(iunit,'inp',1,stat,' ')
        call xvopen(iunit,stat,' ')
        do i=1,m
           call xvread(iunit,a(1,i),stat,'line',i,'nsamps',n,' ')
        end do
     
c       ***********************************************
c       forward transform..
c       Resulting transform contained in lines m=1 to 18

        call xvmessage('before forward transform:',' ')
	mp2 = m + 2
	do 210 i=1,m
210	call prnt(7,n,a(1,i),' ')

	call tzrft2(a,m,n,1,status)

        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after forward transform:',' ')
	do 230 i=1,mp2
230	call prnt(7,n,a(1,i),' ')

c       ***********************************************
c       the inverse transform..

	call tzrft2(a,m,n,-1,status)

        if (status.ne.1) then
          if (status.eq.1) goto 3
          if (status.eq.2) goto 4
          if (status.eq.3) goto 5
        endif

        call xvmessage('after reverse transform:',' ')       
	s = 2*m*n

	do 260 i=1,m
	call divv(7,n,s,a(1,i),0,1)
260	call prnt(7,n,a(1,i),' ')

      call xvclose(iunit,stat,' ')
      return
c**************************************************************
3	call xvmessage('m or n has too large a prime factor',' ')
	return
4	call xvmessage(
     +    'product of square-free factors of m or n too big',' ')
	return
5	call xvmessage('number of lines must be even',' ')
	return

      end
$!-----------------------------------------------------------------------------
$ create tzrft2.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* Unit test C-bridge for TRFT2.F */
/************************************************************************/
void FTN_NAME(tzrft2) (matrix,nl,ns,flag,status)
void  *matrix;  /* input matrix; 2D array of float      (input/output) */
int   *nl;       /* number of lines in the matrix        (input) */
int   *ns;       /* number of samples in the matrix      (input) */
int   *flag;    /* flag to do forward/reverse transform (input) */
int   *status; 
{
      zrft2(matrix,*nl,*ns,*flag,*status);
}



$!-----------------------------------------------------------------------------
$ create trft2.imake
/* Imake file for Test of VICAR subroutine rft2 */

#define PROGRAM trft2

#define MODULE_LIST trft2.f tzrft2.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB
/* #define LIB_LOCAL must be removed before delivery, but used during */
/* developer testing on VAX and SUN.                                  */ 

$!-----------------------------------------------------------------------------
$ create trft2.pdf
!*****************************************************************************
! TRFT2.PDF - pdf for test program TRFT2.F for the subroutine RFT2
!*****************************************************************************
process
parm inp string
end-proc
$!-----------------------------------------------------------------------------
$ create tstrft2.pdf
!****************************************************************************
! TSTRFT2.PDF, unit test procedure for subroutine RFT2.F
!****************************************************************************
procedure help=*
refgbl $echo
refgbl $syschar
parm inp string count=0:1 default=--
body
let _onfail="continue"
let $echo="yes"

if ($count(inp)=0) let inp = "a"
gen out=&inp nl=18 ns=16 format=real
trft2 inp=&inp
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create rft2.hlp
1  RFT2

     RFT2 computes a transform of a doubly-subscripted array of real numbers
     or an inverse transform whose result is such an array.  The transform
     is similar to a Fourier transform except that the redundant Fourier 
     coefficients resulting from the fact that the data are real are not
     computed nor is space provided for them in the transform format.  This 
     results in nearly a factor of two savings in space requirements and
     also a savings in computer time as compared with a complex Fourier
     transform.

2  CALLING SEQUENCE

     FORTRAN calling sequence and parameters:
      
         call rft2(matrix,nl,ns,invflag,status)

     Where:
	 real*4    matrix   - 2D matrix to be transformed     (input/output)
         integer*4 nl       - number of lines in the matrix   (input)
         integer*4 ns       - number of samples in the matrix (input)
         integer*4 invflag  - Flag:			      (input)
				1 for forward transform 
                               -1 for inverse transform
         integer*4 status   - Operational status:
                              1 : success
		             -3 : the dimension of the input matrix in the
                                  real direction, isn't even.
                             -2 : one of the square-free factors of the 
                                  matrix dimensions is too large.
                             -1 : one of the dimensions of the input matrix
                                  has too large a prime factor.


               
    C calling sequence and parameters:

         rft2(matrix,nl,ns,invflag,status);

     Where:
       	 float matrix;   - 2D matrix to be transformed     (input/output)
         int   nl;       - number of lines in the matrix   (input)
         int   ns;       - number of samples in the matrix (input)
         int   invflag;  - Flag:			   (input)
				1 for forward transform 
                               -1 for inverse transform
         int   status;   - Operational status:
                              1 : success
		             -3 : the dimension of the input matrix in the
                                  real direction, isn't even.
                             -2 : one of the square-free factors of the 
                                  matrix dimensions is too large.
                             -1 : one of the dimensions of the input matrix
                                  has too large a prime factor.


                             * * *

     The array dimensions required for an m-line, n-sample transform are 
     provided in Fortran with

                 REAL A(n,m+2).

     The number of lines, m, must be even.  To compute the transform

                 CALL RFT2(A,M,N,1,STATUS).

     The initial content of the last two lines is immaterial.  In the
     resulting transform, adjacent pairs of real lines may be considered
     to be single lines of complex numbers with their real parts
     appearing in the first line of the pair and corresponding imaginary
     parts in the second.   The first and last complex lines correspond
     to vertical dc and maximum frequency and exhibit conjugate symmetry
     in the sample direction.  The whole may be viewed as the upper half
     of the full complex Fourier transform with dc in the upper left
     corner.

     To compute the inverse transform of an array initially in this format
     (conjugate symmetry in first and last complex lines),

                 CALL RFT2(A,M,N,-1,STATUS).

     The resulting array of real numbers fills the first m lines of A.
     The last two lines of A should be ignored.  Scaling is such that the 
     successive use of the two transforms multiplies everything by 2.m.n.

     In both the forward and the inverse cases, the alternate returns are
     caused by various problems with factoring M or N.

2  HISTORY

     Original Programmer: John E. Kreznar
     Current Cognizant Programmer: John E. Krezner
     Source Language: Fortran, C for the C-bridge
     Revision: New,          15 April 1977
               Port to UNIX, 8 June 1993, M. O'Shaughnessy

2  OPERATION


     RFT2 uses existing general purpose mixed-radix fast Fourier transform
     routines RCSFFT and REALTR.  The reader is directed to the documenta-
     tion for these subroutines for details such as range of acceptable
     line and sample counts and dependence of timing on these values.

     Given the general purpose tools RCSFFT and REALTR, completing the 
     description of RFT@ is largely a matter of defining data formats.
     The subscript conventions used to describe these formats are as
     follows:

        o  When using a subscript (x,y) in an algebraic expression, we
           associate the first component, x, with line number increasing 
           downward and the second, y, with sample number increasing to 
           the right.  This provides a right-handed coordinate system
           which is consistent with image processing practice.

        o  It is image processing convention that, when an image is
           stored on a secondary storage medium such as disk or tape,
           consecutive locations on the secondary medium are occupied
           by consecutive samples in a given line of the image, with
           consecutive lines following one another.  From a performance
           point of view, it is essential to preserve the adjacency of
           samples within a line in the main store representatin of
           images as well, at least in arrays that are used as input/
           output buffers.  This means that if Fortran double subscript-
           ing is used to address such an array, sample must correspond
           to the first Fortran subscript and line to the second because
           of the USA Standard Array Element Successor Function of 
           Fortran.  This places the Fortran subscripts in the opposite
           order from the "right-handed" convention above.

        o  Finally, in Fourier-transform work, it is most natural to con-
           sider the subscript of an n-element array as ranging over 
           0,1,2,...,n-1.  However, USA Standard Fortran requires that
           Fortran subscripts be positive, thereby excluding zero.
           Therefore, a consistent difference of 1 occurs in this document
           between subscripts appearing in Fortran expressions and the
           corresponding subscripts appearing elsewhere.

3 MATHEMATICAL_DEFINITION

      The function of RFT2 may be defined as follows.  Let m and n be the 
      number of lines and samples, respectively, in the signal (image)
      array.  m must be even and, in addition, both m and n must meet
      the factorability criteria defined in the "restrictions" paragraph.

      Let {f(jk)},j=0,1,2,...,m-1;k=0,1,2,...,n-1 be the signal (image)
      array and {g(jk)},j=0,1,2,...,m/2; k=0,1,...,n-1 be the transform
      array.  Each f(jk) is a real number and each g(jk) is a complex
      number.  Note that the number of lines in {g} is one-half that
      in {f}, plus one.

      The forward transform

                     CALL RFT2(f,m,n,1,&x,&y,&x)

      causes f to overlaid by g, where

                    m-1    n-1
                    _____  _____
         g(jk) = 2  \      \     f(pq)exp( 2(PI)ijp/m + 2(PI)ikq/n ).
                    /____  /____ 
                    p=0    q=0

      Note that the first and last lines of g exhibit conjugate symmetry 
      in the sample direction, i.e.,
                       
         g(0k) = g*(0,n-k),k=1,2,...,n-1; g(00) = g*(00)
      and                                                                 
         g(m/2,k) = g*(m/2),(n-k),k=1,2,...,n-1; g*(m/2,0) = g*(m/2,0).

      For any m/2 - 1 xn complex array g having conjugate symmetry in the 
      first and last lines, the inverse transform 

                           CALL RFT2(g,m,n,-1,&x,&y,&z)
                                
      causes g to e overlaid by f, where

                    m 
                    - _ 1
                    2       n-1
                    -----   -----
          f(jk) =   \       \        g(pq)exp( -2(PI)ijp/m - 2(PI)ikq/n )
                    /____   /____
                    p=0     q=0

      
                 m-1     n-1
                 -----   -----                   
              +  \       \       g*(m-p,q)exp( -2(PI)ijp/m + 2(PI)ikq/n )
                 /____   /____  
                     m   q=0
                 p = -
                     2
                  
       f is real.  If g was obtained by taking the forward RFT2 transform 
       of an array F, then scaling is such that

                 f = 2mnF

3 ERROR_RETURNS

       The significance of the three error returns, &x, &y,and &z, is the
       same in both the forward and inverse cases.  Control is tranferred
       to the corresponding statement numbers in the caller's program as
       follows:

              x - A prime factor of m or n exceeds 23;

              y - The square-free portion of m/2 or n has two or more
                  factors and exceeds 210, or, the number of prime factors
                  of m/2 or n exceeds 208;

              z - m is odd.

3 TRANSFORM_FORMAT

       The following array illustrates the signal and transform formats,
       i.e., the arrangement of the elements in memory, for the case m=4,
       n=3.  Each square corresponds to one machine word.  The four 
       notations in each square are:

         Corner                    Definition
       __________               ________________

       Upper Left              Signal array element
       Upper Right             Transform array element
       Lower Left              Fortran double subscript (dimensions = 3,6)
       Lower Right             Fortran single subscript

        ____________________________________________________
        | f(00)  Rea(00) | f(01)  Rea(01) | f(02)  Rea(02) |
        |                |                |                |
        |   11        1  |   21        2  |   31        3  |
        |________________|________________|________________|
        | f(10)  Img(00) | f(11)  Img(01) | f(12)  Img(02) |
        |                |                |                |
        |   12        4  |   22        5  |   32        6  |
        |________________|________________|________________|
        | f(20)  Rea(10) | f(21)  Rea(11) | f(22)  Rea(12) |
        |                |                |                |
        |   13        7  |   23        8  |   33        9  |
        |________________|________________|________________|
        | f(30)  Img(10) | f(31)  Img(11) | f(32)  Img(12) |
        |                |                |                |
        |   14       10  |   24       11  |   34       12  |
        |________________|________________|________________|
        | ----   Rea(20) | ----   Rea(21) | ----   Rea(22) |
        |                |                |                |
        |   15       13  |   25       14  |   35       15  |
        |________________|________________|________________|
        | ----   Img(20) | ----   Img(21) | ----   Img(22) |
        |           (=0) |                |                |
        |   16       16  |   26       17  |   36       18  |
        |________________|________________|________________|

       (It might be noted that there is at least one natural fromat that
       might have been used instead.  Roughly stated, the alternative
       format has the role of line and sample reversed so that the left
       half, rather than the upper half, of the full complex transform
       is retained.  This format has the advantage that the real and 
       imaginary parts of each complex element of the transform are ad-
       jacent in memory, thereby enabling the complex number to be refer-
       enced as such.  It has the disadvantage however, that the extra
       space required for the transform is not added on at the end, but
       requires two extra words in each line; this difference of two,
       unlike that in the line direction, can not be suppressed and will
       propagate up to the highest level routine utilizing the array, at
       each stage triggering mental gymnastics and probably, arithmetic
       performed on the array dimensions.  In addition to these clear
       pros and cons, two other tradoffs occur which are a priori neutral:
       First, the "even restriction" shifts from the line direction to
       the sample direction.  Second, the storage required for the RFT2
       format is n(m+2) while that required for the alternative is
       m(n+2); if m and n are not equal, the total space requirement is 
       different.)

       (RFT2 is a very small piece of code.  The user interested in the
       alternative format should have little trouble coding it up for
       himself, using RFT2 as a guide.)

3 TIMING

       For other than the smallest values of m or n, the bulk of the time
       spent by RFT2 is in RCSFFT, with small amounts also going to REALTR.
       In both the froward and inverse modes, RFT2 makes n calls to RCSFFT
       with RCSFFT N=m/2, m/2 calls on RCSFFT with RCSFFT N=n, and n calls
       on REALTR with REALTR N=m/2.  The timings for RCSFFT and REALTR may 
       be obtained from the related document.  The time for RCSFFT may be 
       roughly approximated as 20 micro sec X n.sum(n(i)), where the n(i)
       are the prime factors of n.  Thus both m and n should be highly 
       composite numbers if possible.

3 RESTRICTIONS
             
       The number of lines, m, must be even.  In addition, both (m/2) and n
       must meet the restrictions of RCSFFT.  The are that no prime factor
       may exceed 23, that if the square-free portion has two or more
       factors, it may not exceed 210, and that the number of prime factors
       may not exceed 208.  The RCSFFT restrictions may be relaxed by
       changing certain array sizes and recompiling; see the document.

3 EXAMPLE

       The following subroutine computs the cyclic convolution of F and X
       and saves the result in X:

			SUBROUTINE CONV2(F,X,M,N,*)
			DIMENSION F(N,M),X(N,M)
			CALL RFT2(F,M,N,1,*3,*4,*5)
			CALL RFT2(X,M,N,1)
			M2 = M+2
			S = 1. / (4*M*N)
			DO 2 J = 2,M2,2
			DO 1 K = 1,N
 
		   C  REPLACE ONE COMPLEX ELEMENT OF THE TRANSFORM OF

		   C  X WITH THE APPROPRIATELY SCALED PRODUCT OF CORRESPONDING

		   C  ELEMENTS OF F AND X. . .

				T	= S*(F(K,J-1)*X(K,J-1) - F(K,J)*X(K,J))
				X(K+J)  = S*(F(K,J-1)*X(K,J) + F(K,J)*X(K,J-1))
				X(K,J-1)=T
			1	CONTINUE
			2	CONTINUE	
				CALL RFT2 (X,M,N,-1)
				RETURN
			3	CONTINUE
			4	CONTINUE
			5	CONTINUE
				RETURN 1
				END

       This subroutine was coded up as an exercise and compared against a 
       "brute-force" convolution routine, also in Fortran, using 64x64
       arrays.  The RMS difference in the answers was less than 10E-5 times
       the RMS of the element values.  The brute-force routine took 71.77
       CPU seconds; CONV2 took 1.07 CPU seconds.


$ Return
$!#############################################################################
