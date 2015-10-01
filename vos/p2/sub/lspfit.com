$!****************************************************************************
$!
$! Build proc for MIPL module lspfit
$! VPACK Version 1.9, Monday, December 07, 2009, 16:26:14
$!
$! Execute by entering:		$ @lspfit
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
$ write sys$output "*** module lspfit ***"
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
$ write sys$output "Invalid argument given to lspfit.com file -- ", primary
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
$   if F$SEARCH("lspfit.imake") .nes. ""
$   then
$      vimake lspfit
$      purge lspfit.bld
$   else
$      if F$SEARCH("lspfit.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake lspfit
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @lspfit.bld "STD"
$   else
$      @lspfit.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create lspfit.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack lspfit.com -mixed -
	-s lspfit.f zlspfit.c -
	-i lspfit.imake -
	-t tlspfit.f tzlspfit.c tlspfit.imake tlspfit.pdf tstlspfit.pdf -
	-o lspfit.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create lspfit.f
$ DECK/DOLLARS="$ VOKAGLEVE"
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Solve the linear equation SX = U for X where X is the COEF matrix
C and U is the UMOM matrix.  This routine sets up the call for DGELG
C which actually does the work.
C
      SUBROUTINE LSPFIT(M,MOM,UMOM,coef,ier)
      IMPLICIT NONE
      INTEGER M,IER
      REAL*8 MOM(1),UMOM(1)
      REAL*8 S(5151)		!Output S matrix
                                !M <= 100, therfore the max size for S should 
                                !not be greater than 5151.
                                !(i.e. (100+1)(100+2)/2 = 5151)

      REAL*8 COEF(1)		!Output coefficients

      REAL*8 EPS/1.D-15/
      INTEGER*4 NU,IX,IY,IS,IM,J,K,L,JJ,KK

C
C     ...Check for boundary conditions
      IF ((M .LT. 0) .OR. (M .GT. 100)) THEN
          IER = 1 
         CALL MABEND ('Invalid M value.  See lspfit.hlp.')
      END IF

C
C     ....Generate S-array from moments
      IS = 0
C
      DO 20 J=0,M
      DO 20 K=0,J
C
      DO 10 JJ=0,M
      DO 10 KK=0,JJ
      IX = (JJ-KK) + (J-K)		!power of x
      IY = KK + K			!power of y
      L = IX + IY			!degree of term
      IM = (L*(L+1))/2 + IY + 1
      IS = IS + 1
   10 S(IS) = MOM(IM)

   20 CONTINUE
C
      NU = (M+1)*(M+2)/2
      CALL MVE(8,2*NU,UMOM,COEF,1,1)
CCC      CALL PRNT(8,2*NU,COEF,'UMOM=.')
CCC      CALL PRNT(8,NU*NU,S,'S=.')
      CALL DGELG(COEF,S,NU,2,EPS,IER)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Constrained Linear FIT routine CLFIT.  Finds coefficients a,b,e,f such that
C          U = a*X + b*Y + e
C          V =-b*X + a*Y + f
C subject to various constraints on a,b,e, and f.
C
      SUBROUTINE CLFIT(IFIT,MOM,UMOM,coef,ier)
      IMPLICIT NONE
      INTEGER IFIT,IER
      REAL*8 MOM(6),UMOM(6),COEF(6)

      INTEGER ROTATE,SCALE,OFFSET
      REAL*8 N,X,Y,X2,Y2,U,XU,YU,V,XV,YV,A,B,E,F,R,R1,R2
      REAL*8 EPS/1.D-15/
C
C         IFIT  ROTATE SCALE  OFFSET
C          1      0      0      1
C          2      0      1      0
C          3      0      1      1
C          4      1      0      0
C          5      1      0      1
C          6      1      1      0
C          7      1      1      1
C
      IER = -1
      IF (IFIT.LT.0.OR.IFIT.GT.7) RETURN
      IF (MOM(1).LE.1) RETURN
      N = MOM(1)
      X = MOM(2)
      Y = MOM(3)
      X2 = MOM(4)
      Y2 = MOM(6)
      U = UMOM(1)
      XU = UMOM(2)
      YU = UMOM(3)
      V = UMOM(4)
      XV = UMOM(5)
      YV = UMOM(6)
C          INITIALIZE WITH IDENTITY TRANSORMATION
      A = 1.D0
      B = 0.D0
      E = 0.D0
      F = 0.D0
      OFFSET = MOD(IFIT,2)
      SCALE = MOD(IFIT/2,2)
      ROTATE = IFIT/4
      R1 = XU + YV
      R2 = YU - XV
      R = X2 + Y2
      IF (OFFSET.EQ.1) THEN
         R1 = N*R1 - (X*U+Y*V)
         R2 = N*R2 - (Y*U-X*V)
         R = N*R - (X*X+Y*Y)
      ENDIF
      IF(SCALE.EQ.0) R=DSQRT(R1*R1+R2*R2)
      IF (ROTATE+SCALE.GT.0) THEN
         IF (DABS(R).LE.EPS) RETURN
         A = R1/R
         IF (ROTATE.EQ.1) B=R2/R
      ENDIF
      IF (OFFSET.EQ.1) THEN
         E = (U - A*X - B*Y)/N
         F = (V + B*X - A*Y)/N
      ENDIF

      COEF(1) = E
      COEF(2) = A
      COEF(3) = B
      COEF(4) = F
      COEF(5) = -B
      COEF(6) = A
      IER = 0
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute the moments of the x,y and u,v coordinates.
C
      SUBROUTINE MOMENT(NPOW,PTS,N,mom,umom)
      IMPLICIT NONE
      INTEGER NPOW,N
      REAL*4 PTS(4,1)
      REAL*8 MOM(1),UMOM(1)

      INTEGER MPOW,IPOW,NU,NM,I,J,K,L,IP1
      REAL*8 XI,YI,UI,VI,U,V,Z
C
      MPOW = 2*NPOW
      NU = (NPOW+1)*(NPOW+2)/2
      NM = (MPOW+1)*(MPOW+2)/2
      U = 0.D0
      V = 0.D0
      CALL MVE(8,NM,U,MOM,0.,1)
      CALL MVE(8,2*NU,U,UMOM,0.,1)
C
      DO 10 K=1,N
      XI = PTS(1,K)
      YI = PTS(2,K)
      UI = PTS(3,K)
      VI = PTS(4,K)
      U = U + UI
      V = V + VI
      L = 1
C
      DO 10 I=1,MPOW
      IP1 = I + 1
      IPOW = I
      Z = XI**I
      DO 10 J=1,IP1
      L = L + 1
      IF (J.GT.1) THEN
         IF (J.LT.IP1) THEN
            Z = XI**IPOW*YI**(I-IPOW)
         ELSE
            Z = YI**I
         ENDIF
      ENDIF
      MOM(L) = MOM(L) + Z
      IF (I.LE.NPOW) THEN
         UMOM(L) = UMOM(L) + UI*Z
         UMOM(NU+L) = UMOM(NU+L) + VI*Z
      ENDIF
   10 IPOW = IPOW - 1
C
      MOM(1) = MOM(1) + N
      UMOM(1) = UMOM(1) + U
      UMOM(NU+1) = UMOM(NU+1) + V
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute RMS error of polynomial fit.
C
      SUBROUTINE RMSFIT(MODE,NPOW,PTS,N,coef,drms,dmax,imax)
      IMPLICIT NONE
      INTEGER MODE,NPOW,N,IMAX
      REAL*4 PTS(4,1)
      REAL*8 COEF(100),DRMS,DMAX

      INTEGER K
      REAL*8 DU,DV
      REAL*4 X,Y,U,V
C
      DRMS = 0.D0
      DMAX = 0.D0
      IMAX = 0
C
      DO 20 K=1,N
      X = PTS(1,K)
      Y = PTS(2,K)
      CALL POLYTRAN(NPOW,COEF,X,Y,U,V)
      DU = U - PTS(3,K)
      DV = V - PTS(4,K)
      DU = DU*DU + DV*DV
      IF (DMAX.LT.DU) THEN
         DMAX = DU
         IMAX = K
      ENDIF
      DRMS = DRMS + DU
      IF (MODE.EQ.1) THEN
         PTS(3,K) = U
         PTS(4,K) = V
      ENDIF
   20 CONTINUE
C
      DMAX = SQRT(DMAX)
      DRMS = DSQRT(DRMS/N)
      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Compute coordinates (U0,V0) as a polynomial of coordinates (X0,Y0)
C  u(x,y) = a00 + a10*x + a01*y + a20*x**2 + a11*x*y + a02*y**2 + ... + a0m*y**m
C  v(x,y) = b00 + b10*x + b01*y + b20*x**2 + b11*x*y + b02*y**2 + ... + b0m*y**m
C
      SUBROUTINE POLYTRAN(M,COEF,X0,Y0,u0,v0)
      IMPLICIT NONE
      INTEGER M			!Order of the polynomial
      REAL*8 COEF(100)		!Coefficients aij and bij
      REAL*4 X0,Y0,U0,V0	!Input and outputs coordinates

      REAL*8 X,Y,U,V,Z		!All calculations done in double precision
      INTEGER NU,J,K,L
C
      NU = (M+1)*(M+2)/2	!Index to start of V values
      X = X0			!Convert inputs to double precision
      Y = Y0
      U = 0.
      V = 0.
      L = 0
C
      DO 10 J=0,M
      DO 10 K=0,J
      Z = X**(J-K)*Y**K
      L = L + 1
      U = U + COEF(L)*Z
   10 V = V + COEF(NU+L)*Z
C
      U0 = U			!Convert outputs to single precision
      V0 = V

      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zlspfit.c
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
   This file contains a set of C-bridges to LSPFIT subroutines.  
   See lspfit.hlp for more information.

   History:

      April 6, 1998     Thomas Huang     Initial release.
*/
 
#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zlspfit.h"

/*
   This is a C-bridge to the Fotran subroutine LSPFIT, which solves the 
   linear equation SX = U for X where X is the COEF matrix and U is the 
   UMOM matrix.  

   Arguments:
      m        Input; the order of the polynomial.
      mom      Input; an array that  contains sums of products 
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Input; an array that contains sums of products 
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and 
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
      coef     Output; an array that contains the coefficients A(ij) and B(ij).
*/ 
int zlspfit (int m, double *mom, double *umom, double *coef)
{
   int ier;  /* flag to indicate if there is an error. */

   FTN_NAME2(lspfit, LSPFIT) (&m, mom, umom, coef, &ier);

   return ier;
}



/*
   This is a C-bridge to the Fortran subroutine CLFIT, which is a constrained
   Linear FIT routine.  It finds coefficients a,b,e,f such that 
      U =  a*X + b*Y + e
      V = -b*X + a*Y + f
   subject to various constraints on a,b,e and f.

   Arguments:
      ifit     Input; code which specifies the combination of operations 
                  (rotate, scale, offset) to be used in the fit.
      mom      Input; an array that  contains sums of products
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Input; an array that contains sums of products
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
      coef     Output; an array that contains the coefficients A(ij) and B(ij).
*/ 
int zclfit (int ifit, double *mom, double *umom, double *coef)
{
   int ier;  /* flag to indicate if there is an error. */

   FTN_NAME2(clfit, CLFIT) (&ifit, mom, umom, coef, &ier);

   return ier;
}



/*
   This is a C-bridge to the Fortran subroutine MOMENT, which computes 
   the moments of the x,y and u,v coordinates.

   Arguments:
      npow     Input; the order of the polynomial.
      pts      Input; an array of N data points in transposed order, because
                  Fortran matrix is column-major order and C is row-major
                  order.
      n        Input; number of tiepoints in 'pts' array.
      mom      Output; an array that  contains sums of products
                  where S(jk)=SUM(Xi^j * Yi^k), for i = 1..n.
      umom     Output; an array that contains sums of products
                  where U(jk) = SUM(Ui * Xi^j * Yi^k) and
                  V(jk) = SUM(Vi * Xi^j * Yi^k), for i = 1..n.
*/ 
void zmoment (int npow, float *pts, int n, double *mom, double *umom)
{
   FTN_NAME2(moment, MOMENT) (&npow, pts, &n, mom, umom);
}



/*
   This is a C-bridge to the Fortran subroutine RMSFIT, which computes
   the RMS error of polynomial fit.

   Arguments:
      mode     Input; if mode=1, the input (Ui, Vi) values in the 'pts' array
                  are replaced by their computed polynomial values.
      npow     Input; the order of the polynomial.
      pts      Input; an array of N data points in transposed order, because
                  Fortran matrix is column-major order and C is row-major
                  order.
      n        Input; number of tiepoints in 'pts' array.
      coef     Input; An array that contains the coefficients A(ij) and B(ij).
      drmx     Output; RMS error.
      dmax     Output; the maximum error.
      imax     Output; index to 'pts' with the largest error.
*/
void zrmsfit (int mode, int npow, float *pts, int n, 
              double *coef, double *drms, double *dmax, int *imax)
{
   FTN_NAME2(rmsfit, RMSFIT) (&mode, &npow, pts, &n, coef, drms, dmax, imax);
}



/*
   This is a C-bridge to the Fortran subroutine POLYTRAN, which computes
   coordinates (U0, V0) as a polynomial of coordinates (X0, Y0).

   Arguments:
      npow     Input; the order of the polynomial.
      coef     Input; an array that contains the coefficients A(ij) and B(ij).
      x0       Input; x coordinate.
      y0       Input; y coordinate.
      u0       Output; x coordinate.
      v0       Output; y coordinate.
*/
void zpolytran (int m, double *coef, float x0, float y0, float *u0, float *v0)
{
   FTN_NAME2(polytran, POLYTRAN) (&m, coef, &x0, &y0, u0, v0);
}

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create lspfit.imake
/* Imake file for VICAR subroutine LSPFIT */
#define SUBROUTINE lspfit

#define MODULE_LIST lspfit.f zlspfit.c

#define FTN_STRING
#define P2_SUBLIB
#define USES_FORTRAN
#define USES_ANSI_C

$ Return
$!#############################################################################
$Test_File:
$ create tlspfit.f
      INCLUDE 'VICMAIN_FOR' 
C VICAR PROGRAM TLSPFIT
C
      SUBROUTINE MAIN44
      IMPLICIT NONE
      INTEGER N,IFIT, IMAX, IER
      REAL*4 PTS(4,100)
      REAL*8 COEF(12),MOM(36),UMOM(12)
      REAL*8 PI,DTR,RTD, DMAX, DRMS

      CALL XVMESSAGE ('*** Testing Fortran interfaces ***',' ') 
      PI = 3.141592653589793D0
      DTR = PI/180.D0
      RTD = 180.D0/PI

      PTS(1,1) = 1.
      PTS(2,1) = 1.
      PTS(1,2) = 1.
      PTS(2,2) = 400.
      PTS(1,3) = 1.
      PTS(2,3) = 800.

      PTS(1,4) = 400.
      PTS(2,4) = 1.
      PTS(1,5) = 400.
      PTS(2,5) = 400.
      PTS(1,6) = 400.
      PTS(2,6) = 800.

      PTS(1,7) = 800.
      PTS(2,7) = 1.
      PTS(1,8) = 800.
      PTS(2,8) = 400.
      PTS(1,9) = 800.
      PTS(2,9) = 800.
      N = 9		!number of tiepoints

C     ....Test with offset only
      COEF(1) = 1.
      COEF(2) = 1.
      COEF(3) = 0.
      COEF(4) = 1.
      COEF(5) = 0.
      COEF(6) = 1.
      IFIT = 1
      CALL PTRAN(IFIT,COEF,N,pts)
      CALL LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IF (IER .NE. 0) GOTO 999

C     ....Test 2nd order polynomial
      COEF(1) = 6.
      COEF(2) = 5.
      COEF(3) = 4.
      COEF(4) = 3.
      COEF(5) = 2.
      COEF(6) = 1.
      COEF(7) = 12.
      COEF(8) = 11.
      COEF(9) = 10.
      COEF(10) = 9.
      COEF(11) = 8.
      COEF(12) = 7.
   
      IFIT = 9
      CALL PTRAN(IFIT,COEF,N,pts)
      CALL LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IF (IER .NE. 0) GOTO 999
      CALL XVMESSAGE('TLSPFIT task completed',' ')
      CALL XVMESSAGE(' ',' ')
      CALL XVMESSAGE(' ',' ')
      CALL TZLSPFIT()  ! calling C-bridge test
      RETURN
C
  999 CALL XVMESSAGE('***TLSPFIT task cancelled',' ')
      CALL ABEND
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE PTRAN(IFIT,COEF,N,pts)
      IMPLICIT NONE

      INTEGER IFIT,N
      REAL*4 PTS(4,1)
      REAL*8 COEF(20)
      INTEGER NPOW,NU,I
      REAL*4 X,Y,U,V
      LOGICAL*1 BLANK(132)
      CHARACTER*132 MSG
      DATA BLANK/132*' '/
  100 FORMAT(4(F10.1,A1))

      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)
      CALL PRNT(8,NU,COEF,'INITIAL COEF=.')
      DO 20 I=1,N
      X = PTS(1,I)
      Y = PTS(2,I)
      CALL POLYTRAN(NPOW,COEF,X,Y,U,V)
      CALL MVLC(BLANK,MSG(1:132),132)
      WRITE(MSG,100) X,' ',Y,' ',U,' ',V,' '
      CALL XVMESSAGE(MSG,' ')
      PTS(3,I) = U
   20 PTS(4,I) = V

      RETURN
      END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      SUBROUTINE LFIT(IFIT,PTS,N,coef,mom,umom,drms,dmax,imax,ier)
      IMPLICIT NONE

      INTEGER IFIT,N,IMAX,IER
      REAL*4 PTS(4,1)
      REAL*8 COEF(1),MOM(1),UMOM(1),DRMS,DMAX
      INTEGER NPOW,NU
C
      IER = -1
      NPOW = MAX0(IFIT-7,1)
      NU = (NPOW+1)*(NPOW+2)/2
      IF (NU.GT.N) RETURN

      CALL MOMENT(NPOW,PTS,N,MOM,UMOM)
      CALL MVE(8,12,0.D0,COEF,0,1)
      CALL PRNT(4,1,IFIT,'IFIT=.')
      IF (IFIT.LT.8) CALL CLFIT(IFIT,MOM,UMOM,coef,ier)
      IF (IFIT.GE.8) CALL LSPFIT(NPOW,MOM,UMOM,coef,ier)
      CALL RMSFIT (0,NPOW,PTS,N,coef,drms,dmax,imax)
      CALL PRNT(8,2*NU,COEF,'COEF=.')
      CALL PRNT(8,1,DRMS,'RMS=.')
      CALL PRNT(8,1,DMAX,'MAX ERROR=.')
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzlspfit.c
#include "xvmaininc.h"
#include "zlspfit.h"
#include "ftnbridge.h"

#define MAX_ROW 100
#define MAX_COL 4

void zptran (int, double [], int, float [][MAX_COL]);
int zlfit (int, float [][MAX_COL], int, double [], double [], double [], 
           double, double, int);


void FTN_NAME(tzlspfit) (void)
{
   int n, ifit, imax, ier;
   float pts[MAX_ROW][MAX_COL];
   double mom[36], umom[12];
   double pi, dtr, rtd, drms, dmax;

   double coef[12] = 
       {1.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

   zvmessage ("*** Testing C-bridge interfaces ***","");
   pi = 3.141592653589793;
   dtr = pi/180.0;
   rtd = 180.0/pi;
   drms = 0.0;
   dmax = 0.0;
   imax = 0;

   pts[0][0] = 1.0;
   pts[0][1] = 1.0;
   pts[1][0] = 1.0;
   pts[1][1] = 400.0;
   pts[2][0] = 1.0;
   pts[2][1] = 800.0;
 
   pts[3][0] = 400.0;
   pts[3][1] = 1.0;
   pts[4][0] = 400.0;
   pts[4][1] = 400.0;
   pts[5][0] = 400.0;
   pts[5][1] = 800.0;
 
   pts[6][0] = 800.0;
   pts[6][1] = 1.0;
   pts[7][0] = 800.0;
   pts[7][1] = 400.0;
   pts[8][0] = 800.0;
   pts[8][1] = 800.0;

   n = 9;

   ifit = 1;
   zptran (ifit, coef, n, pts);
   if (zlfit (ifit, pts, n, coef, mom, umom, drms, dmax, imax))
      zmabend ("***TZLSPFIT task cancelled.");

   coef[0] = 6.0;
   coef[1] = 5.0;
   coef[2] = 4.0;
   coef[3] = 3.0;
   coef[4] = 2.0;
   coef[5] = 1.0;
   coef[6] = 12.0;
   coef[7] = 11.0;
   coef[8] = 10.0;
   coef[9] = 9.0;
   coef[10] = 8.0;
   coef[11] = 7.0;

   ifit = 9;
   zptran (ifit, coef, n, pts);
   if (zlfit (ifit, pts, n, coef, mom, umom, drms, dmax, imax))
      zmabend ("***TZLSPFIT task cancelled.");

   zvmessage ("TZLSPFIT task completed.","");
   return; 
}


void zptran (int ifit, double coef[], int n, float pts[][MAX_COL])
{
   int npow, nu, i;
   float x, y, u, v;
   float blank[132];
   char msg[132];
 
   for (i=0; i<132; i++)
      blank[i] = 0.;
   npow = ((ifit-7) > 1) ? (ifit - 7) : 1;
   nu = (npow+1) * (npow+2);
   zprnt (8, nu, coef, "INITIAL COEF=.");
   for (i=0; i < n; i++)
   {
      x = pts[i][0];
      y = pts[i][1];
      zpolytran (npow, coef, x, y, &u, &v);
      zmove (blank, msg, 132);
      sprintf (msg, "%10.1f %10.1f %10.1f %10.1f ", x, y, u, v);
      zvmessage (msg, "");
      pts [i][2] = u;
      pts [i][3] = v;
 
   }
}
 
 
int zlfit (int ifit, float pts[][MAX_COL], int n, double coef[], double mom[],
            double umom[], double drms, double dmax, int imax)
{
   int npow, nu, ier;
   double temp=0.0;
 
   ier = -1;
   npow = ((ifit-7) > 1) ? (ifit - 7) : 1;
   nu = (npow+1) * (npow+2) / 2;
   if (nu > n) return ier;
   zmoment (npow, (float *)pts, n, mom, umom);
   zmve (8, 12, &temp, coef, 0, 1);
   zprnt (4, 1, &ifit, "IFIT=.");
   if (ifit < 8)
      ier = zclfit (ifit, mom, umom, coef);
   else
      ier = zlspfit (npow, mom, umom, coef);
   zrmsfit (0,npow,(float *)pts,n,coef,&drms,&dmax,&imax);
   zprnt (8, 2*nu, coef, "COEF=.");
   zprnt (8, 1, &drms, "RMS=.");
   zprnt (8, 1, &dmax, "MAX ERROR=.");
   return ier;
}

$!-----------------------------------------------------------------------------
$ create tlspfit.imake
#define PROGRAM tlspfit

#define MODULE_LIST tlspfit.f tzlspfit.c
                            
#define MAIN_LANG_FORTRAN
#define R2LIB

#define USES_FORTRAN
#define USES_ANSI_C
#define LIB_P2SUB
#define LIB_RTL
#define LIB_TAE
#define LIB_MATH77

/*
#define LIB_LOCAL
*/

$!-----------------------------------------------------------------------------
$ create tlspfit.pdf
process help=*
END-PROC

.TITLE
TSTLSPFIT - test program for subroutine package LSPFIT
.end
$!-----------------------------------------------------------------------------
$ create tstlspfit.pdf
Procedure help=*
Refgbl $echo
refgbl $autousage
Body
let $echo="no"
write  "Test of subroutine lspfit"
tlspfit
End-proc
$ Return
$!#############################################################################
$Other_File:
$ create lspfit.hlp
1 LSPFIT,CLFIT,MOMENT,RMSFIT,POLYTRAN

LSPFIT,CLFIT,MOMENT,RMSFIT, and POLYTRAN comprise a subroutine set for
performing a polynomial fit over two sets of ordered pairs.  Two common
applications are (1) the fitting of tiepoints to compute a distortion model for
registering a pair of images (see program TPFIT) and (2) the measurement of
geometric camera distortions by comparing the position of a set of imaged grid
points to their known locations (see program LOCUS).

Let (Xi,Yi) and (Ui,Vi), i=1,2,3,...,n be two sets of matching tiepoints.  The
transformation mapping each (Ui,Vi) onto the corresponding (Xi,Yi) are the
polynomials U=p(X,Y) and V=q(X,Y), where:

 p(X,Y) = A00 + A10*X + A01*Y + A20*X**2 + A11*X*Y + A02*Y**2 + ... + A0,m*y**m
 q(X,Y) = B00 + B10*X + B01*Y + B20*X**2 + B11*X*Y + B02*Y**2 + ... + B0,m*y**m

The routine LSPFIT performs a least squares fit to determine the coefficients
aij and bij which minimize the error between the polynomials and the actual
points.  The routine CLFIT (Constrained Linear FIT) considers a linear
transformation where the mapping is restricted to a combination of three basic
operations: scale, rotation, and offset.

The routine MOMENT computes statistical data from the tiepoints.  These are
input to LSPFIT or CLFIT to perform the fit.  RMSFIT computes the error in the
fit and optionally performs the transformation (by calling POLYTRAN).

The equations used in this program set are documented in the following memo:
   "Least squares polynomial fit routines", Gary Yagi, March 5, 1998.


FORTRAN calling sequence:

      INTEGER*4 IFIT,M,N,IER,MODE,IMAX
      REAL*4 PTS(4,N),X,Y,U,V
      REAL*8 MOM(N1),UMOM(N2,2),COEF(N2,2),RMS

      CALL LSPFIT(M,MOM,UMOM,coef,ier)
      CALL CLFIT(IFIT,MOM,UMOM,coef,ier)
      CALL MOMENT(M,PTS,N,mom,umom)
      CALL RMSFIT(MODE,M,PTS,N,COEF,rms,dmax,imax)
      CALL POLYTRAN(M,COEF,X,Y,u,v)

  The values N1 and N2, which determine the size of the arrays MOM(N1),
  UMOM(N2,2), and COEF(N2,2) are computed as follows:

                N1 = (M+1)(2M+1)
		N2 = (M+1)(M+2)/2

  where M is the order of the polynomial.

C calling sequence:

      Note:
         Include file name: zlspfit.h

         For the C interfaces, PTS is in Nx4 format (i.e. float pts[N][4];).
         This is because Fortran matrix is in column-major order and C is 
         row-major order.

         ier should be the return value for subroutines zlspfit and zclfit.

      int zlspfit    (int m, double *mom, double *umom, double *coef);
      int zclfit     (int ifit, double *mom, double *umom, double *coef);
      void zmoment   (int m, float *pts, int n, double *mom, double *umom);
      void zrmsfit   (int mode, int m, float *pts, int n, double *coef, 
                      double *rms, double *dmax, int *imax);
      void zpolytran (int m, double *coef, 
                      float x, float y, float *u, float *v);

2 Arguments:


  INTEGER M		Order of the polynomial.  0<= M <=100.

  INTEGER IFIT
	Code which specifies the combination of operations (rotate, scale,
        offset) to be used in the fit.	Let the variables ROTATE, SCALE, and
	OFFSET have values 1 or 0 depending whether that operation is allowed
	or suppressed.  Then IFIT has the value:

	        IFIT  ROTATE SCALE  OFFSET
	          1      0      0      1
	          2      0      1      0
	          3      0      1      1
	          4      1      0      0
	          5      1      0      1
	          6      1      1      0
	          7      1      1      1

	IFIT is a bit map which can be computed as follows:

	        IFIT = 4*ROTATE + 2*SCALE + OFFSET

  REAL*4 PTS(4,N)
	Array of N data points (Xi,Yi) and (Ui,Vi) as follows:
		PTS(1,I)=Xi		PTS(3,I)=Ui
		PTS(2,I)=Yi		PTS(4,I)=Vi
  INTEGER*4 N	Number of tiepoints in PTS array

  REAL*8 MOM(N1),UMOM(N2,2)
	MOM and UMOM contain statistical data about the tiepoints.  These
 	data are computed by routine MOMENT and used in computing the fit by
	routines LSPFIT and CLFIT.

        MOM contains sums of products sjk=SUM(Xi**j*Yi**k), i=1,2,3,...,n
	in the following order:

		s00
		s10    s01
		s20    s11    s02
                s30    s21    s12    s03
		.        .      .      .
		.        .      .      .     .
		.        .      .      .     .     .
                s2m,0  s2m+1,1  .      .     .   s0,2m
	where
 
        UMOM contains sums of products ujk=SUM(Ui*Xi**j*Yi**k) and
	vjk=SUM(Vi*Xi**j*Yi**k), i=1,2,3,...,n, in the following order:

		u00
		u10   u01
		u20   u11    u02
		.       .      .     .
		.       .      .     .    .
		.       .      .     .    .    .
                um,0  um-1,1   .     .    .   u0,m
		v00
		v10   v01
		v20   v11    v02
		.       .      .     .
		.       .      .     .    .
		.       .      .     .    .     .
                vm,0  vm+1,1   .     .    .   v0,m


  REAL*8 COEF(N2,2)
	Contains the coefficients Aij and Bij in the following order:

		A00
		A10   A01
		A20   A11     A02
		 .     .       .      .
		 .     .       .      .     .
		 .     .       .      .     .     .
		Am,0  Am-1,1  Am-2,2  .     .     .  A0,m
		B00
		B10   B01
		B20   B11     B02
		 .     .       .      .
		 .     .       .      .     .
		 .     .       .      .     .     .
		Bm,0  Bm-1,1  Bm-2,2  .     .     .  B0,m

  INTEGER*4 IER		Return status, 0=success
  REAL*8 RMS		RMS error
  INTEGER*4 IMAX	Index to PTS with largest error

  INTEGER*4 MODE
	If MODE=1, the input (ui,vi) values in the PTS array are
	replaced by their computed polynomial values.

  REAL*4 X,Y,U,V
	(X,Y) and (U,V) are the input and output coordinates from a call to
        routine POLYTRAN, where

		U=p(X,Y) and V=q(X,Y)

2 Operation


2 History

  Original Programmer: Gary Yagi, March 5, 1998
  Current Cognizant Programmer: Gary Yagi
  Source Language: F
  Revision history: New

  April 06, 1998      Thomas Huang      Added C-bridges for the subroutines.

$ Return
$!#############################################################################
