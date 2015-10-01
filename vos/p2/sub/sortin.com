$!****************************************************************************
$!
$! Build proc for MIPL module sortin
$! VPACK Version 1.9, Friday, May 04, 2001, 11:37:31
$!
$! Execute by entering:		$ @sortin
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
$ write sys$output "*** module sortin ***"
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
$ write sys$output "Invalid argument given to sortin.com file -- ", primary
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
$   if F$SEARCH("sortin.imake") .nes. ""
$   then
$      vimake sortin
$      purge sortin.bld
$   else
$      if F$SEARCH("sortin.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake sortin
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @sortin.bld "STD"
$   else
$      @sortin.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create sortin.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack sortin.com -mixed -
	-s sortin.f zsortin.c -
	-i sortin.imake -
	-t tsortin.f tzsortin.c tsortin.imake tsortin.pdf tstsortin.pdf -
	-o sortin.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create sortin.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C Routine to sort an INTEGER*4 array
C
      SUBROUTINE SORTIN(BUF,N)
      INTEGER*4 BUF(N)
C
      IF (N.LE.1) RETURN
      L = N
      M = N/2
C
   10 K = M
      IBUF = BUF(K)
C
   20 J = 2*K
      IF (J.GT.N) GOTO 25
      IF (J.LT.N.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 25
      BUF(K) = BUF(J)
      K = J
      GOTO 20
C
   25 BUF(K) = IBUF
      M = M - 1
      IF (M.GT.0) GOTO 10
C
   30 K = 1
      IBUF = BUF(K)
C
   40 J = 2*K
      IF (J.GT.L) GOTO 45
      IF (J.LT.L.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 45
      BUF(K) = BUF(J)
      K = J
      GOTO 40
C
   45 BUF(K) = IBUF
      IBUF = BUF(1)
      BUF(1) = BUF(L)
      BUF(L) = IBUF
      L = L - 1
      IF (L.GT.1) GOTO 30
C
      RETURN
      END
C Routine to sort an INTEGER*2 array, with pointer.
C This is a modification of SORTIN
C
      SUBROUTINE I2SORT(BUF,PTR,N)
      INTEGER*2 BUF(N),PTR(N)
C
      IF (N.EQ.1) RETURN
      L = N
      M = N/2
C
   10 K = M
      IBUF = BUF(K)
      IPTR = PTR(K)
C
   20 J = 2*K
      IF (J.GT.N) GOTO 25
      IF (J.LT.N.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 25
      BUF(K) = BUF(J)
      PTR(K) = PTR(J)
      K = J
      GOTO 20
C
   25 BUF(K) = IBUF
      PTR(K) = IPTR
      M = M - 1
      IF (M.GT.0) GOTO 10
C
   30 K = 1
      IBUF = BUF(K)
      IPTR = PTR(K)
C
   40 J = 2*K
      IF (J.GT.L) GOTO 45
      IF (J.LT.L.AND.BUF(J+1).GT.BUF(J)) J=J+1
      IF (BUF(J).LE.IBUF) GOTO 45
      BUF(K) = BUF(J)
      PTR(K) = PTR(J)
      K = J
      GOTO 40
C
   45 BUF(K) = IBUF
      PTR(K) = IPTR
      IBUF = BUF(1)
      IPTR = PTR(1)
      BUF(1) = BUF(L)
      PTR(1) = PTR(L)
      BUF(L) = IBUF
      PTR(L) = IPTR
      L = L - 1
      IF (L.GT.1) GOTO 30
C
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zsortin.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"
/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zsortin(buf,n)
int *buf;	
int n;		
{
FTN_NAME(sortin)(buf,&n);
}

void zi2sort(buf,ip,n)
short *buf,*ip;
int n;
{
FTN_NAME(i2sort)(buf,ip,&n);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create sortin.imake
/* Imake file for VICAR subroutine SORTIN */

#define SUBROUTINE sortin

#define MODULE_LIST sortin.f zsortin.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create tsortin.f
c  test subroutines SORTIN and I2SORT
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT INTEGER*4 (A-Z)
      INTEGER*4 KEY(10)/2,10,3,8,15,16,1,20,7,19/,K(10)
      INTEGER*4 PTR(10)/1,2,3,4,5,6,7,8,9,10/,P(10)

      N=10
      CALL XVMESSAGE('TEST OF SORTIN',' ')
      CALL MVE(4,N,KEY,K,1,1)
      CALL PRNT(4,N,K,' SORTIN input= .')
      CALL SORTIN(K,N)
      CALL PRNT(4,N,K,' SORTIN output=.')

      CALL XVMESSAGE('TEST OF I2SORT',' ')
      CALL MVE(-6,N,KEY,K,1,1)
      CALL MVE(-6,N,PTR,P,1,1)
      CALL PRNT(2,N,K,' I2SORT input= .')
      CALL I2SORT(K,P,N)
      CALL PRNT(2,N,K,' I2SORT output=.')
      CALL PRNT(2,N,P,' POINTER=.')

      CALL XVMESSAGE('TEST C BRIDGES',' ')
      CALL TZSORTIN
      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzsortin.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
void FTN_NAME(tzsortin)()
{
  int i,n,buf[10];
  short hbuf[10],hptr[10];

  n=10;
  for (i=0; i<n; i++) {
     buf[i] = -1000 * i + 8000;
     hbuf[i] = buf[i];
     hptr[i] = i + 1;
  }

  zprnt(4,10,buf,"Test zsortin: buf=");
  zsortin(buf,n);
  zprnt(4,10,buf," Sorted buf=");

  zprnt(2,10,hbuf,"Test zi2sort: buf=");
  zi2sort(hbuf,hptr,n);
  zprnt(2,10,hbuf,"Sorted buf=");
  zprnt(2,10,hptr,"Pointer=");
}
$!-----------------------------------------------------------------------------
$ create tsortin.imake
/* Imake file for Test of VICAR subroutine sortin */

#define PROGRAM tsortin

#define MODULE_LIST tsortin.f tzsortin.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
/* #define LIB_LOCAL */
$!-----------------------------------------------------------------------------
$ create tsortin.pdf
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstsortin.pdf
! Test of subroutines SORTIN, I2SORT
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tsortin
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create sortin.hlp
1 SORTIN

  The following subroutines are available for sorting an array (BUF)
  in ascending order:

        CALL SORTIN(BUF,LEN)		!INTEGER*4 BUF(LEN)
        CALL I2SORT(BUF,PTR,LEN)	!INTEGER*2 BUF(LEN),PTR(LEN)
  where
        BUF is an array to be sorted in ascending order.
        PTR is an array which is moved in parallel with BUF.
        LEN is the number of elements in the array

  Note that BUF and PTR CANNOT be the same array.  You must include a PTR
  array even if you do not need it.

  A more complete list of sort routines are available in the MATH77 library:

	ISORT(BUF,M,N)		ISORTP(BUF,M,N,PTR)	INTEGER*4 BUF(K)
        SSORT(SBUF,M,N)		SSORTP(SBUF,M,N,PTR)	REAL*4 SBUF(K)
	DSORT(DBUF,M,N)		DSORTP(DBUF,M,N,PTR)	REAL*8 DBUF(K)

  Note that the MATH77 routines sort array elements M thru N.  The C-bridges for
  these routines are in MATH77_SORT.COM.  You must include the following in
  your imake file if you use any of the MATH77 routines:

	#define LIB_MATH77 

  The subroutines I4SORT and R4SORT (formerly included here) have been obsoleted
  because they could not be made portable.  Instead, replace the calls to
  I4SORT and R4SORT with the following MATH77 routines:

     INTEGER*4 BUF(N),BUF2(N)
     REAL*4 SBUF(N),SBUF2(N)
     INTEGER*4 PTR(N),PTR2(N),IP(N)

  Change:                        To:

      CALL I4SORT(BUF,BUF,N)         CALL ISORT(BUF,1,N)
      CALL R4SORT(SBUF,SBUF,N)       CALL SSORT(SBUF,1,N)

      CALL I4SORT(BUF,PTR,N)         CALL MVE(4,N,BUF,BUF2,1,1)
                                     CALL MVE(4,N,PTR,PTR2,1,1)
                                     CALL ISORTP(BUF,1,N,IP)
                                     DO K=1,N
                                        BUF(K) = BUF2(IP(K))
                                        PTR(K) = PTR2(IP(K))
                                     END DO


      CALL R4SORT(SBUF,PTR,N)        CALL MVE(7,N,SBUF,SBUF2,1,1)
                                     CALL MVE(7,N,PTR,PTR2,1,1)
                                     CALL SSORTP(SBUF,1,N,IP)
                                     DO K=1,N
                                         SBUF(K) = SBUF2(IP(K))
                                         PTR(K) = PTR2(IP(K))
                                     END DO


1 C BRIDGES:

	int buf[k],ptr[k];
	short hbuf[k],hptr[k];

        zsortin(buf,n);
        zi2sort(hbuf,hptr,n);

2 EXAMPLES

  The PTR is commonly used as an index:

		INTEGER*2 BUF(5),PTR(5)

		DO I=1,5
		   PTR(I) = I
		ENDDO
		CALL I2SORT(BUF,PTR,5)

   The following example sorts a set of line-sample coordinates by line
   number:
		INTEGER*2 LINE(N),SAMP(N)
		CALL I2SORT(LINE,SAMP,N)

2 HISTORY

SORTIN and I2SORT written by Gary Yagi, Jan 77 and Mar 88.
Converted to VAX by:  C. C. Avis,  25 Mar 83
Current Cognizant Programmer: G.M. Yagi
Source Language: Fortran
Ported to UNIX by:    Steve Pohorsky, 18 Dec 92

REVISION HISTORY:
  17 Jan 89  GMY  Handle case where N=1 in SORTIN.
  13 Mar 88  GMY  Added I2SORT.  Rewrote help and test files.  
  18 Dec 92  SP   Added C bridge (zsortin) for SORTIN. Obsoleted
                  I4SORT, and R4SORT because they were not portable
                  and were slower than the MATH77 routines (ISORT,ISORTP,
                  SSORT, SSORTP.)
  02 May 01  GMY  Added C bridge for I2SORT.
$ Return
$!#############################################################################
