$!****************************************************************************
$!
$! Build proc for MIPL module mve
$! VPACK Version 1.9, Monday, December 07, 2009, 15:58:18
$!
$! Execute by entering:		$ @mve
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
$ write sys$output "*** module mve ***"
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
$ write sys$output "Invalid argument given to mve.com file -- ", primary
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
$   if F$SEARCH("mve.imake") .nes. ""
$   then
$      vimake mve
$      purge mve.bld
$   else
$      if F$SEARCH("mve.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake mve
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @mve.bld "STD"
$   else
$      @mve.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create mve.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack mve.com -mixed -
	-s mve.f zmve.c -
	-i mve.imake -
	-t tmve.f tzmve.c tmve.imake tmve.pdf tstmve.pdf -
	-o mve.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create mve.f
$ DECK/DOLLARS="$ VOKAGLEVE"
      SUBROUTINE MVE(DCODE,N,A,B,INCA,INCB)
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C	VICAR SUBROUTINE                                               MVE
C       ----------------                                               ---
C	General routine for transferring arrays.
C	Fortran format of call:
C
C	CALL MVE(DCODE, N, A, B, INCA, INCB)
C
C	Parameters:-
C
C	DCODE......Transfer mode
C	           1  = Move byte to byte
C                  2  = Move halfword to halfword
C                  3  = Move byte to halfword
C                  4  = Move fullword to fullword
C                  5  = Move byte to fullword
C                  6  = Move halfword to fullword
C                  7  = Move real (single) to real.
C                  8  = Move double to double.
C                  9  = Move real to double
C	           negative values -1 to -9 reverse of above.
C	N.........No of elements to transfer
C       A.........Source vector to be transferred
C       B.........Destination vector.
C       INCA......Source address increament 
C	INCB......Dest address increment 
C
C   REVISION HISTORY
C
C 92-03-30 ...SP.... Changed to use fortport.fin as improved portability
C                    method.  Added special case code for inca=1=incb.
C 89-11-22 ...SP.... Adapted FORTRAN version from J. Mosher to use ZEXT for
C                    speed and sysarch.fin (to handle byte-swap) for 
C                    portability.
C                    ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.
C 85-10-8  ...GMY... change back to MVE, numerous changes
C 84-7-21  ...LWK... fixed bug for negative numbers in DCODE=6
C 83-3-22  ... changed to umve, MVE fortran to handle quoted
C		string arguments --LWK
C 83-2-3: revised to allow 5 arguments - LWK
C UCL routine  --  used for VICAR1, 82-12.
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       INTEGER*4 DCODE,INCA,INCB,A(*),B(*),N
       CHARACTER*80 CBUF
C==================================================================
      IF(DCODE.EQ.1.OR.DCODE.EQ.-1) CALL MVE1 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.2.OR.DCODE.EQ.-2) CALL MVE2 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.3)                CALL MVE3P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-3)               CALL MVE3N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.4.OR.DCODE.EQ.-4) CALL MVE4 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.5)                CALL MVE5P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-5)               CALL MVE5N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.6)                CALL MVE6P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-6)               CALL MVE6N(N,A,B,INCA,INCB)
      IF(DCODE.EQ.7.OR.DCODE.EQ.-7) CALL MVE7 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.8.OR.DCODE.EQ.-8) CALL MVE8 (N,A,B,INCA,INCB)
      IF(DCODE.EQ.9)                CALL MVE9P(N,A,B,INCA,INCB)
      IF(DCODE.EQ.-9)               CALL MVE9N(N,A,B,INCA,INCB)
      IF(DCODE.GT.9.OR.DCODE.LT.-9 .OR. DCODE .EQ. 0)THEN
        WRITE( CBUF,90101) DCODE
90101   FORMAT(' MVE---DCODE OUT OF RANGE,DCODE=', I12)
        CALL XVMESSAGE( CBUF, ' ')
      ENDIF
      RETURN
      END
      SUBROUTINE MVE1(N,A,B,INCA,INCB)   !BYTE IN , BYTE OUT
      BYTE A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO k=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE2(N,A,B,INCA,INCB)!HALF IN , HALF OUT
      INTEGER*2 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO I=1,N
          B(JJ)=A(II)
          II=II+INCA
          JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE3P(N,A,B,INCA,INCB)! BYTE IN , HALF OUT
      include 'fortport'

      BYTE A(*)
      INTEGER*2 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=BYTE2INT( A(K) )
       ENDDO
      ELSE       
       II=1
       JJ=1
       DO I=1,N
         B(JJ)= BYTE2INT( A(II) )
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
       RETURN
      END IF
      END
      SUBROUTINE MVE3N(N,A,B,INCA,INCB) !HALF IN , BYTE OUT

      include  'fortport'  ! defines INT2BYTE.

      INTEGER*2 A(*)
      BYTE B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 255 ) THEN
           B(K) = 255
        ELSE IF ( A(K) .LT. 0 ) THEN
           B(K) = 0
        ELSE
           B(K)= INT2BYTE( A(K) )
        END IF
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 255 ) THEN
           B(JJ) = 255
        ELSE IF ( A(II) .LT. 0 ) THEN
           B(JJ) = 0
        ELSE
           B(JJ)= INT2BYTE( A(II) )
        END IF
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE4(N,A,B,INCA,INCB) !FULLWORD IN , FULLWORD OUT
      INTEGER*4 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE5P(N,A,B,INCA,INCB) !BYTE IN ,FULLWORD OUT

      include  'fortport'  ! defines BYTE2INT.

      BYTE A(*)
      INTEGER*4 B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)= BYTE2INT( A(K) )
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        B(JJ)= BYTE2INT( A(II) )
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE5N(N,A,B,INCA,INCB) !FULLWORD IN , BYTE OUT

      include  'fortport'  ! DEFINES INT2BYTE

      INTEGER*4 A(*)
      BYTE B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 255 ) THEN
           B(K) = 255
        ELSE IF ( A(K) .LT. 0 ) THEN
           B(K) = 0
        ELSE
           B(K)= INT2BYTE( A(K) )
        END IF
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 255 ) THEN
           B(JJ) = 255
        ELSE IF ( A(II) .LT. 0 ) THEN
           B(JJ) = 0
        ELSE
           B(JJ)= INT2BYTE( A(II) )
        END IF
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE6P(N,A,B,INCA,INCB) !HALFWORD IN , FULLWORD OUT
      INTEGER*2 A(*)
      INTEGER*4 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE6N(N,A,B,INCA,INCB) !FULLWORD IN , HALFWORD OUT

      INTEGER*4 A(*)
      INTEGER*2 B(*)

C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        IF ( A(K) .GT. 32767 ) THEN
           B(K) = 32767
        ELSE IF ( A(K) .LT. -32768 ) THEN
           B(K) = -32768
        ELSE
           B(K)= A(K)
        END IF
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
        IF ( A(II) .GT. 32767 ) THEN
           B(JJ) = 32767
        ELSE IF ( A(II) .LT. -32768 ) THEN
           B(JJ) = -32768
        ELSE
           B(JJ)= A(II)
        END IF
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE7(N,A,B,INCA,INCB) !REAL*4 IN , REAL*4 OUT
      REAL*4 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE8(N,A,B,INCA,INCB) !REAL*8 IN , REAL*8 OUT
      REAL*8 A(*),B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
        B(JJ)=A(II)
        II=II+INCA
        JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE9P(N,A,B,INCA,INCB) !REAL*4 IN , REAL*8 OUT
      REAL*4 A(*)
      REAL*8 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO
      ELSE       

       II=1
       JJ=1
       DO I=1,N
          B(JJ)=A(II)
          II=II+INCA
          JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
      SUBROUTINE MVE9N(N,A,B,INCA,INCB) !REAL*8 IN , REAL*4 OUT
      REAL*8 A(*)
      REAL*4 B(*)
C=========================================
      IF (INCA .EQ. 1 .AND. INCB .EQ. 1)  THEN
       DO K=1,N
        B(K)=A(K)
       ENDDO

      ELSE       
       II=1
       JJ=1
       DO I=1,N
         B(JJ)=A(II)
         II=II+INCA
         JJ=JJ+INCB
       ENDDO
      END IF
      RETURN
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zmve.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME2(mve, MVE) (int* dcode, int *n, int* a, int* b,
			int* inca, int* incb);

/************************************************************************/
/* C-Callable Version: zmve - move values from array a to array b	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void zmve(
  int dcode,			/* specifies numeric type of a and b	*/
  int n,			/* number of values ot move		*/
  void *a,			/* array FROM which values are moved	*/
  void *b,			/* array TO which values are moved	*/
  int inca,			/* spacing (step) of elements of a to be moved*/
  int incb			/* spacing (step) of elements of b	*/
)
{
FTN_NAME2(mve, MVE) ( &dcode, &n, a, b, &inca, &incb);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create mve.imake
/* Imake file for VICAR subroutine MVE */

#define SUBROUTINE mve

#define MODULE_LIST mve.f zmve.c

#define P1_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tmve.f
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      DIMENSION BUF(32)
      DIMENSION BBUF(20),HBUF(20),FBUF(20),RBUF(20),DBUF(20)
      DIMENSION BBUG(10),HBUG(10),FBUG(10),RBUG(10),DBUG(10)
      BYTE BBUF,BBUG
      INTEGER*2 HBUF,HBUG
      INTEGER*4 FBUF,FBUG
      REAL*4 RBUF,RBUG
      REAL*8 DBUF,DBUG
C
      DO I=1,10
      IVAL = I - 5
      BBUF(I) = IVAL
      HBUF(I) = IVAL
      FBUF(I) = IVAL
      RBUF(I) = IVAL + 0.123412341234D0
      DBUF(I) = IVAL + 0.123412341234D0
      ENDDO
C      
      HBUF(9) = -32768
      HBUF(10) = 32767
      FBUF(7) = -32769
      FBUF(8) = -32768
      FBUF(9) = 32767
      FBUF(10) = 32768
C
      NS = 10
      CALL XVMESSAGE('Test #1:  AINC and BINC =1', ' ')
      CALL PRNT(1,NS,BBUF,' Initial BBUF=.')
      CALL PRNT(2,NS,HBUF,' Initial HBUF=.')
      CALL PRNT(4,NS,FBUF,' Initial FBUF=.')
      CALL PRNT(7,NS,RBUF,' Initial RBUF=.')
      CALL PRNT(8,NS,DBUF,' Initial DBUF=.')
      CALL MVE(-9,NS,DBUF,BUF,1,1)
      CALL PRNT(7,NS,BUF,' DCODE=-9.')
      CALL MVE(-8,NS,DBUF,BUF,1,1)
      CALL PRNT(8,NS,BUF,' DCODE=-8.')
      CALL MVE(-7,NS,RBUF,BUF,1,1)
      CALL PRNT(7,NS,BUF,' DCODE=-7.')
      CALL MVE(-6,NS,FBUF,BUF,1,1)
      CALL PRNT(2,NS,BUF,' DCODE=-6.')
      CALL MVE(-5,NS,FBUF,BUF,1,1)
      CALL PRNT(1,NS,BUF,' DCODE=-5.')
      CALL MVE(-4,NS,FBUF,BUF,1,1)
      CALL PRNT(4,NS,BUF,' DCODE=-4.')
      CALL MVE(-3,NS,HBUF,BUF,1,1)
      CALL PRNT(1,NS,BUF,' DCODE=-3.')
      CALL MVE(-2,NS,HBUF,BUF,1,1)
      CALL PRNT(2,NS,BUF,' DCODE=-2.')
      CALL MVE(-1,NS,BBUF,BUF,1,1)
      CALL PRNT(1,NS,BUF,' DCODE=-1.')
      CALL XVMESSAGE('MVE SHOULD PRINT ERROR MESSAGE FOR DCODE=0', ' ')
      CALL MVE(0,NS,BBUF,BUF,1,1)
      CALL MVE(1,NS,BBUF,BUF,1,1)
      CALL PRNT(1,NS,BUF,' DCODE=1.')
      CALL MVE(2,NS,HBUF,BUF,1,1)
      CALL PRNT(2,NS,BUF,' DCODE=2.')
      CALL MVE(3,NS,BBUF,BUF,1,1)
      CALL PRNT(2,NS,BUF,' DCODE=3.')
      CALL MVE(4,NS,FBUF,BUF,1,1)
      CALL PRNT(4,NS,BUF,' DCODE=4.')
      CALL MVE(5,NS,BBUF,BUF,1,1)
      CALL PRNT(4,NS,BUF,' DCODE=5.')
      CALL MVE(6,NS,HBUF,BUF,1,1)
      CALL PRNT(4,NS,BUF,' DCODE=6.')
      CALL MVE(7,NS,RBUF,BUF,1,1)
      CALL PRNT(7,NS,BUF,' DCODE=7.')
      CALL MVE(8,NS,DBUF,BUF,1,1)
      CALL PRNT(8,NS,BUF,' DCODE=8.')
      CALL MVE(9,NS,RBUF,BUF,1,1)
      CALL PRNT(8,NS,BUF,' DCODE=9.')
C
      DO I=1,20
      IVAL = I-1
      BBUF(I) = IVAL
      HBUF(I) = IVAL
      FBUF(I) = IVAL
      RBUF(I) = IVAL
      DBUF(I) = IVAL
      ENDDO
C      
      NS = 10
      CALL XVMESSAGE('Test #2:  AINC=2 and BINC=-1)', ' ')
      CALL PRNT(1,20,BBUF,' Initial BBUF=.')
      CALL PRNT(2,20,HBUF,' Initial HBUF=.')
      CALL PRNT(4,20,FBUF,' Initial FBUF=.')
      CALL PRNT(7,20,RBUF,' Initial RBUF=.')
      CALL PRNT(8,20,DBUF,' Initial DBUF=.')
      CALL MVE(-9,NS,DBUF,RBUG(10),2,-1)
      CALL PRNT(7,NS,RBUG,' DCODE=-9.')
      CALL MVE(-8,NS,DBUF,DBUG(10),2,-1)
      CALL PRNT(8,NS,DBUG,' DCODE=-8.')
      CALL MVE(-7,NS,RBUF,RBUG(10),2,-1)
      CALL PRNT(7,NS,RBUG,' DCODE=-7.')
      CALL MVE(-6,NS,FBUF,HBUG(10),2,-1)
      CALL PRNT(2,NS,HBUG,' DCODE=-6.')
      CALL MVE(-5,NS,FBUF,BBUG(10),2,-1)
      CALL PRNT(1,NS,BBUG,' DCODE=-5.')
      CALL MVE(-4,NS,FBUF,FBUG(10),2,-1)
      CALL PRNT(4,NS,FBUG,' DCODE=-4.')
      CALL MVE(-3,NS,HBUF,BBUG(10),2,-1)
      CALL PRNT(1,NS,BBUG,' DCODE=-3.')
      CALL MVE(-2,NS,HBUF,HBUG(10),2,-1)
      CALL PRNT(2,NS,HBUG,' DCODE=-2.')
      CALL MVE(-1,NS,BBUF,BBUG(10),2,-1)
      CALL PRNT(1,NS,BBUG,' DCODE=-1.')
      CALL MVE(0,NS,BBUF,BBUG(10),2,-1)
      CALL MVE(1,NS,BBUF,BBUG(10),2,-1)
      CALL PRNT(1,NS,BBUG,' DCODE=1.')
      CALL MVE(2,NS,HBUF,HBUG(10),2,-1)
      CALL PRNT(2,NS,HBUG,' DCODE=2.')
      CALL MVE(3,NS,BBUF,HBUG(10),2,-1)
      CALL PRNT(2,NS,HBUG,' DCODE=3.')
      CALL MVE(4,NS,FBUF,FBUG(10),2,-1)
      CALL PRNT(4,NS,FBUG,' DCODE=4.')
      CALL MVE(5,NS,BBUF,FBUG(10),2,-1)
      CALL PRNT(4,NS,FBUG,' DCODE=5.')
      CALL MVE(6,NS,HBUF,FBUG(10),2,-1)
      CALL PRNT(4,NS,FBUG,' DCODE=6.')
      CALL MVE(7,NS,RBUF,RBUG(10),2,-1)
      CALL PRNT(7,NS,RBUG,' DCODE=7.')
      CALL MVE(8,NS,DBUF,DBUG(10),2,-1)
      CALL PRNT(8,NS,DBUG,' DCODE=8.')
      CALL MVE(9,NS,RBUF,DBUG(10),2,-1)
      CALL PRNT(8,NS,DBUG,' DCODE=9.')

      CALL XVMESSAGE(
     . 'Repeat test cases in C to test C interface: zmve', ' ')

      call tzmve

      RETURN
      END
$!-----------------------------------------------------------------------------
$ create tzmve.c
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* 	*/
/************************************************************************/

/*  	DCODE......Transfer mode
	          1  = Move byte array to byte array
                  2  = Move halfword to halfword
                  3  = Move byte to halfword
                  4  = Move fullword to fullword
                  5  = Move byte to fullword
                  6  = Move halfword to fullword
                  7  = Move real (single) to real.
                  8  = Move double to double.
                  9  = Move real to double
	           negative values -1 to -9 reverse of above.	  */

void FTN_NAME(tzmve)() 
{
      int buf[32];
      unsigned char  bbuf[20],bbug[10];
      short hbuf[20],hbug[10];
      int   fbuf[20],fbug[10];
      float rbuf[20],rbug[10];
      double dbuf[20],dbug[10];

      int i, ival, ns;

      for ( i=0; i<10; i++ )
      {
        ival = i - 4;
        bbuf[i] = ival;
        hbuf[i] = ival;
        fbuf[i] = ival;
        rbuf[i] = ival + 0.123412341234;
        dbuf[i] = ival + 0.123412341234;
      }
      
      hbuf[8] = -32768;
      hbuf[9] = 32767;
      fbuf[6] = -32769;
      fbuf[7] = -32768;
      fbuf[8] = 32767;
      fbuf[9] = 32768;

      ns = 10;
       zvmessage("Test #1:  AINC and BINC =1", "");
       zprnt(1,ns,bbuf," Initial bbuf=");
       zprnt(2,ns,hbuf," Initial hbuf=");
       zprnt(4,ns,fbuf," Initial fbuf=");
       zprnt(7,ns,rbuf," Initial rbuf=");
       zprnt(8,ns,dbuf," Initial dbuf=");
       zmve(-9,ns,dbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=-9");
       zmve(-8,ns,dbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=-8");
       zmve(-7,ns,rbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=-7");
       zmve(-6,ns,fbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=-6");
       zmve(-5,ns,fbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-5");
       zmve(-4,ns,fbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=-4");
       zmve(-3,ns,hbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-3");
       zmve(-2,ns,hbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=-2");
       zmve(-1,ns,bbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=-1");
       zmve(0,ns,bbuf,buf,1,1);
       zmve(1,ns,bbuf,buf,1,1);
       zprnt(1,ns,buf," dcode=1");
       zmve(2,ns,hbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=2");
       zmve(3,ns,bbuf,buf,1,1);
       zprnt(2,ns,buf," dcode=3");
       zmve(4,ns,fbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=4");
       zmve(5,ns,bbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=5");
       zmve(6,ns,hbuf,buf,1,1);
       zprnt(4,ns,buf," dcode=6");
       zmve(7,ns,rbuf,buf,1,1);
       zprnt(7,ns,buf," dcode=7");
       zmve(8,ns,dbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=8");
       zmve(9,ns,rbuf,buf,1,1);
       zprnt(8,ns,buf," dcode=9");

      for ( i=0; i<20; i++ )
      {
      ival = i;
      bbuf[i] = ival;
      hbuf[i] = ival;
      fbuf[i] = ival;
      rbuf[i] = ival;
      dbuf[i] = ival;
      }
      
      ns = 10;
       zvmessage(" Test #2:  AINC=2 and BINC=-1)", "");
       zprnt(1,20,bbuf," Initial bbuf=");
       zprnt(2,20,hbuf," Initial hbuf=");
       zprnt(4,20,fbuf," Initial fbuf=");
       zprnt(7,20,rbuf," Initial rbuf=");
       zprnt(8,20,dbuf," Initial dbuf=");
       zmve(-9,ns,dbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=-9");
       zmve(-8,ns,dbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=-8");
       zmve(-7,ns,rbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=-7");
       zmve(-6,ns,fbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=-6");
       zmve(-5,ns,fbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-5");
       zmve(-4,ns,fbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=-4");
       zmve(-3,ns,hbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-3");
       zmve(-2,ns,hbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=-2");
       zmve(-1,ns,bbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=-1");
       zmve(0,ns,bbuf,&bbug[9],2,-1);
       zmve(1,ns,bbuf,&bbug[9],2,-1);
       zprnt(1,ns,bbug," dcode=1");
       zmve(2,ns,hbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=2");
       zmve(3,ns,bbuf,&hbug[9],2,-1);
       zprnt(2,ns,hbug," dcode=3");
       zmve(4,ns,fbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=4");
       zmve(5,ns,bbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=5");
       zmve(6,ns,hbuf,&fbug[9],2,-1);
       zprnt(4,ns,fbug," dcode=6");
       zmve(7,ns,rbuf,&rbug[9],2,-1);
       zprnt(7,ns,rbug," dcode=7");
       zmve(8,ns,dbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=8");
       zmve(9,ns,rbuf,&dbug[9],2,-1);
       zprnt(8,ns,dbug," dcode=9");
}
$!-----------------------------------------------------------------------------
$ create tmve.imake
/* Imake file for Test of VICAR subroutine MVE */

#define PROGRAM tmve

#define MODULE_LIST tmve.f tzmve.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL


#define LIB_TAE
#define LIB_LOCAL
/*    #define LIB_P2SUB */
$!-----------------------------------------------------------------------------
$ create tmve.pdf
process
end-proc
$!-----------------------------------------------------------------------------
$ create tstmve.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
tmve
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create mve.hlp
1 MVE

  General routine for copying or sub-sampling arrays.

  FORTRAN Calling Sequence:  CALL MVE(DCODE,N,A,B,AINC,BINC)
  C Calling Sequence:        zmve(dcode,n,a,b,ainc,binc);

  Arguments:

	INTEGER*4 DCODE		Data code.
	INTEGER*4 N		Number of elements to be moved.
	INTEGER*4 AINC		Increment for input array A.
	INTEGER*4 BINC		Increment for output array B.

  AINC and BINC are REQUIRED arguments. (dcode, n, ainc, and binc are
  passed by value for zmve.)
  The input and output arrays A and B may be any FORTRAN data type
  (or equivalent C data type) specifiable by DCODE:

	DCODE		Input		Output
	-----		-----		------
	  1		BYTE		BYTE
	  2		INTEGER*2	INTEGER*2
	  3		BYTE		INTEGER*2
	  4		INTEGER*4	INTEGER*4
	  5		BYTE		INTEGER*4
	  6		INTEGER*2	INTEGER*4
	  7		REAL*4		REAL*4
	  8		REAL*8		REAL*8
	  9		REAL*4		REAL*8

  In addition, making the data code negative reverses its meaning.
  E.g. if DCODE= -3, then the input array contains INTEGER*2 data
  and the output array contains BYTE data.

2 Operation

  The subroutine call

		CALL MVE(DCODE,N,A,B,AINC,BINC)

  is equivalent to the following FORTRAN code:

		I = 1
		J = 1
		DO K=1,N
		  B(J) = A(I)
		  I = I + AINC
		  J = J + BINC
		ENDDO

  MVE does check for data overflow when moving data to a smaller data type.
  Out-of-range data is set to the extreme values of the output range.
  If the value 257 were moved into a byte array, for example, the
  result would be chopped down to 255.
  If the value -1 were moved into a byte array, for example, the
  result would be 0.

2 Examples

	BYTE A(10),B(10)
	INTEGER*2 C(10)

	CALL MVE(1,10,A,B(10),1,-1)	  ! Reverse order of the elements
	CALL MVE(3,10,A,C,1,1)		  ! Unpack a byte array
	CALL MVE(1,10,0,B,0,1)		  ! Zero out array B


2 History

 92-03-30 ...SP.... Changed to use fortport.fin as improved portability
                    method.  Added special case code for inca=1=incb.
                    Added zmve for calls from C.
 89-11-22 ...SP.... Adapted FORTRAN version from J. Mosher to use ZEXT for
                    speed and sysarch.fin (to handle byte-swap) for 
                    portability.
                    ELIMINATED OPTIONAL PARAMETERS FOR PORTABILITY.

  Original Programmer:  Gary Yagi, 4 Feb 1975
  Cognizant Programmer:  Steve Pohorsky

  Source Language:  FORTRAN

$ Return
$!#############################################################################
