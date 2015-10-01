$!****************************************************************************
$!
$! Build proc for MIPL module minmax
$! VPACK Version 1.9, Monday, December 07, 2009, 16:27:37
$!
$! Execute by entering:		$ @minmax
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
$ write sys$output "*** module minmax ***"
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
$ write sys$output "Invalid argument given to minmax.com file -- ", primary
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
$   if F$SEARCH("minmax.imake") .nes. ""
$   then
$      vimake minmax
$      purge minmax.bld
$   else
$      if F$SEARCH("minmax.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake minmax
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @minmax.bld "STD"
$   else
$      @minmax.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create minmax.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack minmax.com -mixed -
	-s minmax.f zminmax.c -
	-i minmax.imake -
	-t tminmax.f tzminmax.c tminmax.imake tminmax.pdf tstminmax.pdf -
	-o minmax.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create minmax.f
$ DECK/DOLLARS="$ VOKAGLEVE"
	SUBROUTINE MINMAX(DCODE,N,BUF,MIN,MAX,IMIN,IMAX)

	INTEGER*4  BUF(*),MIN,MAX,IMIN,IMAX,DCODE,N

	IF ( DCODE.EQ.1 ) THEN
	        CALL BMINMAX( N, BUF, MIN, MAX,IMIN , IMAX)
	ELSE IF ( DCODE.EQ.2) THEN
		CALL I2MINMAX( N, BUF, MIN, MAX, IMIN,IMAX)
	ELSE IF ( DCODE.EQ.4) THEN
		CALL I4MINMAX( N, BUF, MIN, MAX, IMIN, IMAX)
	ELSE IF ( DCODE.EQ.7) THEN
		CALL R4MINMAX( N, BUF,  MIN, MAX, IMIN, IMAX )
	ELSE IF ( DCODE.EQ.8) THEN
		CALL R8MINMAX( N, BUF, MIN, MAX, IMIN, IMAX )
	ELSE IF ( DCODE.EQ.10) THEN
		CALL CMINMAX( N, BUF, MIN, MAX, IMIN, IMAX )
	ENDIF

 	RETURN
	END
	
	
	SUBROUTINE BMINMAX(N,BBUF, MIN, MAX, IMIN, IMAX)
C  BYTE ARRAY

        include  'fortport'  ! defines BYTE2INT.
	BYTE  BBUF(N)
	INTEGER*4 N,MAX,MIN,IMAX,IMIN,I, IVAL

	I = 1
	MIN = BYTE2INT( BBUF(I) )
	MAX = BYTE2INT( BBUF(I) )
	IMIN = I
	IMAX = I

	DO WHILE (I.LE.N)
                IVAL = BYTE2INT( BBUF(I) )
		IF (MIN.GT.IVAL)THEN
			MIN = IVAL
			IMIN = I
		ENDIF

		IF (MAX.LT.IVAL) THEN
			MAX = IVAL
			IMAX = I
		ENDIF
		I = I + 1


	ENDDO


	RETURN
	END

	SUBROUTINE I2MINMAX(N,I2BUF,MIN,MAX,IMIN,IMAX)
C INTEGER*2 ARRAY

	INTEGER*2 I2BUF(N)
	INTEGER*4 N,MIN,MAX,IMIN,IMAX,I

	I = 1
	MIN = I2BUF(I)
	MAX = I2BUF(I)
	IMIN = I
	IMAX = I

	DO WHILE (I.LE.N)
		IF (MIN.GT.I2BUF(I))THEN
			MIN = I2BUF(I)
			IMIN = I
		ENDIF

		IF (MAX.LT.I2BUF(I)) THEN
			MAX = I2BUF(I)
			IMAX = I
		ENDIF
		I = I + 1
	ENDDO


	RETURN 
	END

	SUBROUTINE I4MINMAX(N,I4BUF,MIN,MAX,IMIN,IMAX)
C  INTEGER*4 ARRAY

	INTEGER*4 N,I4BUF(N),MIN,MAX,IMIN,IMAX,I

	I = 1
	MIN = I4BUF(I)
	MAX = I4BUF(I)
	IMIN = I
	IMAX = I

	DO WHILE (I.LE.N)
		IF (MIN.GT.I4BUF(I))THEN
			MIN = I4BUF(I)
			IMIN = I
		ENDIF

		IF (MAX.LT.I4BUF(I)) THEN
			MAX = I4BUF(I)
			IMAX = I
		ENDIF
		I = I + 1
	ENDDO

	RETURN
	END


	SUBROUTINE R4MINMAX(N,R4BUF,RMIN,RMAX,IMIN,IMAX)
C REAL*4 ARRAY

	INTEGER*4  N,IMIN,IMAX,I
	REAL*4     R4BUF(N),RMIN,RMAX

	I = 1
	RMIN = R4BUF(I)
	RMAX = R4BUF(I)
	IMIN = I
	IMAX = I

	DO WHILE (I.LE.N)
		IF (RMIN.GT.R4BUF(I))THEN
			RMIN = R4BUF(I)
			IMIN = I
		ENDIF

		IF (RMAX.LT.R4BUF(I)) THEN
			RMAX = R4BUF(I)
			IMAX = I
		ENDIF
		I = I + 1
	ENDDO

	RETURN
	END


	SUBROUTINE R8MINMAX(N,R8BUF,RMIN,RMAX,IMIN,IMAX)
C  REAL*8 ARRAY
	
	INTEGER*4   N,I,IMIN,IMAX
	REAL*8      R8BUF(N)
	REAL*8      RMIN,RMAX
	
	I = 1
	RMIN = R8BUF(I)
	RMAX = R8BUF(I)
	IMIN = I
	IMAX = I

	DO WHILE (I.LE.N)
		IF (RMIN.GT.R8BUF(I))THEN
			RMIN = R8BUF(I)
			IMIN = I
		ENDIF

		IF (RMAX.LT.R8BUF(I)) THEN
			RMAX = R8BUF(I)
			IMAX = I
		ENDIF
		I = I + 1
	ENDDO


	RETURN
	END


	SUBROUTINE CMINMAX(N,CBUF,RMIN,RMAX,IMIN,IMAX)
C  COMPLEX ARRAY
	
	INTEGER*4   N,I,IMIN,IMAX
	COMPLEX     CBUF(N)
	REAL*4      RMIN,RMAX
	
	I = 1
	RMIN = CABS( CBUF(I) )
	RMAX = CABS( CBUF(I) )
	IMIN = I
	IMAX = I

	DO I = 2, N
                RVAL = CABS( CBUF(I) )
		IF (RMIN.GT.RVAL)THEN
			RMIN = RVAL
			IMIN = I
		ENDIF

		IF (RMAX.LT.RVAL) THEN
			RMAX = RVAL
			IMAX = I
		ENDIF
	ENDDO


	RETURN
	END
C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C C 
	SUBROUTINE MINMAXE(DCODE,N,BUF, EXCLUDE,MIN,MAX,IMIN,IMAX)

	INTEGER*4  BUF(*),MIN,MAX,IMIN,IMAX,DCODE,N, EXCLUDE

	IF ( DCODE.EQ.1 ) THEN
	        CALL BMINMAXE( N, BUF, EXCLUDE, MIN, MAX,IMIN , IMAX)
	ELSE IF ( DCODE.EQ.2) THEN
		CALL I2MINMAXE( N, BUF, EXCLUDE, MIN, MAX, IMIN,IMAX)
	ELSE IF ( DCODE.EQ.4) THEN
		CALL I4MINMAXE( N, BUF, EXCLUDE, MIN, MAX, IMIN, IMAX)
	ELSE IF ( DCODE.EQ.7) THEN
		CALL R4MINMAXE( N, BUF, EXCLUDE,  MIN, MAX, IMIN, IMAX )
	ELSE IF ( DCODE.EQ.8) THEN
		CALL R8MINMAXE( N, BUF, EXCLUDE, MIN, MAX, IMIN, IMAX )
	ELSE IF ( DCODE.EQ.10) THEN
		CALL CMINMAXE( N, BUF, EXCLUDE, MIN, MAX, IMIN, IMAX )
	ENDIF

 	RETURN
	END
	
	
	SUBROUTINE BMINMAXE(N,BBUF, EXCLUDE, MINI, MAXI, IMIN, IMAX)
C  BYTE ARRAY

        include  'fortport'  ! defines BYTE2INT.
	BYTE  BBUF(N)
	INTEGER*4 N,MAXI,MINI,IMAX,IMIN,I, IVAL, EXCLUDE
C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. BYTE2INT(BBUF(I)) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           MINI = BYTE2INT(BBUF(1))
           MAXI = BYTE2INT(BBUF(1))
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           MINI = BYTE2INT(BBUF(I))
           MAXI = BYTE2INT(BBUF(I))
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               IVAL = BYTE2INT( BBUF(I) )
               IF ( IVAL .NE. EXCLUDE )  THEN
		IF (MINI.GT.IVAL)THEN
			MINI = IVAL
			IMIN = I
		ENDIF

		IF (MAXI.LT.IVAL) THEN
			MAXI = IVAL
			IMAX = I
		ENDIF
               END IF
	       I = I + 1
           ENDDO
        END IF

	RETURN
	END

	SUBROUTINE I2MINMAXE(N,I2BUF, EXCLUDE,MINI,MAXI,IMIN,IMAX)
C INTEGER*2 ARRAY

	INTEGER*2 I2BUF(N)
	INTEGER*4 N,MINI,MAXI,IMIN,IMAX,I, EXCLUDE
C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. I2BUF(I) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           MINI = I2BUF(1)
           MAXI = I2BUF(1)
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           MINI = I2BUF(I)
           MAXI = I2BUF(I)
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               IF ( I2BUF(I) .NE. EXCLUDE )  THEN
		IF (MINI.GT.I2BUF(I))THEN
			MINI = I2BUF(I)
			IMIN = I
		ENDIF

		IF (MAXI.LT.I2BUF(I)) THEN
			MAXI = I2BUF(I)
			IMAX = I
		ENDIF
               END IF
               I = I + 1
           ENDDO
        END IF

	RETURN 
	END

	SUBROUTINE I4MINMAXE(N,I4BUF, EXCLUDE,MINI,MAXI,IMIN,IMAX)
C  INTEGER*4 ARRAY

	INTEGER*4 N,I4BUF(N),MINI,MAXI,IMIN,IMAX,I, EXCLUDE

C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. I4BUF(I) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           MINI = I4BUF(1)
           MAXI = I4BUF(1)
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           MINI = I4BUF(I)
           MAXI = I4BUF(I)
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               IF ( I4BUF(I) .NE. EXCLUDE )  THEN
		IF (MINI.GT.I4BUF(I))THEN
			MINI = I4BUF(I)
			IMIN = I
		ENDIF

		IF (MAXI.LT.I4BUF(I)) THEN
			MAXI = I4BUF(I)
			IMAX = I
		ENDIF
               END IF
               I = I + 1
           ENDDO
        END IF

	RETURN
	END


	SUBROUTINE R4MINMAXE(N,R4BUF, EXCLUDE,RMIN,RMAX,IMIN,IMAX)
C REAL*4 ARRAY

	INTEGER*4  N,IMIN,IMAX,I
	REAL*4     R4BUF(N),RMIN,RMAX, EXCLUDE

C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. R4BUF(I) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           RMIN = R4BUF(1)
           RMAX = R4BUF(1)
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           RMIN = R4BUF(I)
           RMAX = R4BUF(I)
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               IF ( R4BUF(I) .NE. EXCLUDE )  THEN
		IF (RMIN.GT.R4BUF(I))THEN
			RMIN = R4BUF(I)
			IMIN = I
		ENDIF

		IF (RMAX.LT.R4BUF(I)) THEN
			RMAX = R4BUF(I)
			IMAX = I
		ENDIF
               END IF
               I = I + 1
           ENDDO
        END IF

	RETURN
	END


	SUBROUTINE R8MINMAXE(N,R8BUF,EXCLUDE,RMIN,RMAX,IMIN,IMAX)
C  REAL*8 ARRAY
	
	INTEGER*4   N,I,IMIN,IMAX
	REAL*8      R8BUF(N)
	REAL*8      RMIN,RMAX,EXCLUDE
	
C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. R8BUF(I) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           RMIN = R8BUF(1)
           RMAX = R8BUF(1)
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           RMIN = R8BUF(I)
           RMAX = R8BUF(I)
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               IF ( R8BUF(I) .NE. EXCLUDE )  THEN
		IF (RMIN.GT.R8BUF(I))THEN
			RMIN = R8BUF(I)
			IMIN = I
		ENDIF

		IF (RMAX.LT.R8BUF(I)) THEN
			RMAX = R8BUF(I)
			IMAX = I
		ENDIF
               END IF
               I = I + 1
           ENDDO
        END IF


	RETURN
	END


	SUBROUTINE CMINMAXE(N,CBUF, EXCLUDE,RMIN,RMAX,IMIN,IMAX)
C  COMPLEX ARRAY
	
	INTEGER*4   N,I,IMIN,IMAX
	COMPLEX     CBUF(N)
	REAL*4      RMIN,RMAX, EXCLUDE
C==================================================================

	I = 1

C...SEARCH FOR FIRST NON-EXCLUDED VALUE.

        DO WHILE (I .LE. N .AND. CABS(CBUF(I)) .EQ. EXCLUDE)
           I = I+1
        END DO

C...IF NO NON-EXCLUDED VALUE, USE THE FIRST.

        IF (I .GT. N) THEN
           RVAL = CABS( CBUF(1) )
           RMIN = RVAL
           RMAX = RVAL
           IMIN = 1
           IMAX = 1

C...ELSE SEARCH FOR MIN AND MAX STARTING AT FIRST NON-EXCLUDED VALUE.

        ELSE
           RVAL = CABS( CBUF(I) )
           RMIN = RVAL
           RMAX = RVAL
           IMIN = I
           IMAX = I
           I    = I+1

           DO WHILE (I.LE.N)
               RVAL = CABS( CBUF(I) )
               IF ( RVAL .NE. EXCLUDE )  THEN
		IF (RMIN.GT.RVAL)THEN
			RMIN = RVAL
			IMIN = I
		ENDIF

		IF (RMAX.LT.RVAL) THEN
			RMAX = RVAL
			IMAX = I
		ENDIF
               END IF
               I = I + 1
           ENDDO
        END IF

	RETURN
	END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create zminmax.c
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "xvmaininc.h"
#include "ftnbridge.h"

/************************************************************************/
/* C-Callable Version: zminmax - find min and max of array 		*/
/************************************************************************/

void zminmax( dcode, n, buf, min, max, imin, imax) 
int dcode, n, *min, *max, *imin, *imax;
void *buf;

{
FTN_NAME2(minmax, MINMAX) ( &dcode, &n, buf, min, max, imin, imax);
}

/************************************************************************/
/* C-Callable Version: zminmaxe - find min and max of array excluding value*/
/************************************************************************/

void zminmaxe( dcode, n, buf, exclude, min, max, imin, imax) 
int dcode, n, *min, *max, *imin, *imax, *exclude;
void *buf;

{
  FTN_NAME2(minmaxe, MINMAXE) ( &dcode, &n, buf, exclude, min, max, imin, imax);
}
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create minmax.imake
/* Imake file for VICAR subroutine MINMAX */

#define SUBROUTINE minmax

#define MODULE_LIST minmax.f zminmax.c

#define P2_SUBLIB

#define USES_C
#define USES_FORTRAN
#define FTNINC_LIST fortport

$ Return
$!#############################################################################
$Test_File:
$ create tminmax.f
      INCLUDE 'VICMAIN_FOR'
 
      SUBROUTINE MAIN44
C TEST PROGRAM FOR THE SUBROUTINE MINMAX
      BYTE  BUF(25)
      DATA BUF/1,2,3,4,5,6,7,0,1,2,3,-1,34,55,6,78,47,24,
     &      11,22,33,44,55,66,77/     ! -1 IS DN OF 255.
      INTEGER*2  I2BUF(2)
      DATA I2BUF/1,2/
      INTEGER*4  I4BUF(30)
      DATA I4BUF/1,12,123,1234,12345,123456,7,78,789,
     &      10,-11,12,13,14,154,16789,-170,123,1905,20345,21345,
     &      229876,23098,24567,25,.26,270001,2.8,29.678,.30/
      REAL*4     R4BUF(10)
      DATA R4BUF/-1278.0,123.45,345.12,4987.23,5678.12,
     &      -612.11,-.7,8,.91,0/
      REAL*8     R8BUF(5)
      DATA R8BUF/1.23456789,0.0,-12345678,987654,12/
      COMPLEX    CBUF(5) 
      DATA CBUF/ ( -1., -1.), ( 0., 0.), ( 1., 0.), 
     &                     ( 0., 1.),   ( 1., 1.)   /

      INTEGER*4 DCODE
      INTEGER*4  N,MIN,MAX,IMIN,IMAX
      REAL*4	  RMIN,RMAX
      REAL*8	  RMIN8,RMAX8

C TEST FOR BYTE ARRAY
      DCODE = 1
      N = 25

      CALL MINMAX(DCODE,N,BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*2 ARRAY
      DCODE = 2
      N = 2

      CALL MINMAX(DCODE,N,I2BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*4 ARRAY
      DCODE = 4
      N = 25

      CALL MINMAX(DCODE,N,I4BUF,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')
C TEST FOR REAL*4
      DCODE = 7
      N = 10

      CALL MINMAX(DCODE,N,R4BUF,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('********************.',' ')

C TEST FOR REAL*8 ARRAY

      DCODE = 8
      N = 5

      CALL MINMAX(DCODE,N,R8BUF,RMIN8,RMAX8,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(8,1,RMIN8,'RMIN =.')
      CALL PRNT(8,1,RMAX8,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR COMPLEX ARRAY

      DCODE = 10
      N = 5

      CALL MINMAX(DCODE,N,CBUF,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')


      call XVMESSAGE( 'NOW TEST MINMAXE',' ')

C TEST FOR BYTE ARRAY
      DCODE = 1
      N = 25

      CALL MINMAXE(DCODE,N,BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*2 ARRAY
      DCODE = 2
      N = 2

      CALL MINMAXE(DCODE,N,I2BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR INTEGER*4 ARRAY
      DCODE = 4
      N = 25

      CALL MINMAXE(DCODE,N,I4BUF,0,MIN,MAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(4,1,MIN,'MIN =.')
      CALL PRNT(4,1,MAX,'MAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')
C TEST FOR REAL*4
      DCODE = 7
      N = 10

      CALL MINMAXE(DCODE,N,R4BUF,0.,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('********************.',' ')

C TEST FOR REAL*8 ARRAY

      DCODE = 8
      N = 5

      CALL MINMAXE(DCODE,N,R8BUF,0.,RMIN8,RMAX8,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(8,1,RMIN8,'RMIN =.')
      CALL PRNT(8,1,RMAX8,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

C TEST FOR COMPLEX ARRAY

      DCODE = 10
      N = 5

      CALL MINMAXE(DCODE,N,CBUF,0.,RMIN,RMAX,IMIN,IMAX)

      CALL PRNT(4,1,DCODE,'DCODE=.')
      CALL PRNT(7,1,RMIN,'RMIN =.')
      CALL PRNT(7,1,RMAX,'RMAX =.')
      CALL PRNT(4,1,IMAX,'IMAX = .')
      CALL PRNT(4,1,IMIN,'IMIN = .')
      CALL XVMESSAGE('*****************.',' ')

      CALL XVMESSAGE(
     . 'Repeat some cases in C to test C interface: zminmax & zminmaxe',
     .   ' ')

      call tzminmax

      RETURN
      END

$!-----------------------------------------------------------------------------
$ create tzminmax.c
#include "xvmaininc.h"
#include "ftnbridge.h"

void FTN_NAME(tzminmax)() 
{
      int buf[32];

      int i, exclude;
      int dcode,n, min, max, imin, imax;
/*  ==================================================================  */

      for ( i=0; i<10; i++ )
      {
        buf[i] = i;
      }

      dcode = 4;
      n = 10;
      zminmax(dcode,n,buf,&min,&max,&imin,&imax);
       zvmessage("Should get MIN=0, MAX=9, IMIN=1, IMAX=10", "");
       zprnt(4,1, &min," MIN =");
       zprnt(4,1, &max," MAX =");
       zprnt(4,1, &imin," IMIN =");
       zprnt(4,1, &imax," IMAX =");

       zvmessage("***********", "");
       zvmessage("Now exclude 0.", "");
       zvmessage("Should get MIN=1, MAX=9, IMIN=2, IMAX=10", "");

      exclude=0;
      zminmaxe(dcode,n,buf, &exclude, &min,&max,&imin,&imax);
       zprnt(4,1, &min," MIN =");
       zprnt(4,1, &max," MAX =");
       zprnt(4,1, &imin," IMIN =");
       zprnt(4,1, &imax," IMAX =");
}
$!-----------------------------------------------------------------------------
$ create tminmax.imake
/* Imake file for Test of VICAR subroutine MINMAX */

#define PROGRAM tminmax

#define MODULE_LIST tminmax.f tzminmax.c

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN
#define USES_C

#define LIB_RTL
#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create tminmax.pdf
! PDF FOR TMINMAX WHICH TESTS MINMAX 
PROCESS
END-PROC
$!-----------------------------------------------------------------------------
$ create tstminmax.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!TEST FILE FOR TESTING MINMAX.FOR
!CALLS THE TEST PROGRAM TMINMAX
tminmax
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create minmax.hlp
1  MINMAX

2  PURPOSE

      MINMAX is a subroutine which scans an array
      for its algebraic minimum and maximum.
      MINMAXE is a subroutine which scans an array
      for its algebraic minimum and maximum, excluding a specified value.


2  CALLING SEQUENCE

  FORTRAN Calling Sequences:  
      CALL MINMAX(DCODE,N,BUF,MIN,MAX,IMIN,IMAX)
      CALL MINMAXE(DCODE,N,BUF, EXCLUDE, MIN,MAX,IMIN,IMAX)

  C Calling Sequences:        
      zminmax(dcode,n,buf,&min,&max,&imin,&imax);
      zminmaxe(dcode,n,buf, &exclude, &min,&max,&imin,&imax);

2  ARGUMENTS

      DCODE       is a data code which specifies the format of the input
                  array BUF.  (See table.)
      
      N           specifies the number of elements in the array.

      BUF         is the input array.

      EXCLUDE     value to be excluded in search for minimum and 
                  maximum. (MINMAXE ONLY.)

      MIN,MAX     are returned as the minimum and maximum values of BUF.
                  For MINMAXE, MIN and MAX are set to EXCLUDE if all of
                  the values in BUF are equal to EXCLUDE. This return value
		  should be the same type as the input array.  

      IMIN,IMAX   are arguments which are returned as the indices
                  pointing to the minimum and maximum values in buf.  In
                  case of a tie,  IMIN and IMAX will point to the first
                  occurrence of a minima or maxima.

      The arguments DCODE, N, IMIN and IMAX are Integer*4 quantities.  

      If BUF is an integer array (DCODE<=4), then MIN and MAX are returned as 
      INTEGER*4 quantities and EXCLUDE is also INTEGER*4.  
 
      If BUF is a REAL*4 floating point array, then MIN and MAX are returned 
      as REAL*4 quantities and EXCLUDE is also REAL*4.   

      If BUF is a REAL*8 floating point array, then MIN and MAX are returned 
      as REAL*8 quantities and EXCLUDE is also REAL*8. 

      If DCODE=10 (COMPLEX), then MINMAX finds the values of minimum and 
      maximum magnitude (Fortran CABS function) and returns these values 
      as REAL*4 values.  For DCODE=10, EXCLUDE is REAL*4 and it is the \
      magnitudes of the values in BUF that are compared against EXCLUDE.

      For zminmax and zminmaxe, dcode and n are passed by value. exclude is
      passed by address for zminmaxe, and is an int * for dcode <= 4 and is a
      float * for DCODE >4.

      The possible values of the data code are:

			DCODE        ARRAY TYPE
                        -----        ----------

                          1             BYTE (unsigned)
                          2             INTEGER*2
                          4             INTEGER*4
                          7             REAL*4
                          8             REAL*8
                         10             COMPLEX
  
2  HISTORY   
 
  Original Programmer: Gary Yagi, 1 September 1980
  Current Cognizant Programmer: Steve Pohorsky    21 April 1988
  Source Language: Fortran
  Ported to UNIX: Steve Pohorsky

	REVISION HISTORY

   4-88  SP   ADDED ZEXT CALLS TO CORRECT THE BYTE CASE, AND ADDED
	       COMPLEX CASE (DCODE=10).
              ADDED SUBROUTINE MINMAXE WHICH FINDS MIN AND MAX EXCLUDING
              A SPECIFIED VALUE.
  12-88  SP   CORRECTED MINMAXE TO HANDLE CASE WHERE BUFFER BEGINS WITH
              EXCLUDED VALUE.
   1-93  SP   Ported to UNIX; added C bridges; No more optional parameters.
              Changed to use fortport.fin as improved portability method.  
  12-98  BAM  Modified to handle R*8 IMIN,IMAX values.

2 OPERATION

     For fixed point arrays, MINMAX is equivalent to the following FORTRAN:

			MIN = BUF(1)
			MAX = BUF(1)
			IMIN = 1
			IMAX = 1
			IF (N.LT.MIN) RETURN
			DO 10 I = 2,N
          		IF (BUF(I.LT.MIN)) GO TO 5
			IF (BUF(I.LE.MAX)) GO TO 10
			MAX = BUF(I)
			IMAX = I
			GO TO 10
		    5   MIN = BUF(I)
			IMIN = I
		   10   CONTINUE



$ Return
$!#############################################################################
