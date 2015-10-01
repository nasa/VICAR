$!****************************************************************************
$!
$! Build proc for MIPL module realtr
$! VPACK Version 1.9, Monday, December 07, 2009, 16:33:09
$!
$! Execute by entering:		$ @realtr
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
$ write sys$output "*** module realtr ***"
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
$ write sys$output "Invalid argument given to realtr.com file -- ", primary
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
$   if F$SEARCH("realtr.imake") .nes. ""
$   then
$      vimake realtr
$      purge realtr.bld
$   else
$      if F$SEARCH("realtr.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake realtr
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @realtr.bld "STD"
$   else
$      @realtr.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create realtr.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack realtr.com -mixed -
	-s realtr.f -
	-i realtr.imake -
	-t trealtr.f trealtr.imake trealtr.pdf tstrealtr.pdf -
	-o realtr.hlp
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create realtr.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C/*   15 JUNE 77  ..JEK..    INITIAL RELEASE */
      SUBROUTINE REALTR(A,B,N,ISN)
C  IF ISN=1, THIS SUBROUTINE COMPLETES THE FOURIER TRANSFORM
C    OF 2*N REAL DATA VALUES, WHERE THE ORIGINAL DATA VALUES ARE
C    STORED ALTERNATELY IN ARRAYS A AND B, AND ARE FIRST
C    TRANSFORMED BY A COMPLEX FOURIER TRANSFORM OF DIMENSION N.
C    THE COSINE COEFFICIENTS ARE IN A(1),A(2),...A(N+1) AND
C    THE SINE COEFFICIENTS ARE IN B(1),B(2),...B(N+1).
C    A TYPICAL CALLING SEQUENCE IS
C      CALL FFT(A,B,N,N,N,1)
C      CALL REALTR(A,B,N,1)
C    THE RESULTS SHOULD BE MULTIPLIED BY 0.5/N TO GIVE THE
C    USUAL SCALING OF COEFFICIENTS.
C  IF ISN=-1, THE INVERSE TRANSFORMATION IS DONE, THE FIRST STEP
C    IN EVALUATING A REAL FOURIER SERIES.
C    A TYPICAL CALLING SEQUENCE IS
C      CALL REALTR(A,B,N,-1)
C      CALL FFT(A,B,N,N,N,-1)
C    THE RESULTS SHOULD BE MULTIPLIED BY 0.5 TO GIVE THE USUAL
C    SCALING, AND THE TIME DOMAIN RESULTS ALTERNATE IN ARRAYS A
C    AND B, I.E. A(1),B(1),A(2),B(2),...A(N),B(N).
C  THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE COMPLEX
C    ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO TWO TO
C    GIVE THE CORRECT INDEXING INCREMENT AND A(2) USED TO
C    PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF IMAGINARY
C    VALUES, E.G.
C      CALL FFT(A,A(2),N,N,N,2)
C      CALL REALTR(A,A(2),N,2)
C    IN THIS CASE, THE COSINE AND SINE COEFFICIENTS ALTERNATE IN A.
C  BY R. C. SINGLETON, STANFORD RESEARCH INSTITUTE, OCT. 1968
      DIMENSION A(1),B(1)
      REAL IM
      INC=IABS(ISN)
      NK=N*INC+2
      NH=NK/2
      SD=2.0*ATAN(1.0)/FLOAT(N)
      CD=2.0*SIN(SD)**2
      SD=SIN(SD+SD)
      SN=0.0
      IF(ISN .LT. 0) GO TO 30
      CN=1.0
      A(NK-1)=A(1)
      B(NK-1)=B(1)
   10 DO 20 J=1,NH,INC
      K=NK-J
      AA=A(J)+A(K)
      AB=A(J)-A(K)
      BA=B(J)+B(K)
      BB=B(J)-B(K)
      RE=CN*BA+SN*AB
      IM=SN*BA-CN*AB
      B(K)=IM-BB
      B(J)=IM+BB
      A(K)=AA-RE
      A(J)=AA+RE
      AA=CN-(CD*CN+SD*SN)
      SN=(SD*CN-CD*SN)+SN
C  THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
C    ERROR.  IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE
C  20 CN=AA
      CN=0.5/(AA**2+SN**2)+0.5
      SN=CN*SN
   20 CN=CN*AA
      RETURN
   30 CN=-1.0
      SD=-SD
      GO TO 10
      END
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create realtr.imake
/* Imake file for VICAR subroutine REALTR*/

#define SUBROUTINE  realtr

#define MODULE_LIST  realtr.f  

#define P2_SUBLIB

#define USES_FORTRAN
$ Return
$!#############################################################################
$Test_File:
$ create trealtr.f

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
C-----THIS IS A TEST PROGRAM FOR MODULE REALTR
C-----A REAL*4 IMAGE IS INPUT (CREATED WITH GEN.
      DIMENSION A(16,18)

	M = 16
	N = 16
        CALL XVUNIT(IUNIT,'INP',1,STAT, ' ')
        CALL XVOPEN(IUNIT,STAT,' ')
	DO I=1,M
           CALL XVREAD(IUNIT,A(1,I),STAT,'LINE',I,'NSAMPS',N, ' ')
        END DO
	M2 = M/2
      IF ( 2*M2 .NE. M ) GO TO 930
C THAT BECAUSE THE DIMENSION MUST BE EVEN IN THE REAL TRANSFORM
C DIRECTION.
      MP2=M + 2
      N2 = 2*N
C
C FORWARD TRANSFORM..
	DO 110 I=1,M
110	CALL PRNT(7,N,A(1,I), 'BEFORE.')
C
      DO 120 I=1,N
      CALL dfft(A(I,1),A(I,2),M2,M2,M2,N2,&950,&960)
      CALL REALTR(A(I,1),A(I,2),M2,N2)
120   CONTINUE
      DO 140 I=2,MP2,2
      CALL dfft(A(1,I-1),A(1,I),N,N,N,1,&950,&960)
140   CONTINUE
	CALL XVMESSAGE( 'THE RESULTING TRANSFORM', ' ')
	DO 130 I=1,MP2
130	CALL PRNT(7,N,A(1,I), 'TRANSFORM.')
C
C THE INVERSE TRANSFORM..
C
      DO 220 I=2,MP2,2
      CALL dfft(A(1,I-1),A(1,I),N,N,N,-1,&950,&960)
220   CONTINUE
      DO 240 I=1,N
      CALL REALTR(A(I,1),A(I,2),M2,-N2)
      CALL dfft(A(I,1),A(I,2),M2,M2,M2,-N2,&950,&960)
240   CONTINUE
	CALL XVMESSAGE( 'REVERSE THE TRANSFORM', ' ')
	S = 2*M*N
	DO 160 I=1,M
        DO J = 1, N
           A(J,I) = A(J,I)/S
        END DO
160	CALL PRNT(7,N,A(1,I), 'AFTER.')
      CALL XVCLOSE(IUNIT,STAT, ' ')
      RETURN
C
930 	CALL XVMESSAGE( 'NUMBER OF LINES MUST BE EVEN', ' ')
	RETURN
C
950 	CALL XVMESSAGE( 'M OR N HAS TOO LARGE A PRIME FACTOR', ' ')
	RETURN
C
960	CALL XVMESSAGE( 
     +  'PRODUCT OF SQUARE-FREE FACTORS OF M OR N TOO BIG',' ')
	RETURN

      END
C************************* START PDF *************************
CPROCESS
CPARM 	INP	TYPE=STRING
CEND-PROC
C**************************** END PDF ***************************
$!-----------------------------------------------------------------------------
$ create trealtr.imake
/* Imake file for Test of VICAR subroutine realtr */

#define PROGRAM trealtr

#define MODULE_LIST trealtr.f

#define MAIN_LANG_FORTRAN
#define TEST

#define USES_FORTRAN

#define LIB_RTL

#define LIB_TAE
#define LIB_P2SUB 
$!-----------------------------------------------------------------------------
$ create trealtr.pdf
! pdf for test pgm for subroutine realtr
PROCESS
PARM 	INP	TYPE=STRING
END-PROC
$!-----------------------------------------------------------------------------
$ create tstrealtr.pdf
procedure
refgbl $echo
body
let _onfail="continue"
let $echo="yes"
!trealtr
!THIS IS A TEST OF MODULE REALTR
!REALTR  ALLOWS THE USE OF REAL NUMBERS INSTEAD OF COMPLEX ONES
!IN THE PERFORMING OF FFTS (SEE TSTDFFT.PDF).
!THE TEST WILL DO ITS THING ON A 16x16 IMAGE.  
!FIRST, THE INPUT REAL*4 BUFFER IS PRINTED OUT.
!THEN, THE TRANSFORM IS PRINTED.
!LAST, THE TRANSFORM IS REVERSED USING THE SAME ROUTINES
!AND THE RESULT IS PRINTED.  THIS LAST RESULT SHOULD BE
!IDENTICAL WITH THE ORIGINAL INPUT BUFFER.
!
!A COMPARISON RUN DONE ON THE IBM HAS BEEN PLACED IN THE
!FILE OF EACH ROUTINE.
GEN A 16 64 'REAL4
TREALTR A
end-proc
$ Return
$!#############################################################################
$Other_File:
$ create realtr.hlp

REALTR

1	Calling sequence:

      SUBROUTINE REALTR(A,B,N,ISN)

2	Arguments:

IF ISN=1, THIS SUBROUTINE COMPLETES THE FOURIER TRANSFORM
	OF 2*N REAL DATA VALUES, WHERE THE ORIGINAL DATA VALUES ARE
	STORED ALTERNATELY IN ARRAYS A AND B, AND ARE FIRST
	TRANSFORMED BY A COMPLEX FOURIER TRANSFORM OF DIMENSION N.
	THE COSINE COEFFICIENTS ARE IN A(1),A(2),...A(N+1) AND
	THE SINE COEFFICIENTS ARE IN B(1),B(2),...B(N+1).
	A TYPICAL CALLING SEQUENCE IS
	  CALL FFT(A,B,N,N,N,1)
	  CALL REALTR(A,B,N,1)
	THE RESULTS SHOULD BE MULTIPLIED BY 0.5/N TO GIVE THE
	USUAL SCALING OF COEFFICIENTS.

IF ISN=-1, THE INVERSE TRANSFORMATION IS DONE, THE FIRST STEP
	IN EVALUATING A REAL FOURIER SERIES.
	A TYPICAL CALLING SEQUENCE IS
	  CALL REALTR(A,B,N,-1)
	  CALL FFT(A,B,N,N,N,-1)
	THE RESULTS SHOULD BE MULTIPLIED BY 0.5 TO GIVE THE USUAL
	SCALING, AND THE TIME DOMAIN RESULTS ALTERNATE IN ARRAYS A
	AND B, I.E. A(1),B(1),A(2),B(2),...A(N),B(N).

THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE COMPLEX
	ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO TWO TO
	GIVE THE CORRECT INDEXING INCREMENT AND A(2) USED TO
	PASS THE INITIAL ADDRESS FOR THE SEQUENCE OF IMAGINARY
	VALUES, E.G.
	  CALL FFT(A,A(2),N,N,N,2)
	  CALL REALTR(A,A(2),N,2)
	IN THIS CASE, THE COSINE AND SINE COEFFICIENTS ALTERNATE IN A.

3	History:

ALGORITHM BY R. C. SINGLETON, STANFORD RESEARCH INSTITUTE, OCT. 1968

15 JUNE 77  ..JEK..    INITIAL RELEASE
$ Return
$!#############################################################################
