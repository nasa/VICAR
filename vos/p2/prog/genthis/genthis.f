C#######################################################################
C  NAME OF ROUTINE
C      GENTHIS
C
C  PURPOSE
C      THIS IS THE STANDARD MAIN PROGRAM USED FOR TAE/VICAR PROGRAMS.
C      THIS MODULE CALLS SUBROUTINE MAIN44 TO ENTER INTO THE BODY OF THE
C      PROGRAM.
C      GENTHIS generates small exactly-defined test files.
C  PREPARED FOR USE ON MIPL SYSTEM BY
C      STEVE POHORSKY   INFORMATICS GENERAL CORPORATION    APRIL 1986
C  FOR
C      MIPL SOFTWARE DEVELOPMENT
C
C  DERIVED FROM CODE FOR VICAR PROGRAM GEN
C
C  ENVIRONMENT
C      VAX 11/780    VMS  with TAE/VICAR2 EXECUTIVE       FORTRAN-77
C     
C  REVISION HISTORY
C       14 April 1993   NDR: Ported to UNIX
C
C  SUBROUTINES CALLED
C      MAIN44
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'VICMAIN_FOR'

      SUBROUTINE MAIN44

C           GENTHIS A NL NS PARAMS

C     GENTHIS DOES NOT DISTINGUISH BETWEEN INTEGER AND REAL 
C     NUMBER DN VALUES. ALL VALUES ARE NOW PROCESSED AS REAL NUMBERS. 

      CALL XVMESSAGE(' GENTHIS VERSION 2',' ')
      CALL GENTHISLAB
      CALL GENTHIS1

      RETURN
      END


      SUBROUTINE GENTHISLAB

      IMPLICIT NONE

      INTEGER DEF,CNT,STATUS
      INTEGER*4  MAXC
      CHARACTER*5  DATA

      REAL*4 BUF(1000)
      INTEGER*4 NL,NS,OUTUNIT
      COMMON /UNIT/ OUTUNIT
      COMMON /C1/ NL,NS,BUF
C

C          Label processor
      CALL XVUNIT(OUTUNIT,'OUT',1,STATUS,' ')
      if (status .ne. 1) then
	 call xvmessage(' *** XVUNIT:Invalid unit number ***',' ')
	 call abend
      end if

C--- Determine data type
      call xvp('FORMAT',data,cnt)
      IF (data(1:5) .eq. 'REAL4') THEN
	data = 'REAL'
      ELSE IF (data(1:5) .eq. 'REAL8') THEN
        data = 'DOUB'
      ENDIF

C--- Open output file with specified values
      call xvparm('NL',nl,cnt,def,maxc)
      call xvparm('NS',ns,cnt,def,maxc)
      CALL XVOPEN(OUTUNIT,STATUS,'U_FORMAT','REAL','O_FORMAT',
     +            DATA,'OP','WRITE','U_NL',nl,'U_NS',ns,
     +		  'IO_ACT','SA','OPEN_ACT','SA',' ')

C--- Get DN VALUES.
      CALL XVP('DN', BUF, CNT)
      IF (CNT .NE. NL*NS) THEN
         CALL XVMESSAGE(
     +    'PARAMETER ERR...DN COUNT DOES NOT MATCH SIZE',' ')
         CALL XVMESSAGE('**GENTHIS TASK CANCELLED',' ')
         CALL ABEND
      END IF

      RETURN
      END
C
C
      SUBROUTINE GENTHIS1

      IMPLICIT NONE

      INTEGER STATUS, I,N
 
      REAL*4 BUF(1000)
      INTEGER*4 NL,NS,OUTUNIT
      COMMON /UNIT/ OUTUNIT
      COMMON /C1/ NL,NS,BUF

        N = 1
	DO I = 1, NL
	  CALL XVWRIT(OUTUNIT,BUF(N),STATUS,' ')
          N = N + NS
	END DO

C--- Close output file
      CALL XVCLOSE(OUTUNIT,STATUS,' ')
      CALL XVMESSAGE(' GENTHIS TASK COMPLETED',' ')
      RETURN
      END
