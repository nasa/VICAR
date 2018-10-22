      SUBROUTINE JDAY(M,D,Y,DOY)
      IMPLICIT NONE

C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
C  
C  Routine to Calculate Day-of-Year from the given Month, Day, and Year 
C  information	
C
C  Fortran Usage:    Call  JDAY(M, D, Y, DOY)
C
C  Parameters:-
C
C       M:  Month           Input,   Integer*4
C       D:  Day of Month    Input,   Integer*4
C       Y:  Year            Input,   Integer*4
C     DOY:  Day of Year     Output,  Integer*4
C
C   History
C
C   07-28-92....WPL....Initial Release under MSTP (Ported for UNIX conversion) 
C
C   07-08-1998 .TXH.   Modified date checking statements to use
C                      'date_check' subroutine calls for correct
C                      calculation of leap year.
C
C* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

       INTEGER*4  M,D,Y,DOY

       INTEGER*4  LEAP,STATUS  ! flags to indicate leap year and status.

       Integer*4  BDOY(12)     ! Beginning Day of Year for the 12 months

C    Define beginnign day for each month (Regular, NON-Leap Year)

        BDOY(1) =   1   ! Beginning of January   is Day    1 of Year
        BDOY(2) =  32   !              Februrary          32
        BDOY(3) =  60   !              March              60 
        BDOY(4) =  91   !              April              91
        BDOY(5) = 121   !              May               121
        BDOY(6) = 152   !              June              152
        BDOY(7) = 182   !              July              182
        BDOY(8) = 213   !              August            213
        BDOY(9) = 244   !              September         244
        BDOY(10)= 274   !              October           274
        BDOY(11)= 305   !              November          305
        BDOY(12)= 335   !              December          335

        LEAP = 0    ! initialize leap year flag
        STATUS = 0  ! initialize status flag

C Check some default inputs (original implementation)
C
C        If (M .LT. 1 .or.  M .GT. 12)  then
C          Call Xvmessage(' Input value of MONTH out of bound', ' ')
C          Call Abend
C        Endif 
C        If (D .LT. 1  .or. D. GT. 31)  then
C          Call Xvmessage(' Input value of DAY out of bound', ' ')
C          Call Abend
C        Endif

C Check some default inputs (new implementation)
C Using the 'date_check' subroutines
        CALL CHK_DAY (Y,M,D,STATUS)
        IF (STATUS .EQ. 0) THEN
           CALL XVMESSAGE ('Invalid date input.',' ')
           CALL ABEND
        END IF

C    Calculate DOY  for NON-Leap year !
        DOY = BDOY(M) + D - 1

C Determine if the year is a leap year using 'date_check' subroutine
        CALL CHK_LEAP (Y, LEAP)
 
C Add 1 day for LEAP Year if Month passes Februrary
        IF ((LEAP .EQ. 1) .AND. (M .GT. 2)) THEN
            DOY = DOY + 1
        ENDIF
 
        RETURN
        END

