C$NAIF routine RESET ( Reset Error Status )
c
C Renamed RESET1 and comment out ACCEPT to avoid naming conflict.
c 
C      Reset the SPICELIB error status to a value of
C      "no error."   As a result, the status routine, FAILED,
C      will return a value of .FALSE.
C 
      SUBROUTINE RESET1
      LOGICAL               SETERR
c      LOGICAL               ACCEPT
 
      LOGICAL               STAT

      STAT = SETERR ( .FALSE. )
C           Wipe out the short and long error messages:
      CALL PUTSMS ( ' ' )
      CALL PUTLMS ( ' ' )
C           Allow long error message to be updated:
c      STAT = ACCEPT ( .TRUE. )
C           Reset the frozen traceback:
      CALL FREEZE
      RETURN
      END
