C Load the CONSTANTS, SCLK, and LEAPSECONDS kernels.
C The logical names CONSTANTS, SCLK, and LEAPSECONDS must be defined.
C
      SUBROUTINE INIT_SPICE
      IMPLICIT NONE

      CHARACTER*132 KDB, SPICEKER, MIPSKER, SCLK, CONSTS
      CHARACTER*132 BODYIDS, LEAPSEC

      INTEGER*4 STATUS
      LOGICAL FAILED

      KDB = ' '
      SPICEKER = ' '
      MIPSKER = ' '
      SCLK = ' '
      CONSTS = ' '
      BODYIDS = ' '
      LEAPSEC = ' '

      CALL GLL_SPICE_ENV (KDB, SPICEKER, MIPSKER, SCLK, CONSTS,
     &                    BODYIDS, LEAPSEC, STATUS)

      IF (STATUS .LT. 0) 
     &   CALL MABEND('Undefine GLL SPICE environment variables.')

      CALL ERRPRT('SET','NONE')
      CALL ERRACT('SET','RETURN')

      CALL CLPOOL

      CALL LDPOOL(CONSTS)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading CONSTANTS kernel',' ')

      CALL LDPOOL(SCLK)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading SCLK kernel',' ')

      CALL LDPOOL(LEAPSEC)
      IF (FAILED()) CALL XVMESSAGE
     &		('***Error loading LEAPSECONDS kernel',' ')
      RETURN
      END
