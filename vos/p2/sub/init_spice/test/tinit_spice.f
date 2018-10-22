      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      IMPLICIT NONE

      CHARACTER*132 KDB, SPICEKER, MIPSKER, SCLK, CONSTS
      CHARACTER*132 BODYIDS, LEAPSEC
      CHARACTER*256 MSG
      INTEGER*4 STATUS

      KDB = ' '
      SPICEKER = ' '
      MIPSKER = ' '
      SCLK = ' '
      CONSTS = ' '
      BODYIDS = ' '
      LEAPSEC = ' '
      MSG = '  '

      CALL GLL_SPICE_ENV (KDB, SPICEKER, MIPSKER, SCLK, CONSTS,
     &                    BODYIDS, LEAPSEC, STATUS)

      IF (STATUS .LT. 0) 
     &   CALL MABEND('Undefine GLL SPICE environment variables.')

      CALL XVMESSAGE ('**** Testing FORTRAN Interface ****', ' ')

10    FORMAT (A19,A132) 

      WRITE (MSG,10) 'GLL_KDB ---------->', KDB
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_SPICEKER ----->', SPICEKER
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_MIPSKER ------>', MIPSKER
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_SCLK --------->', SCLK
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_CONSTANTS ---->', CONSTS
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_BODY_IDS ----->', BODYIDS
      CALL XVMESSAGE (MSG, ' ')

      MSG = '  '
      WRITE (MSG,10) 'GLL_LEAPSECONDS -->', LEAPSEC
      CALL XVMESSAGE (MSG, ' ')
      CALL XVMESSAGE (' ', ' ')
      CALL XVMESSAGE ('Calling INIT_SPICE', ' ')
      CALL INIT_SPICE ()
      CALL XVMESSAGE ('Return from INIT_SPICE', ' ')

      CALL XVMESSAGE (' ', ' ')
      CALL XVMESSAGE (' ', ' ')

      CALL TZINIT_SPICE ()

      RETURN
      END


