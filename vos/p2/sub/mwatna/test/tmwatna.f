
C  THIS PROGRAM TEST THE FORTRAN CALLING SEQUENCE FOR MWATNA
      INCLUDE 'VICMAIN_FOR'
      SUBROUTINE MAIN44
      INTEGER*2 ICAM
      CHARACTER*50 MSG
      REAL*4 WALINE,WASAMP,NALINE,NASAMP
   
C
      DO ICAM=4,8,2
          WRITE(MSG,100)ICAM
          CALL XVMESSAGE(MSG,' ')
  100     FORMAT('ICAM= ',I)
          WRITE(MSG,120)
          CALL XVMESSAGE(MSG,' ')
  120     FORMAT(' WALINE,WASAMP,NALINE,NASAMP')
          WALINE = 500.0	 ! See where the optical axis goes
          WASAMP = 500.0         ! just for intellectual curiosity.
          CALL MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,&999)
          WRITE(MSG,140) WALINE,WASAMP,NALINE,NASAMP
          CALL XVMESSAGE(MSG,' ')
  140     FORMAT(4E12.4)
          WALINE = 1.0           ! Check one of the corners
          WASAMP = 1.0
          CALL MWATNA(ICAM,WALINE,WASAMP,NALINE,NASAMP,&999)
          WRITE(MSG,140) WALINE,WASAMP,NALINE,NASAMP
          CALL XVMESSAGE(MSG,' ')
      ENDDO
      RETURN
  999 WRITE (MSG,160)
      CALL XVMESSAGE(MSG,' ')
  160 FORMAT(' ***Invalid camera serial number')
      RETURN
      END
